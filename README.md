# barker

A hackable swaybar generator, configured in Common Lisp.

barker speaks the swaybar protocol. You describe the bar as a list of
blocks in a Lisp file; each block updates on a timer, a Unix signal, or
an event source, and can react to clicks. A Common Lisp analogue of
[gubar](https://codeberg.org/trevarj/gubar). SBCL only.

## Installation

### Guix

barker is packaged in [guixos](https://github.com/logoraz/guixos) as
`barker` (`guixos packages barker`).

### From source

Dependencies: SBCL, bordeaux-threads, jzon, trivial-gray-streams.
Install via
`ocicl install git+https://codeberg.org/logoraz/barker` into your local
project `ocicl/` directory, then:

```lisp
(asdf:load-system :barker)
```

To build a standalone executable (needs ASDF to find the system):

```
sbcl --eval '(asdf:make :barker)' --quit
```

The block commands are looked up on `PATH` at runtime: `nmcli`,
`brightnessctl` and `pactl`.

## Usage

### Integrating with swaybar

Point the bar's status command at barker:

```
bar {
    position top
    status_command barker
}
```

### Configuration

barker loads `$XDG_CONFIG_HOME/barker/config.lisp`, falling back to
`~/.config/barker/config.lisp`. The file is plain Lisp, evaluated in the
`barker` package, so `defblock` is available with no imports. Blocks
appear in the bar in the order they are defined.

If there is no config, a single `barker` block is shown. If the config
fails to load, a red `config-error` block shows the error. Anything the
config prints goes to stderr, never to the bar.

### Example config

```lisp
(defblock guixos :kind label :text "GuixOS" :color "#81a1c1")
(defblock network-manager :ssid t)
(defblock brightness :nerd-icons t)
(defblock volume)
(defblock battery :nerd-icons t)
(defblock clock :format "%a %b %d %Y %-I:%M:%S %p")
```

### Built-in blocks

A block named after a kind uses that kind. Use `:kind` to name a block
differently, or to repeat a kind.

| Block             | Options                                    | Signal |
|-------------------|--------------------------------------------|--------|
| `label`           | `:text`, `:color`                          |        |
| `clock`           | `:format` (strftime, default `%c`)         |        |
| `battery`         | `:format`, `:nerd-icons`, `:bat`, `:ac`    |        |
| `brightness`      | `:format`, `:nerd-icons`                   | 4      |
| `volume`          | `:pactl-command`                           | 2      |
| `xkb-layout`      | `:alias-pairs`                             | 3      |
| `network-manager` | `:ssid`                                    |        |

Clicking a `label` toggles its urgent flag. `clock` ticks on the second
boundary, driven by an event source rather than a timer.

### Defining blocks

`defblock` takes these keys; anything else is passed to the block kind.

| Key              | Meaning                                         |
|------------------|-------------------------------------------------|
| `:kind`          | Kind to build from (default: the block name)    |
| `:body`          | Plist of swaybar fields, e.g. `:background`     |
| `:interval`      | Seconds between updates                         |
| `:trigger`       | N; update on `SIGRTMIN+N`                       |
| `:procedure`     | Function of the body; returns the new body      |
| `:click-handler` | Function of event and body; returns the body    |
| `:event-source`  | Stream of lines; each line triggers an update   |

Config values override a kind's defaults. A counter that ticks every
second:

```lisp
(defvar *count* 0)

(defblock counter
  :interval 1
  :body (:full-text "0")
  :procedure (lambda (body)
               (setf (swblock-full-text body)
                     (format nil "~d" (incf *count*)))
               body))
```

Define your own reusable kind with `defkind`; its body returns a plist
of block keys plus a `:body` plist:

```lisp
(defkind greeting (&key (name "world"))
  "A static greeting."
  (list :body (list :full-text (format nil "hello ~a" name))))

(defblock greeting :name "Erik")
```

### Corresponding sway config

Blocks with a signal refresh when sway sends `SIGRTMIN+N`:

```
bindsym --release XF86AudioRaiseVolume exec pkill -SIGRTMIN+2 -n barker
bindsym --release XF86AudioLowerVolume exec pkill -SIGRTMIN+2 -n barker
bindsym --release XF86AudioMute exec pkill -SIGRTMIN+2 -n barker
bindsym --release XF86MonBrightnessUp exec pkill -SIGRTMIN+4 -n barker
bindsym --release XF86MonBrightnessDown exec pkill -SIGRTMIN+4 -n barker
```

## Testing

```lisp
(asdf:test-system :barker)
```

## License

```lisp
(defmacro license-terms (system . plist)
  "See LICENSE for the actual legally-binding, non-parenthesized version."
  (declare (optimize (safety 0))) ; use at your own risk
  `(list :system ',system ,@plist))

(license-terms barker
  :type        '(:|LGPL-2.1-only WITH LLGPL| . "https://spdx.org/licenses/LLGPL.html")
  :permissions '(:use :copy :modify :distribute :link)
  :conditions  '(:include-copyright-notice
                 :disclose-source-lib
                 :same-license-lib
                 :state-changes
                 :lisp-linking)
  :warranty    nil)
```

↳ [LICENSE](LICENSE)
