;;;; SPDX-License-Identifier: LGPL-2.1-only WITH LLGPL
;;;; Copyright (C) 2026 Erik P Almaraz
;;;;
;;;; Licensed under the LGPL-2.1-only with the LLGPL preamble;
;;;; see the LICENSE file in this repository.

(defpackage #:barker/tests/blocks
  (:use #:cl #:5am)
  (:import-from #:barker/tests/suite
                #:suite)
  (:local-nicknames (#:sp #:barker/base/swaybar-protocol)
                    (#:bk #:barker/base/bkblock)
                    (#:sup #:barker/blocks/support)
                    (#:bat #:barker/blocks/battery)
                    (#:nm #:barker/blocks/network-manager))
  (:export #:blocks-suite)
  (:documentation "Tests for the core blocks."))

(in-package #:barker/tests/blocks)


(def-suite blocks-suite
  :description "Tests for code/blocks."
  :in suite)

(in-suite blocks-suite)


;;; Fixtures

(defun write-file (path text)
  "Write TEXT to the file at PATH, replacing any previous content."
  (with-open-file (out path :direction :output :if-exists :supersede)
    (write-string text out)))

(defun call-with-temp-dir (thunk)
  "Call THUNK with the path of a fresh temporary directory, then delete it."
  (let ((dir (sb-posix:mkdtemp "/tmp/barker-test-XXXXXX")))
    (unwind-protect (funcall thunk dir)
      (sb-ext:run-program "rm" (list "-rf" dir) :search t))))

(defun call-with-commands (commands thunk &key isolate)
  "Call THUNK with fake programs first on the PATH.
COMMANDS is an alist of (NAME . SCRIPT), where SCRIPT is the body of
a sh script. With ISOLATE, the fake programs are the only ones."
  (call-with-temp-dir
   (lambda (dir)
     (dolist (command commands)
       (let ((file (format nil "~a/~a" dir (car command))))
         (write-file file (format nil "#!/bin/sh~%~a~%" (cdr command)))
         (sb-posix:chmod file #o755)))
     (let ((old (sb-posix:getenv "PATH")))
       (unwind-protect
            (progn
              (sb-posix:setenv "PATH"
                               (if isolate dir (format nil "~a:~a" dir old))
                               1)
              (funcall thunk))
         (sb-posix:setenv "PATH" old 1))))))

(defun call-with-power-supplies (supplies thunk)
  "Call THUNK with a fake sysfs power supply directory.
SUPPLIES is a list of (NAME CAPACITY ONLINE); a NIL value skips that file."
  (call-with-temp-dir
   (lambda (root)
     (ensure-directories-exist (format nil "~a/sys/" root))
     (dolist (supply supplies)
       (destructuring-bind (name capacity online) supply
         (let ((dir (format nil "~a/real/~a" root name)))
           (ensure-directories-exist (format nil "~a/" dir))
           (when capacity
             (write-file (format nil "~a/capacity" dir)
                         (format nil "~a~%" capacity)))
           (when online
             (write-file (format nil "~a/online" dir)
                         (format nil "~a~%" online)))
           (sb-posix:symlink dir (format nil "~a/sys/~a" root name)))))
     (let ((bat::*power-supply-path* (format nil "~a/sys/" root)))
       (funcall thunk)))))

(defun print-lines (lines)
  "Return a sh command that prints each of LINES on its own line."
  (if lines
      (format nil "printf '%s\\n'~{ '~a'~}" lines)
      "true"))

(defmacro with-blocks (&body body)
  "Run BODY with an empty block registry."
  `(let ((bk:*blocks* nil))
     ,@body))

(defun registered (name)
  "Return the bkblock registered under NAME."
  (cdr (assoc name bk:*blocks*)))

(defun refresh (name)
  "Run the procedure of the block registered under NAME once."
  (bk:do-procedure (registered name) (lambda ())))

(defun block-text (name)
  "Return the text shown by the block registered under NAME."
  (sp:swblock-full-text (bk:bkblock-body (registered name))))

(defun icon (code)
  "Return the one-character string for the code point CODE."
  (string (code-char code)))


;;; Support

(test glyphs-make-strings
      "glyphs turns code points into one-character strings."
      (let ((icons (sup:glyphs #xF007A #xF007B)))
        (is (= 2 (length icons)))
        (is (string= (icon #xF007B) (aref icons 1)))))

(test command-output-returns-stdout
      "command-output returns what the program prints."
      (is (string= (format nil "hi~%") (sup:command-output "echo" "hi"))))

(test command-output-nonzero-is-nil
      "command-output returns NIL when the program fails."
      (is-false (sup:command-output "false")))

(test command-output-missing-program-errors
      "command-output signals an error for a program that does not exist."
      (signals error (sup:command-output "barker-no-such-program")))

(test read-file-line-returns-first-line
      "read-file-line returns the first line of a file."
      (call-with-temp-dir
       (lambda (dir)
         (let ((file (format nil "~a/data" dir)))
           (write-file file (format nil "one~%two~%"))
           (is (string= "one" (sup:read-file-line file)))))))

(test strftime-formats-the-time
      "strftime formats the current time with the C library."
      (is (string= "x" (sup:strftime "x")))
      (is (= 5 (length (sup:strftime "%H:%M"))))
      (is (char= #\: (char (sup:strftime "%H:%M") 2))))

(test guarded-passes-result-through
      "A guarded procedure returns what the procedure returns."
      (let ((body (make-instance 'sp:swblock)))
        (is (eq body (funcall (sup:guarded #'identity) body)))))

(test guarded-shows-fallback-on-error
      "A guarded procedure shows the fallback text when it fails."
      (let ((body (make-instance 'sp:swblock)))
        (funcall (sup:guarded (lambda (b) (declare (ignore b)) (error "x")))
                 body)
        (is (string= "N/A" (sp:swblock-full-text body)))
        (funcall (sup:guarded (lambda (b) (declare (ignore b)) (error "x"))
                              "?")
                 body)
        (is (string= "?" (sp:swblock-full-text body)))))


;;; label

(test label-shows-text-and-color
      "label takes its text and color from defblock."
      (with-blocks
        (bk:defblock guixos :kind label :text "GuixOS" :color "#81a1c1")
        (is (string= "GuixOS" (block-text 'guixos)))
        (let ((body (bk:bkblock-body (registered 'guixos))))
          (is (string= "#81a1c1" (sp:swblock-color body)))
          (is-false (sp:swblock-urgent body)))))

(test label-without-color-leaves-color-unset
      "A label given no color does not set the color slot."
      (with-blocks
        (bk:defblock sep :kind label :text "|")
        (is-false (slot-boundp (bk:bkblock-body (registered 'sep))
                               'sp::color))))

(test label-click-toggles-urgent
      "Each click flips the label's urgent flag."
      (with-blocks
        (bk:defblock guixos :kind label :text "GuixOS")
        (let ((body-of (lambda () (bk:bkblock-body (registered 'guixos)))))
          (bk:bkblock-handle-click (registered 'guixos) nil (lambda ()))
          (is-true (sp:swblock-urgent (funcall body-of)))
          (bk:bkblock-handle-click (registered 'guixos) nil (lambda ()))
          (is-false (sp:swblock-urgent (funcall body-of))))))


;;; clock

(test clock-shows-formatted-time
      "clock shows the time in the format given to defblock."
      (with-blocks
        (bk:defblock clock :format "%H:%M")
        (refresh 'clock)
        (is (= 5 (length (block-text 'clock))))))

(test clock-is-event-driven
      "clock updates from an event source, not an interval or a signal."
      (with-blocks
        (bk:defblock clock)
        (is (eq :persistent (bk:bkblock-interval (registered 'clock))))
        (is-false (bk:bkblock-trigger (registered 'clock)))
        (is (functionp (bk:bkblock-event-source (registered 'clock))))))

(test clock-event-source-yields-a-line-each-second
      "The clock's event source yields an empty line within a second."
      (with-blocks
        (bk:defblock clock)
        (let ((stream (funcall (bk:bkblock-event-source (registered 'clock))))
              (start (get-internal-real-time)))
          (is (string= "" (read-line stream)))
          (is (< (/ (- (get-internal-real-time) start)
                    internal-time-units-per-second)
                 3/2)))))


;;; battery

(test find-power-supply-by-prefix
      "find-power-supply finds a supply whose name contains the prefix."
      (call-with-power-supplies '(("ACAD" nil "0") ("BAT1" "55" nil))
        (lambda ()
          (is (search "/sys/BAT1" (bat::find-power-supply "BAT" 0)))
          (is (search "/sys/ACAD" (bat::find-power-supply "AC"))))))

(test find-power-supply-default
      "find-power-supply falls back to the prefix plus the default."
      (call-with-power-supplies '()
        (lambda ()
          (is (search "/sys/BAT0" (bat::find-power-supply "BAT" 0))))))

(test battery-shows-level
      "A discharging battery shows its level with a text label."
      (with-blocks
        (call-with-power-supplies '(("ACAD" nil "0") ("BAT1" "55" nil))
          (lambda ()
            (bk:defblock battery)
            (refresh 'battery)
            (is (string= "BAT: 55%" (block-text 'battery)))
            (is (= 3 (bk:bkblock-interval (registered 'battery))))
            (is-false (sp:swblock-urgent
                       (bk:bkblock-body (registered 'battery))))))))

(test battery-nerd-icons
      "With nerd-icons the label is the icon for the level."
      (with-blocks
        (call-with-power-supplies '(("ACAD" nil "0") ("BAT1" "55" nil))
          (lambda ()
            (bk:defblock battery :nerd-icons t)
            (refresh 'battery)
            (is (string= (format nil "~a 55%" (icon #xF007F))
                         (block-text 'battery)))))))

(test battery-full-does-not-overflow
      "A full battery still finds an icon."
      (with-blocks
        (call-with-power-supplies '(("ACAD" nil "0") ("BAT1" "100" nil))
          (lambda ()
            (bk:defblock battery :nerd-icons t)
            (refresh 'battery)
            (is (string= (format nil "~a 100%" (icon #xF0079))
                         (block-text 'battery)))))))

(test battery-low-is-urgent
      "A level of 10 percent or less is urgent."
      (with-blocks
        (call-with-power-supplies '(("ACAD" nil "0") ("BAT1" "8" nil))
          (lambda ()
            (bk:defblock battery)
            (refresh 'battery)
            (is-true (sp:swblock-urgent
                      (bk:bkblock-body (registered 'battery))))))))

(test battery-charging
      "A battery on AC power shows the charging label."
      (with-blocks
        (call-with-power-supplies '(("ACAD" nil "1") ("BAT1" "55" nil))
          (lambda ()
            (bk:defblock battery)
            (refresh 'battery)
            (is (string= "CHRG: 55%" (block-text 'battery)))))))

(test battery-without-ac-is-not-charging
      "A missing AC supply counts as not charging."
      (with-blocks
        (call-with-power-supplies '(("BAT1" "55" nil))
          (lambda ()
            (bk:defblock battery)
            (refresh 'battery)
            (is (string= "BAT: 55%" (block-text 'battery)))))))

(test battery-custom-format
      "The format option controls the text."
      (with-blocks
        (call-with-power-supplies '(("ACAD" nil "0") ("BAT1" "55" nil))
          (lambda ()
            (bk:defblock battery :format "~a|~a")
            (refresh 'battery)
            (is (string= "BAT:|55" (block-text 'battery)))))))

(test battery-unreadable-shows-na
      "A battery that cannot be read shows N/A."
      (with-blocks
        (call-with-power-supplies '()
          (lambda ()
            (bk:defblock battery)
            (refresh 'battery)
            (is (string= "N/A" (block-text 'battery)))))))


;;; brightness

(defparameter *brightnessctl*
  "case \"$1\" in get) echo 96000;; max) echo 192000;; esac"
  "A fake brightnessctl at 50 percent.")

(test brightness-shows-percent
      "brightness shows the backlight percent with a text label."
      (with-blocks
        (call-with-commands (list (cons "brightnessctl" *brightnessctl*))
          (lambda ()
            (bk:defblock brightness)
            (refresh 'brightness)
            (is (string= "BRT: 50%" (block-text 'brightness)))))))

(test brightness-nerd-icons
      "With nerd-icons the label is the icon for the level."
      (with-blocks
        (call-with-commands (list (cons "brightnessctl" *brightnessctl*))
          (lambda ()
            (bk:defblock brightness :nerd-icons t)
            (refresh 'brightness)
            (is (string= (format nil "~a 50%" (icon #xF00DD))
                         (block-text 'brightness)))))))

(test brightness-trigger
      "brightness updates on signal 4 unless told otherwise."
      (with-blocks
        (bk:defblock brightness)
        (bk:defblock other :kind brightness :trigger 5)
        (is (= 4 (bk:bkblock-trigger (registered 'brightness))))
        (is (= 5 (bk:bkblock-trigger (registered 'other))))))

(test brightness-missing-command-shows-na
      "Without brightnessctl the block shows N/A."
      (with-blocks
        (call-with-commands '()
          (lambda ()
            (bk:defblock brightness)
            (refresh 'brightness)
            (is (string= "N/A" (block-text 'brightness))))
          :isolate t)))


;;; volume

(defun pactl (percent &key (mute "false") (default "speaker"))
  "Return a fake pactl script with a default sink at PERCENT."
  (format nil "if [ \"$1\" = get-default-sink ]; then echo ~a; exit 0; fi~%~
cat <<'JSON'~%~
[{\"name\":\"other\",\"mute\":false,~
\"volume\":{\"front-left\":{\"value_percent\":\"99%\"}}},~
{\"name\":\"speaker\",\"mute\":~a,~
\"volume\":{\"front-left\":{\"value_percent\":\"~a%\"}}}]~%JSON"
          default mute percent))

(test volume-shows-default-sink
      "volume shows the icon and percent of the default sink."
      (with-blocks
        (call-with-commands (list (cons "pactl" (pactl 45)))
          (lambda ()
            (bk:defblock volume)
            (refresh 'volume)
            (is (string= (format nil "~a 45" (icon #xF0580))
                         (block-text 'volume)))
            (is (= 2 (bk:bkblock-trigger (registered 'volume))))))))

(test volume-muted
      "A muted sink shows the muted icon."
      (with-blocks
        (call-with-commands (list (cons "pactl" (pactl 45 :mute "true")))
          (lambda ()
            (bk:defblock volume)
            (refresh 'volume)
            (is (string= (format nil "~a 45" (icon #xF0581))
                         (block-text 'volume)))))))

(test volume-loud-clamps-icon
      "Volume above the icon range uses the last icon."
      (with-blocks
        (call-with-commands (list (cons "pactl" (pactl 100)))
          (lambda ()
            (bk:defblock volume)
            (refresh 'volume)
            (is (string= (format nil "~a 100" (icon #xF057E))
                         (block-text 'volume)))))))

(test volume-unknown-default-sink-shows-na
      "When no sink matches the default sink, volume shows N/A."
      (with-blocks
        (call-with-commands (list (cons "pactl" (pactl 45 :default "gone")))
          (lambda ()
            (bk:defblock volume)
            (refresh 'volume)
            (is (string= "N/A" (block-text 'volume)))))))

(test volume-missing-command-shows-na
      "A pactl command that does not exist shows N/A."
      (with-blocks
        (bk:defblock volume :pactl-command "barker-no-such-pactl")
        (refresh 'volume)
        (is (string= "N/A" (block-text 'volume)))))


;;; xkb-layout

(defparameter *swaymsg*
  "cat <<'JSON'
[{\"name\":\"Touchpad\",\"type\":\"touchpad\"},
 {\"name\":\"Keyboard\",\"type\":\"keyboard\",
  \"xkb_active_layout_name\":\"Arabic\",\"xkb_active_layout_index\":1}]
JSON"
  "A fake swaymsg listing a touchpad and a keyboard on layout 1.")

(test xkb-layout-shows-active-layout
      "xkb-layout shows the active layout of the first keyboard."
      (with-blocks
        (call-with-commands (list (cons "swaymsg" *swaymsg*))
          (lambda ()
            (bk:defblock xkb-layout)
            (refresh 'xkb-layout)
            (is (string= "Arabic" (block-text 'xkb-layout)))
            (is (= 3 (bk:bkblock-trigger (registered 'xkb-layout))))))))

(test xkb-layout-alias-by-name
      "An alias for the layout name replaces the name."
      (with-blocks
        (call-with-commands (list (cons "swaymsg" *swaymsg*))
          (lambda ()
            (bk:defblock xkb-layout :alias-pairs '(("Arabic" . "AR")))
            (refresh 'xkb-layout)
            (is (string= "AR" (block-text 'xkb-layout)))))))

(test xkb-layout-alias-by-index
      "An alias for the layout index replaces the name."
      (with-blocks
        (call-with-commands (list (cons "swaymsg" *swaymsg*))
          (lambda ()
            (bk:defblock xkb-layout :alias-pairs '((1 . "ar")))
            (refresh 'xkb-layout)
            (is (string= "ar" (block-text 'xkb-layout)))))))


;;; network-manager

(defun nmcli (devices wifi)
  "Return a fake nmcli script printing the lines DEVICES and WIFI."
  (format nil "[ \"$1\" = monitor ] && { echo 'eth0: connected'; exit 0; }~%~
case \"$*\" in~%~
*TYPE,STATE*) ~a;;~%~
*SSID*) ~a;;~%~
esac"
          (print-lines devices) (print-lines wifi)))

(test split-terse-splits-on-colons
      "split-terse splits nmcli terse output on colons."
      (is (equal '("a" "*" "78") (nm::split-terse "a:*:78"))))

(test split-terse-honors-escapes
      "An escaped colon stays inside its field."
      (is (equal '("My:Net" "*" "78") (nm::split-terse "My\\:Net:*:78"))))

(test split-terse-keeps-empty-fields
      "Empty fields are kept so positions stay right."
      (is (equal '("" "*" "40") (nm::split-terse ":*:40"))))

(test network-manager-starts-with-ellipsis
      "Before the first update the block shows an ellipsis."
      (with-blocks
        (bk:defblock network-manager)
        (is (string= (icon #x2026) (block-text 'network-manager)))))

(test network-manager-wifi
      "A connected wifi network shows the icon for its signal."
      (with-blocks
        (call-with-commands
            (list (cons "nmcli" (nmcli '("ethernet:disconnected")
                                       '("Other::80" "My\\:Net:*:78"))))
          (lambda ()
            (bk:defblock network-manager)
            (refresh 'network-manager)
            (is (string= (icon #xF0928) (block-text 'network-manager)))))))

(test network-manager-wifi-with-ssid
      "The ssid option shows the network name after the icon."
      (with-blocks
        (call-with-commands
            (list (cons "nmcli" (nmcli '("ethernet:disconnected")
                                       '("Other::80" "My\\:Net:*:78"))))
          (lambda ()
            (bk:defblock network-manager :ssid t)
            (refresh 'network-manager)
            (is (string= (format nil "~a (My:Net)" (icon #xF0928))
                         (block-text 'network-manager)))))))

(test network-manager-ethernet
      "With no wifi in use, a connected ethernet device shows its icon."
      (with-blocks
        (call-with-commands
            (list (cons "nmcli" (nmcli '("ethernet:connected")
                                       '("Other::80"))))
          (lambda ()
            (bk:defblock network-manager)
            (refresh 'network-manager)
            (is (string= (icon #xF0200) (block-text 'network-manager)))))))

(test network-manager-wifi-wins-over-ethernet
      "Wifi is shown when both wifi and ethernet are connected."
      (with-blocks
        (call-with-commands
            (list (cons "nmcli" (nmcli '("ethernet:connected")
                                       '("My\\:Net:*:78"))))
          (lambda ()
            (bk:defblock network-manager)
            (refresh 'network-manager)
            (is (string= (icon #xF0928) (block-text 'network-manager)))))))

(test network-manager-disconnected
      "With no connection the block shows the disconnected icon."
      (with-blocks
        (call-with-commands
            (list (cons "nmcli" (nmcli '("ethernet:unavailable") '())))
          (lambda ()
            (bk:defblock network-manager)
            (refresh 'network-manager)
            (is (string= (icon #xF092D) (block-text 'network-manager)))))))

(test network-manager-event-source-reads-monitor
      "The event source yields the lines nmcli monitor prints."
      (with-blocks
        (call-with-commands (list (cons "nmcli" (nmcli '() '())))
          (lambda ()
            (bk:defblock network-manager)
            (let ((stream (funcall (bk:bkblock-event-source
                                    (registered 'network-manager)))))
              (is (string= "eth0: connected" (read-line stream)))
              (is-false (read-line stream nil nil)))))))

(test network-manager-event-source-survives-missing-nmcli
      "Without nmcli the event source is an empty stream, not an error."
      (with-blocks
        (call-with-commands '()
          (lambda ()
            (bk:defblock network-manager)
            (let ((stream (funcall (bk:bkblock-event-source
                                    (registered 'network-manager)))))
              (is-false (read-line stream nil nil))))
          :isolate t)))
