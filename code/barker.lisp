;;;; SPDX-License-Identifier: LGPL-2.1-only WITH LLGPL
;;;; Copyright (C) 2026 Erik P Almaraz
;;;;
;;;; Licensed under the LGPL-2.1-only with the LLGPL preamble;
;;;; see the LICENSE file in this repository.

(defpackage #:barker
  (:use #:cl
        #:barker/lib/platform
        #:barker/base/swaybar-protocol
        #:barker/base/bkblock
        #:barker/blocks/support
        #:barker/listeners)
  (:local-nicknames (#:jzon #:com.inuoe.jzon))
  (:export #:main
           #:config-file
           #:default-block
           #:error-block
           #:load-config
           #:write-header
           #:run)
  (:documentation "Startup and Entry for Barker."))

(in-package #:barker)


;;; User Configuration Startup

(defun config-file ()
  "Return the config file path as a string.
Use $XDG_CONFIG_HOME when set and non-empty, else $HOME/.config."
  (let ((xdg (sb-posix:getenv "XDG_CONFIG_HOME")))
    (concatenate 'string
                 (if (and xdg (plusp (length xdg)))
                     xdg
                     (concatenate 'string (sb-posix:getenv "HOME")
                                  "/.config"))
                 "/barker/config.lisp")))

(defun default-block ()
  "Return the bkblock used when no config file exists."
  (make-instance 'bkblock
                 :body (make-instance 'swblock
                                      :name "barker"
                                      :full-text "barker")))

(defun error-block (condition)
  "Return a bkblock that shows CONDITION in red."
  (make-instance
   'bkblock
   :body (make-instance 'swblock
                        :name "config-error"
                        :full-text (format nil "barker config: ~a"
                                           condition)
                        :color "#FF0000")))

(defun load-config ()
  "Load the user's config file and return its bkblocks as a list.
Load with *PACKAGE* bound to BARKER so DEFBLOCK resolves. Output
printed by the config goes to stderr. Return a default block if the
file is missing, or an error block if loading signals an error."
  (let ((file (config-file)))
    (if (probe-file file)
        (handler-case
            (let ((*package* (find-package '#:barker))
                  (*standard-output* *error-output*)
                  (*blocks* '()))
              (load file)
              (mapcar #'cdr *blocks*))
          (error (condition)
            (list (error-block condition))))
        (list (default-block)))))

;;; Startup

(defun write-header (stream)
  "Write the swaybar header line to STREAM and open the infinite array."
  (format stream "~a~%[[]," (jzon:stringify *header-with-clicks*))
  (finish-output stream))

(defun run (bkblocks)
  "Run the bar for BKBLOCKS; never returns.
Write the header, start one thread per block, the click listener and
the event sources, then write updates from the calling thread. Exit
the process when swaybar closes standard input."
  (let* ((notifier (make-notifier))
         (notify (lambda () (notifier-notify notifier)))
         (in *standard-input*)
         (out *standard-output*))
    (write-header out)
    (mapc (lambda (bb)
            (bt2:make-thread (lambda () (bkblock-run bb notify))
                             :name "barker-block"))
          bkblocks)
    (bt2:make-thread (lambda ()
                       (click-listener notify bkblocks in)
                       (sb-ext:exit :code 0 :abort t))
                     :name "barker-clicks")
    (event-source-listener notify bkblocks)
    (update-listener notifier bkblocks out)))

(defun main ()
  "Entry point: load the user's config and run the bar."
  (sb-ext:disable-debugger)
  (run (load-config)))
