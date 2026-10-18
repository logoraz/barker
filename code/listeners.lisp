;;;; SPDX-License-Identifier: LGPL-2.1-only WITH LLGPL
;;;; Copyright (C) 2026 Erik P Almaraz
;;;;
;;;; Licensed under the LGPL-2.1-only with the LLGPL preamble;
;;;; see the LICENSE file in this repository.

(defpackage #:barker/listeners
  (:use #:cl)
  (:local-nicknames (#:jzon #:com.inuoe.jzon)
                    (#:pf #:barker/lib/platform)
                    (#:bk #:barker/base/bkblock)
                    (#:sp #:barker/base/swaybar-protocol))
  (:export #:update-listener
           #:click-listener
           #:event-source-listener)
  (:documentation "Threads that drive the bar: output, clicks, events."))

(in-package #:barker/listeners)

;;; Code:

(defun write-bar (bkblocks stream)
  "Write one bar line to STREAM: the bodies of BKBLOCKS as a JSON array,
followed by a comma and a newline."
  (let ((bodies (map 'vector #'bk:bkblock-body bkblocks)))
    (jzon:stringify bodies :stream stream)
    (format stream ",~%")
    (finish-output stream)))

(defun update-listener (notifier bkblocks stream)
  "Wait on NOTIFIER; after each wake-up, write the bar for BKBLOCKS to
STREAM. Never returns."
  (loop
       (pf:notifier-wait notifier)
       (write-bar bkblocks stream)))

(defun start-event-thread (bb notify)
  "Start a thread that updates BB on every line of its event source."
  (let ((event-source (bk:bkblock-event-source bb)))
    (when event-source
      (bt2:make-thread
       (lambda ()
         (let ((input (funcall event-source)))
           (loop :for line = (read-line input nil :eof)
                 :until (eq line :eof)
                 :do (bk:do-procedure bb notify))))))))


(defun event-source-listener (notify bkblocks)
  "Start a thread for each bkblock in BKBLOCKS that has an event source.
Each thread reads lines from its source and updates the block on every
line. NOTIFY is a function of no arguments."
  (loop :for bb :in bkblocks
        :when (bk:bkblock-event-source bb)
        :collect (start-event-thread bb notify)))


(defun find-block (name bkblocks)
  "Return the bkblock in BKBLOCKS whose body is named NAME, or NIL."
  (find name bkblocks
        :key (lambda (bb)
               (sp:name-of (bk:bkblock-body bb)))
        :test #'equal))

(defun click-listener (notify bkblocks stream)
  "Read swaybar click events from STREAM, find the bkblock each one is
for, and call its click handler. NOTIFY is a function of no arguments.
Returns when STREAM reaches EOF."
  (read-line stream nil nil) ; discard "[" line
  (loop :for line = (read-line stream nil :eof)
        :until (eq line :eof)
        :do (let* ((event (sp:json->click-event (string-trim "," line)))
                    (name (sp:name-of event))
                    (bb (find-block name bkblocks)))
              (when (and name bb)
                (bk:bkblock-handle-click bb event notify)))))
