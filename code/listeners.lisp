(defpackage #:barker/listeners
  (:use #:cl)
  (:import-from #:barker/lib/platform
                #:notifier-wait)
  (:import-from #:barker/base/swaybar-protocol
                #:name-of
                #:json->click-event)
  (:import-from #:barker/base/bkblock
                #:do-procedure
                #:bkblock-body
                #:bkblock-event-source
                #:bkblock-handle-click)
  (:local-nicknames (#:jzon #:com.inuoe.jzon))
  (:export #:update-listener
           #:click-listener
           #:event-source-listener)
  (:documentation "Threads that drive the bar: output, clicks, events."))

(in-package #:barker/listeners)

;;; Code:

(defun write-bar (bkblocks stream)
  "Write one bar line to STREAM: the bodies of BKBLOCKS as a JSON array,
followed by a comma and a newline."
  (let ((bodies (map 'vector #'bkblock-body bkblocks)))
    (jzon:stringify bodies :stream stream)
    (format stream ",~%")
    (finish-output stream)))

(defun update-listener (notifier bkblocks stream)
  "Wait on NOTIFIER; after each wake-up, write the bar for BKBLOCKS to
STREAM. Never returns."
  (loop
       (notifier-wait notifier)
       (write-bar bkblocks stream)))

(defun start-event-thread (bb notify)
  "Start a thread that updates BB on every line of its event source."
  (let ((event-source (bkblock-event-source bb)))
    (when event-source
      (bt2:make-thread
       (lambda ()
         (let ((input (funcall event-source)))
           (loop :for line = (read-line input nil :eof)
                 :until (eq line :eof)
                 :do (do-procedure bb notify))))))))


(defun event-source-listener (notify bkblocks)
  "Start a thread for each bkblock in BKBLOCKS that has an event source.
Each thread reads lines from its source and updates the block on every
line. NOTIFY is a function of no arguments."
  (loop :for bb :in bkblocks
        :when (bkblock-event-source bb)
        :collect (start-event-thread bb notify)))


(defun find-block (name bkblocks)
  "Return the bkblock in BKBLOCKS whose body is named NAME, or NIL."
  (find name bkblocks
        :key (lambda (bb)
               (name-of (bkblock-body bb)))
        :test #'equal))

(defun click-listener (notify bkblocks stream)
  "Read swaybar click events from STREAM, find the bkblock each one is
for, and call its click handler. NOTIFY is a function of no arguments.
Returns when STREAM reaches EOF."
  (read-line stream nil nil) ; discard "[" line
  (loop :for line = (read-line stream nil :eof)
        :until (eq line :eof)
        :do (let* ((event (json->click-event (string-trim "," line)))
                    (name (name-of event))
                    (bb (find-block name bkblocks)))
              (when (and name bb)
                (bkblock-handle-click bb event notify)))))
