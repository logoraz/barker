;;;; SPDX-License-Identifier: LGPL-2.1-only WITH LLGPL
;;;; Copyright (C) 2026 Erik P Almaraz
;;;;
;;;; Licensed under the LGPL-2.1-only with the LLGPL preamble;
;;;; see the LICENSE file in this repository.

;;; Port of gubar's date-time block, event-source driven only.

(defpackage #:barker/blocks/clock
  (:use #:cl)
  (:local-nicknames (#:bk #:barker/base/bkblock)
                    (#:sp #:barker/base/swaybar-protocol)
                    (#:gray #:trivial-gray-streams)
                    (#:support #:barker/blocks/support))
  (:documentation "Clock block: local time redrawn on each second."))

(in-package #:barker/blocks/clock)


(defclass second-ticker (gray:fundamental-character-input-stream)
  ()
  (:documentation "An input stream that yields one line per second."))

(defun sleep-until-next-second ()
  "Sleep until just past the next whole second."
  (multiple-value-bind (seconds microseconds) (sb-ext:get-time-of-day)
    (declare (ignore seconds))
    (sleep (+ (/ (- 1000000 microseconds) 1000000.0) 0.002))))

(defmethod gray:stream-read-char ((stream second-ticker))
  "Wait for the next second boundary and return a newline."
  (sleep-until-next-second)
  #\Newline)

(defmethod gray:stream-read-line ((stream second-ticker))
  "Wait for the next second boundary and return an empty line."
  (sleep-until-next-second)
  (values "" nil))

(bk:defkind clock (&key (format "%c"))
  "Local time shown with the strftime FORMAT, updated every second."
  (list :interval :persistent
        :event-source (lambda () (make-instance 'second-ticker))
        :procedure (lambda (body)
                     (setf (sp:swblock-full-text body)
                           (support:strftime format))
                     body)))
