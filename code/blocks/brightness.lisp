;;;; SPDX-License-Identifier: LGPL-2.1-only WITH LLGPL
;;;; Copyright (C) 2026 Erik P Almaraz
;;;;
;;;; Licensed under the LGPL-2.1-only with the LLGPL preamble;
;;;; see the LICENSE file in this repository.

;;; Port of gubar's brightness block.

(defpackage #:barker/blocks/brightness
  (:use #:cl)
  (:import-from #:barker/base/swaybar-protocol
                #:swblock-full-text)
  (:import-from #:barker/base/bkblock
                #:defkind)
  (:import-from #:barker/blocks/support
                #:glyphs
                #:command-output
                #:guarded)
  (:documentation "Brightness block: backlight level from brightnessctl."))

(in-package #:barker/blocks/brightness)


(defparameter *icons* (glyphs #xF00E0 #xF00DD #xF00DF #xF00DE)
  "Nerd font brightness icons, from dim to bright.")

(defun brightness-percent ()
  "Return the current backlight level as a whole percent."
  (let ((current (parse-integer (command-output "brightnessctl" "get")
                                :junk-allowed t))
        (maximum (parse-integer (command-output "brightnessctl" "max")
                                :junk-allowed t)))
    (floor (* 100 current) maximum)))

(defkind brightness (&key (format "~a ~a%") nerd-icons)
  "Backlight percent, updated on signal 4 (SIGRTMIN+4)."
  (list :trigger 4
        :interval :persistent
        :body (list :full-text "N/A")
        :procedure
        (guarded
         (lambda (body)
           (let* ((percent (brightness-percent))
                  (label (if nerd-icons
                             (aref *icons* (min (1- (length *icons*))
                                                (floor percent 26)))
                             "BRT:")))
             (setf (swblock-full-text body)
                   (format nil format label percent))
             body)))))
