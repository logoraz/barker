;;;; SPDX-License-Identifier: LGPL-2.1-only WITH LLGPL
;;;; Copyright (C) 2026 Erik P Almaraz
;;;;
;;;; Licensed under the LGPL-2.1-only with the LLGPL preamble;
;;;; see the LICENSE file in this repository.

;;; Port of gubar's brightness block.

(defpackage #:barker/blocks/brightness
  (:use #:cl)
  (:local-nicknames (#:bk #:barker/base/bkblock)
                    (#:sp #:barker/base/swaybar-protocol)
                    (#:support #:barker/blocks/support))
  (:documentation "Brightness block: backlight level from brightnessctl."))

(in-package #:barker/blocks/brightness)


(defparameter *icons* (support:glyphs #xF00E0 #xF00DD #xF00DF #xF00DE)
  "Nerd font brightness icons, from dim to bright.")

(defun brightness-percent ()
  "Return the current backlight level as a whole percent."
  (let ((current (parse-integer (support:command-output "brightnessctl" "get")
                                :junk-allowed t))
        (maximum (parse-integer (support:command-output "brightnessctl" "max")
                                :junk-allowed t)))
    (floor (* 100 current) maximum)))

(bk:defkind brightness (&key (format "~a ~a%") nerd-icons)
  "Backlight percent, updated on signal 4 (SIGRTMIN+4)."
  (list :trigger 4
        :interval :persistent
        :body (list :full-text "N/A")
        :procedure
        (support:guarded
         (lambda (body)
           (let* ((percent (brightness-percent))
                  (label (if nerd-icons
                             (aref *icons* (min (1- (length *icons*))
                                                (floor percent 26)))
                             "BRT:")))
             (setf (sp:swblock-full-text body)
                   (format nil format label percent))
             body)))))
