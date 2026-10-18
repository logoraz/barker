;;;; SPDX-License-Identifier: LGPL-2.1-only WITH LLGPL
;;;; Copyright (C) 2026 Erik P Almaraz
;;;;
;;;; Licensed under the LGPL-2.1-only with the LLGPL preamble;
;;;; see the LICENSE file in this repository.

;;; Port of gubar's label block.

(defpackage #:barker/blocks/label
  (:use #:cl)
  (:local-nicknames (#:bk #:barker/base/bkblock)
                    (#:sp #:barker/base/swaybar-protocol))
  (:documentation "Label block: static text that toggles urgent on click."))

(in-package #:barker/blocks/label)


(defun toggle-urgent (event body)
  "Flip BODY's urgent flag and return BODY. EVENT is ignored."
  (declare (ignore event))
  (setf (sp:swblock-urgent body) (not (sp:swblock-urgent body)))
  body)

(bk:defkind label (&key (text "") color)
  "Static TEXT, optionally in COLOR; a click toggles urgent."
  (list :click-handler #'toggle-urgent
        :body (append (list :full-text text :urgent nil)
                      (when color (list :color color)))))
