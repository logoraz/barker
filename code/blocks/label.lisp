(defpackage #:barker/blocks/label
  (:use #:cl)
  (:import-from #:barker/base/bkblock
                #:defkind)
  (:import-from #:barker/base/swaybar-protocol
                #:swblock-urgent)
  (:documentation "Label block: static text that toggles urgent on click."))

(in-package #:barker/blocks/label)


(defun toggle-urgent (event body)
  "Flip BODY's urgent flag and return BODY. EVENT is ignored."
  (declare (ignore event))
  (setf (swblock-urgent body) (not (swblock-urgent body)))
  body)

(defkind label (&key (text "") color)
  "Static TEXT, optionally in COLOR; a click toggles urgent."
  (list :click-handler #'toggle-urgent
        :body (append (list :full-text text :urgent nil)
                      (when color (list :color color)))))
