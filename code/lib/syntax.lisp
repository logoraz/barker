(defpackage #:barker/lib/syntax
  (:use #:cl)
  (:export #:concat
           #:join-strings
           #:nlet)
  (:documentation "Syntactic Language Extensions."))

(in-package #:barker/lib/syntax)


;;; String Manipulation

(defun concat (&rest strings)
  "Shorthand for CONCATENATE specialized for strings."
  (apply #'concatenate 'string strings))

(defun join-strings (strings &key (separator ""))
  "Join STRINGS into one string, placing SEPARATOR between adjacent elements.
Return \"\" if STRINGS is empty."
  (with-output-to-string (out)
    (loop :for (string . more) :on strings
          :do (write-string string out)
              (when more (write-string separator out)))))


;;; Macros

(defmacro nlet (name bindings &body body)
  `(labels ((,name ,(mapcar #'car bindings) ,@body))
     (,name ,@(mapcar #'cadr bindings))))
