;;;; SPDX-License-Identifier: LGPL-2.1-only WITH LLGPL
;;;; Copyright (C) 2026 Erik P Almaraz
;;;;
;;;; Licensed under the LGPL-2.1-only with the LLGPL preamble;
;;;; see the LICENSE file in this repository.

(defpackage #:barker/blocks/support
  (:use #:cl)
  (:local-nicknames (#:sp #:barker/base/swaybar-protocol)
                    (#:sba #:sb-alien))
  (:export #:glyphs
           #:guarded
           #:command-output
           #:read-file-line
           #:strftime)
  (:documentation "Helpers shared by the core blocks."))

(in-package #:barker/blocks/support)


;;; Display

(defun glyphs (&rest codes)
  "Return a vector of one-character strings for the code points CODES."
  (map 'vector (lambda (code) (string (code-char code))) codes))

(defun guarded (procedure &optional (fallback "N/A"))
  "Return a body procedure that runs PROCEDURE and tolerates errors.
When PROCEDURE signals an error, set the body's text to FALLBACK and
return the body, so one failing block never takes down the bar."
  (lambda (body)
    (handler-case (funcall procedure body)
      (error ()
        (setf (sp:swblock-full-text body) fallback)
        body))))


;;; System

(defun command-output (program &rest arguments)
  "Run PROGRAM with ARGUMENTS and return its standard output as a string.
Search the PATH for PROGRAM. Return NIL if it exits with a nonzero
status. Signal an error if PROGRAM cannot be run at all."
  (let* ((process nil)
         (output (with-output-to-string (out)
                   (setf process (sb-ext:run-program program arguments
                                                     :search t
                                                     :input nil
                                                     :output out
                                                     :error nil)))))
    (when (eql 0 (sb-ext:process-exit-code process))
      output)))

(defun read-file-line (path)
  "Return the first line of the file at PATH."
  (with-open-file (in path)
    (read-line in)))

(defun strftime (format)
  "Format the current local time with the C library's strftime.
FORMAT is a strftime control string such as \"%a %H:%M\"."
  (sba:with-alien ((now sba:long)
                   (tm (array sba:unsigned-char 128))
                   (buffer (array sba:char 256)))
    (setf now (sba:alien-funcall
               (sba:extern-alien
                "time" (function sba:long sba:unsigned-long))
               0))
    (sba:alien-funcall
     (sba:extern-alien
      "localtime_r"
      (function sba:unsigned-long
                (* sba:long) (* (array sba:unsigned-char 128))))
     (sba:addr now) (sba:addr tm))
    (sba:alien-funcall
     (sba:extern-alien
      "strftime"
      (function sba:unsigned-long
                (* (array sba:char 256)) sba:unsigned-long
                sba:c-string (* (array sba:unsigned-char 128))))
     (sba:addr buffer) 256 format (sba:addr tm))
    (sba:cast buffer sba:c-string)))
