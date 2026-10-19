;;;; SPDX-License-Identifier: LGPL-2.1-only WITH LLGPL
;;;; Copyright (C) 2026 Erik P Almaraz
;;;;
;;;; Licensed under the LGPL-2.1-only with the LLGPL preamble;
;;;; see the LICENSE file in this repository.

(defpackage #:barker/base/bkblock
  (:use #:cl
        #:barker/base/swaybar-protocol)
  (:import-from #:barker/lib/platform
                #:sigrtmin
                #:sigaction)
  (:export #:bkblock
           #:bkblock-body
           #:bkblock-interval
           #:bkblock-procedure
           #:bkblock-click-handler
           #:bkblock-trigger
           #:bkblock-event-source)
  (:export #:do-procedure
           #:bkblock-handle-click
           #:bkblock-run)
  (:export #:*blocks*
           #:*kinds*
           #:defkind
           #:defblock)
  (:documentation "Status bar blocks: a swaybar body with its update rules."))

(in-package #:barker/base/bkblock)

;;; Code:

(defclass bkblock ()
  ((body
    :initarg :body
    :accessor bkblock-body
    :initform (default-swblock)
    :documentation "Swaybar-protocol body object.")
   (interval
    :initarg :interval
    :accessor bkblock-interval
    :initform :persistent
    :documentation "Seconds between updates, :PERSISTENT for signal updates. ")
   (procedure
    :initarg :procedure
    :accessor bkblock-procedure
    :initform #'identity
    :documentation "Function of the body that returns the updated body.")
   (click-handler
    :initarg :click-handler
    :accessor bkblock-click-handler
    :initform nil
    :documentation "Function that takes click-event and body, returns
updated body, or NIL to ignore clicks.")
   (trigger
    :initarg :trigger
    :accessor bkblock-trigger
    :initform nil
    :documentation "Offset added to SIGRTMIN to update on, or NIL for none.")
   (event-source
    :initarg :event-source
    :accessor bkblock-event-source
    :initform nil
    :documentation "Thunk returning a stream to watch for update events,
or NIL for none."))
  (:documentation "A status bar block: a swaybar body plus the rules that
decide when and how it updates."))


(defun do-procedure (bkblock notify)
  "Run BKBLOCK's procedure on its body and store the result.
Then call NOTIFY, a function of no arguments. Does nothing if the
procedure is not a function."
  (let ((procedure (bkblock-procedure bkblock))
        (body (bkblock-body bkblock)))
    (when (functionp procedure)
      (setf (bkblock-body bkblock) (funcall procedure (copy-swblock body)))
      (funcall notify))))

(defun bkblock-handle-click (bkblock event notify)
  "Run BKBLOCK's click handler on EVENT and its body, store the result.
Then call NOTIFY, a function of no arguments. Does nothing if the
handler is not a function."
  (let ((handler (bkblock-click-handler bkblock))
        (body (bkblock-body bkblock)))
    (when (functionp handler)
      (setf (bkblock-body bkblock) (funcall handler event (copy-swblock body)))
      (funcall notify))))

(defun bkblock-run (bkblock notify)
  "Start BKBLOCK's update cycle; NOTIFY is a function of no arguments.
Register the trigger signal if set, run the procedure once, then
repeat every interval seconds. Never returns for a numeric interval."
  (let ((trigger (bkblock-trigger bkblock))
        (interval (bkblock-interval bkblock)))
    (when trigger
      (sigaction (+ (sigrtmin) trigger)
                 (lambda () (do-procedure bkblock notify))))
    (do-procedure bkblock notify)
    (when (numberp interval)
      (loop
        (sleep interval)
        (do-procedure bkblock notify)))))


;;; Config API

(defvar *blocks* '()
  "Registry holding (NAME . BKBLOCK) instances.")

(defvar *kinds* '()
  "Registry holding (KIND . MAKER) pairs; KIND is a lowercase string.")

(defparameter *bkblock-keys*
  '(:interval :trigger :procedure :click-handler :event-source)
  "The defblock keys that go straight to the bkblock.")

(defun add-or-replace (name bkblock)
  "Add or replace (NAME . BKBLOCK) in the registry."
  (let ((entry (assoc name *blocks*)))
    (if entry (setf (cdr entry) bkblock)
        (setf *blocks* (append *blocks* (list (cons name bkblock)))))))

(defun register-kind (kind maker)
  "Register MAKER under the string KIND, replacing any previous maker."
  (let ((entry (assoc kind *kinds* :test #'string=)))
    (if entry
        (setf (cdr entry) maker)
        (push (cons kind maker) *kinds*))
    kind))

(defmacro defkind (kind lambda-list &body body)
  "Define block KIND, built by a function of LAMBDA-LIST.
LAMBDA-LIST is a &key list that receives the options a config gives
DEFBLOCK beyond the bkblock keys. BODY returns a plist of bkblock
initargs, plus :body, a plist of swblock initargs. These are defaults
that the config can override.

  (defkind battery (&key nerd-icons)
    (list :interval 3 :body (list :full-text \"N/A\")))"
  `(progn
     (register-kind ,(string-downcase (symbol-name kind))
                    (lambda ,lambda-list ,@body))
     ',kind))

(defun split-plist (plist keys)
  "Return two plists: the pairs of PLIST with a key in KEYS, then the rest."
  (loop :for (key value) :on plist :by #'cddr
        :if (member key keys)
        :append (list key value) :into chosen
        :else
        :append (list key value) :into others
        :finally (return (values chosen others))))

(defun build-block (label kind body-plist options extras)
  "Return a bkblock named LABEL.
Without KIND, the block's own name picks a registered kind, if any.
The kind's maker is called with EXTRAS and supplies defaults; BODY-PLIST
and OPTIONS from the config override them."
  (let* ((maker (cdr (assoc (or kind label) *kinds* :test #'string=)))
         (defaults (cond (maker (apply maker extras))
                         (kind (error "Unknown block kind: ~a" kind))
                         (extras (error "Unknown options for ~a: ~s"
                                        label extras)))))
    (apply #'make-instance 'bkblock
           :body (apply #'make-instance 'swblock
                        (append body-plist
                                (getf defaults :body)
                                (list :name label)))
           (append options (nth-value 1 (split-plist defaults '(:body)))))))

(defmacro defblock (name &rest options &key body kind &allow-other-keys)
  "Define the block NAME and add it to the registry.
KIND names the registered kind that supplies defaults; it defaults to
NAME. BODY is a plist of swblock initargs. The keys :interval,
:trigger, :procedure, :click-handler and :event-source go to the
bkblock; any other keys are passed to the kind."
  (let ((label (string-downcase (symbol-name name))))
    (multiple-value-bind (own extras)
        (split-plist (nth-value 1 (split-plist options '(:body :kind)))
                     *bkblock-keys*)
      `(progn
         (add-or-replace
          ',name
          (build-block ,label
                       ,(and kind
                             (string-downcase
                              (symbol-name kind)))
                       (list ,@body)
                       (list ,@own)
                       (list ,@extras)))
         ',name))))
