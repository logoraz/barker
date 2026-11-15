(defpackage #:barker/lib/jzon-ext
  (:use #:cl)
  (:local-nicknames (#:jzon #:com.inuoe.jzon))
  (:export #:define-json-type)
  (:documentation "Extensions for jzon: define classes with JSON support."))

(in-package #:barker/lib/jzon-ext)

(defun make-slot-definition (type-name slot-spec)
  "Takes a TYPE-NAME and a SLOT-SPEC and returns a defclass slot definition."
  (let* ((slot-name (first slot-spec))
         (initarg (intern (symbol-name slot-name) "KEYWORD"))
         (accessor (intern (format nil "~A-~A" type-name slot-name))))
    (list slot-name :initarg initarg :accessor accessor)))

(defun slot-json-key (slot-spec)
  "Return the JSON key string for SLOT-SPEC.
SLOT-SPEC is a list: (SLOT-NAME), (SLOT-NAME KEY), (SLOT-NAME :TYPE BOOLEAN),
and (SLOT-NAME KEY :TYPE BOOLEAN). If KEY is provided as a string,
return it unchanged. Otherwise derive the key from SLOT-NAME."
  (let ((slot-name (first slot-spec))
        (key (second slot-spec)))
    (if (stringp key) key
        (substitute #\_ #\- (string-downcase (symbol-name slot-name))))))

(defun slot-type (slot-spec)
  "Return the slot type from SLOT-SPEC if provided."
  (let ((type (member :type slot-spec)))
    (or (null type) (second type))))

(defun slot-key-pairs (spec)
  "Return the pair (SLOT KEY TYPE) for SLOT-SPEC.
KEY is the explicit key in SLOT-SPEC, or one derived from SLOT.
TYPE is the boolean type in SLOT-SPEC, if provided."
  (list (first spec) (slot-json-key spec) (slot-type spec)))

(defmacro define-json-type (type-name &rest slot-specs)
  "Define a class named TYPE-NAME with JSON reading and writing support.
Each element of SLOT-SPECS is (SLOT), (SLOT KEY), (SLOT :TYPE TYPE), or
(SLOT KEY :TYPE TYPE). SLOT names a slot of the class. KEY is its JSON key;
when omitted, it is derived from SLOT by downcasing and replacing hyphens
with underscores. TYPE is passed to jzon when writing; with :TYPE BOOLEAN, a
slot holding NIL is written as false instead of null.

Expands into four forms:
  - a DEFCLASS with a keyword initarg and an accessor for each slot, where
    the accessor is named TYPE-NAME-SLOT.
  - a method on JZON:COERCED-FIELDS, so JZON:STRINGIFY writes the instance as
    a JSON object. Unbound slots are omitted.
  - a function JSON->TYPE-NAME that parses a JSON string into an instance.
    Keys missing from the string leave their slots unbound.
  - a function COPY-TYPE-NAME that returns a shallow copy of an instance.
    Unbound slots stay unbound in the copy.

Example:
  (define-json-type click-event
    (button)
    (relative-x \"relative_x\")
    (urgent :type boolean))"
  `(progn
     (defclass ,type-name ()
       ,(mapcar (lambda (spec)
                  (make-slot-definition type-name spec))
                slot-specs))

     (defmethod jzon:coerced-fields ((obj ,type-name))
       (loop :for (slot key type) :in ',(mapcar #'slot-key-pairs slot-specs)
             :when (slot-boundp obj slot)
             :collect (list key (slot-value obj slot) type)))

     (defun ,(intern (format nil "JSON->~A" type-name)) (string)
       "Parse the JSON STRING into an instance."
       (let ((table (jzon:parse string))
             (obj (make-instance ',type-name)))
         (loop :for (slot key) :in ',(mapcar #'slot-key-pairs slot-specs)
               :do (multiple-value-bind (value found) (gethash key table)
                     (when found
                       (setf (slot-value obj slot) value))))
         obj))

     (defun ,(intern (format nil "COPY-~A" type-name)) (obj)
       "Return a shallow copy of OBJ with the same bound slots."
       (let ((new (make-instance ',type-name)))
         ,@(mapcar (lambda (spec)
                     `(when (slot-boundp obj ',(first spec))
                        (setf (slot-value new ',(first spec))
                              (slot-value obj ',(first spec)))))
                   slot-specs)
         new))))
