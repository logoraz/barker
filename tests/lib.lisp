(defpackage #:barker/tests/lib
  (:use #:cl #:5am)
  (:local-nicknames (#:jzon #:com.inuoe.jzon)
                    (#:pf #:barker/lib/platform))
  (:import-from #:barker/tests/suite
                #:suite)
  (:import-from #:barker/lib/jzon-ext
                #:define-json-type)
  (:export #:lib-suite)
  (:documentation "Tests for the lib layer."))

(in-package #:barker/tests/lib)


(def-suite lib-suite
  :description "Tests for code/lib."
  :in suite)

(in-suite lib-suite)


(define-json-type click-event (button) (relative-x))

(define-json-type thing (a "oddKey"))

(define-json-type flag
  (on :type boolean)
  (other))

(define-json-type toggle
  (active "isActive" :type boolean))


;;; Writer

(test derived-keys
      "Slot names become snake_case JSON keys."
      (is (string= "{\"button\":1,\"relative_x\":20}"
                   (jzon:stringify
                    (make-instance 'click-event
                                   :button 1
                                   :relative-x 20)))))

(test unbound-slot-omitted
      "A slot that was never set is left out of the JSON."
      (is (string= "{\"button\":1}"
                   (jzon:stringify
                    (make-instance 'click-event :button 1)))))

(test all-unbound-empty-object
      "An instance with no slots set writes an empty object."
      (is (string= "{}"
                   (jzon:stringify (make-instance 'click-event)))))

(test explicit-key-override
      "An explicit key in the slot spec wins over the derived key."
      (is (string= "{\"oddKey\":1}"
                   (jzon:stringify (make-instance 'thing :a 1)))))

(test boolean-slot-writes-false
      "A boolean slot holding NIL writes false; an untyped slot writes null."
      (is (string= "{\"on\":false,\"other\":null}"
                   (jzon:stringify
                    (make-instance 'flag :on nil :other nil)))))

(test boolean-slot-writes-true
      "A boolean slot holding T writes true."
      (is (string= "{\"on\":true}"
                   (jzon:stringify (make-instance 'flag :on t)))))

(test explicit-key-with-boolean-type
      "A slot with an explicit key and a boolean type writes false."
      (is (string= "{\"isActive\":false}"
                   (jzon:stringify
                    (make-instance 'toggle :active nil)))))


;;; Parser

(test parse-found-keys
      "Keys present in the JSON set their slots."
      (let ((e (json->click-event "{\"button\":1,\"relative_x\":20}")))
        (is (= 1 (click-event-button e)))
        (is (= 20 (click-event-relative-x e)))))

(test parse-absent-key-unbound
      "A key missing from the JSON leaves its slot unbound."
      (let ((e (json->click-event "{\"button\":1}")))
        (is-true (slot-boundp e 'button))
        (is-false (slot-boundp e 'relative-x))))

(test parse-empty-object
      "An empty JSON object gives an instance with no slots set."
      (let ((e (json->click-event "{}")))
        (is-false (slot-boundp e 'button))
        (is-false (slot-boundp e 'relative-x))))

(test parse-false-is-not-absent
      "JSON false sets the slot to NIL, which is not the same as absent."
      (let ((e (json->click-event "{\"button\":false}")))
        (is-true (slot-boundp e 'button))
        (is (null (click-event-button e)))))

(test parse-explicit-key-override
      "An explicit key in the slot spec is used when parsing."
      (let ((x (json->thing "{\"oddKey\":5}")))
        (is (= 5 (thing-a x)))))

(test round-trip
      "Stringifying an instance and parsing it back keeps its slots."
      (let* ((original (make-instance 'click-event
                                      :button 1
                                      :relative-x 20))
             (copy (json->click-event (jzon:stringify original))))
        (is (= 1 (click-event-button copy)))
        (is (= 20 (click-event-relative-x copy)))))

;;; Notifier

(test notifier-notify-then-wait
      "A pending notify makes wait return at once."
      (let ((n (pf:make-notifier)))
        (pf:notifier-notify n)
        (is-true (pf:notifier-wait n))))

(test notifier-burst-collapses
      "Several notifies are consumed by a single wait."
      (let ((n (pf:make-notifier)))
        (pf:notifier-notify n)
        (pf:notifier-notify n)
        (pf:notifier-notify n)
        (pf:notifier-wait n)
        (is-true (sb-concurrency:mailbox-empty-p
                  (pf::notifier-mailbox n)))))

(test notifier-wait-blocks-until-notified
      "Wait blocks until another thread notifies."
      (let* ((n (pf:make-notifier))
             (start (get-internal-real-time)))
        (bt2:make-thread (lambda ()
                          (sleep 0.1)
                          (pf:notifier-notify n)))
        (pf:notifier-wait n)
        (is (>= (- (get-internal-real-time) start)
                (* 0.08 internal-time-units-per-second)))))
