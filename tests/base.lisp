;;;; SPDX-License-Identifier: LGPL-2.1-only WITH LLGPL
;;;; Copyright (C) 2026 Erik P Almaraz
;;;;
;;;; Licensed under the LGPL-2.1-only with the LLGPL preamble;
;;;; see the LICENSE file in this repository.

;;; base.lisp

(defpackage #:barker/tests/base
  (:use #:cl #:5am)
  (:import-from #:barker/tests/suite
                #:suite)
  (:local-nicknames (#:jzon #:com.inuoe.jzon)
                    (#:sp #:barker/base/swaybar-protocol)
                    (#:bk #:barker/base/bkblock))
  (:export #:base-suite)
  (:documentation "Tests for the base layer."))

(in-package #:barker/tests/base)


(def-suite base-suite
  :description "Tests for code/base."
  :in suite)

(in-suite base-suite)


;;; Swaybar Protocol

(test header-writes-version-and-click-events
      "A header with version and click events writes both keys."
      (is (string= "{\"version\":1,\"click_events\":true}"
                   (jzon:stringify
                    (make-instance 'sp:header
                                   :version 1
                                   :click-events t)))))

(test header-with-clicks
      "The ready-made header writes version and click_events."
      (is (string= "{\"version\":1,\"click_events\":true}"
                   (jzon:stringify sp:*header-with-clicks*))))

(test header-signal-keys
      "Signal slots write as snake_case keys."
      (is (string= "{\"version\":1,\"cont_signal\":18,\"stop_signal\":19}"
                   (jzon:stringify
                    (make-instance 'sp:header
                                   :version 1
                                   :cont-signal 18
                                   :stop-signal 19)))))

(test header-click-events-false
      "A header with click events off writes false, not null."
      (is (string= "{\"version\":1,\"click_events\":false}"
                   (jzon:stringify
                    (make-instance 'sp:header
                                   :version 1
                                   :click-events nil)))))

(test default-swblock-is-empty
      "A default swblock has no slots set."
      (is (string= "{}" (jzon:stringify (sp:default-swblock)))))

(test swblock-derived-keys
      "Hyphenated swblock slots write as snake_case keys."
      (is (string= "{\"border_bottom\":2,\"separator_block_width\":5}"
                   (jzon:stringify
                    (make-instance 'sp:swblock
                                   :border-bottom 2
                                   :separator-block-width 5)))))

(test swblock-full-and-short-text
      "full_text and short_text are written in slot order."
      (is (string= "{\"full_text\":\"hi\",\"short_text\":\"h\"}"
                   (jzon:stringify
                    (make-instance 'sp:swblock
                                   :full-text "hi"
                                   :short-text "h")))))

(test swblock-booleans-write-false
      "urgent and separator write false when NIL."
      (is (string= "{\"separator\":false,\"urgent\":false}"
                   (jzon:stringify
                    (make-instance 'sp:swblock
                                   :separator nil
                                   :urgent nil)))))

(test parse-swblock
      "json->swblock sets present keys and leaves the rest unset."
      (let ((b (sp:json->swblock "{\"full_text\":\"hi\",\"urgent\":true}")))
        (is (string= "hi" (sp:swblock-full-text b)))
        (is-true (sp:swblock-urgent b))
        (is (string= "{\"full_text\":\"hi\",\"urgent\":true}"
                     (jzon:stringify b)))))

(test parse-full-click-event
      "json->click-event reads every field of a swaybar click event."
      (let ((e (sp:json->click-event
                (concatenate
                 'string
                 "{\"name\":\"clock\",\"instance\":\"edt\","
                 "\"x\":1900,\"y\":10,\"button\":1,\"event\":274,"
                 "\"relative_x\":100,\"relative_y\":8,"
                 "\"width\":120,\"height\":18}"))))
        (is (string= "clock" (sp:click-event-name e)))
        (is (string= "edt" (sp:click-event-instance e)))
        (is (= 1900 (sp:click-event-x e)))
        (is (= 10 (sp:click-event-y e)))
        (is (= 1 (sp:click-event-button e)))
        (is (= 274 (sp:click-event-event e)))
        (is (= 100 (sp:click-event-relative-x e)))
        (is (= 8 (sp:click-event-relative-y e)))
        (is (= 120 (sp:click-event-width e)))
        (is (= 18 (sp:click-event-height e)))))

(test parse-click-event-without-block-name
      "A click on an unnamed block leaves name and instance unset."
      (let ((e (sp:json->click-event "{\"button\":1,\"x\":5,\"y\":6}")))
        (is (string= "{\"button\":1,\"x\":5,\"y\":6}"
                     (jzon:stringify e)))))

;;; bkblock

(test bkblock-defaults
      "A new bkblock has the documented defaults."
      (let ((b (make-instance 'bk:bkblock)))
        (is (eq :persistent (bk:bkblock-interval b)))
        (is (eq #'identity (bk:bkblock-procedure b)))
        (is-false (bk:bkblock-click-handler b))
        (is-false (bk:bkblock-trigger b))
        (is-false (bk:bkblock-event-source b))
        (is (string= "{}" (jzon:stringify (bk:bkblock-body b))))))

(test bkblock-bodies-are-separate
      "Each bkblock gets its own body object."
      (is (not (eq (bk:bkblock-body (make-instance 'bk:bkblock))
                   (bk:bkblock-body (make-instance 'bk:bkblock))))))

(defun record-notify ()
  "Return a NOTIFY function and a function that gives its call count."
  (let ((count 0))
    (values (lambda () (incf count))
            (lambda () count))))

(test do-procedure-updates-body
      "do-procedure stores the procedure's result and calls notify once."
      (multiple-value-bind (notify calls) (record-notify)
        (let ((b (make-instance
                  'bk:bkblock
                  :procedure (lambda (body)
                               (setf (sp:swblock-full-text body) "hi")
                               body))))
          (bk:do-procedure b notify)
          (is (string= "hi" (sp:swblock-full-text (bk:bkblock-body b))))
          (is (= 1 (funcall calls))))))

(test do-procedure-skips-non-function
      "do-procedure does nothing when the procedure is not a function."
      (multiple-value-bind (notify calls) (record-notify)
        (let ((b (make-instance 'bk:bkblock :procedure nil)))
          (bk:do-procedure b notify)
          (is (= 0 (funcall calls))))))

(test run-persistent-runs-once
      "A persistent block with no trigger runs once and returns."
      (let* ((count 0)
             (b (make-instance 'bk:bkblock :interval :persistent)))
        (bk:bkblock-run b (lambda () (incf count)))
        (is (= 1 count))))

(test run-trigger-signal-updates
      "The trigger signal runs the procedure again."
      (let* ((count 0)
             (b (make-instance 'bk:bkblock :trigger 3)))
        (bk:bkblock-run b (lambda () (incf count)))
        (sb-posix:kill (sb-posix:getpid)
                       (+ (barker/lib/platform:sigrtmin) 3))
        (sleep 0.1)
        (is (= 2 count))))

(test run-interval-repeats
      "A numeric interval runs the procedure repeatedly."
      (let* ((count 0)
             (b (make-instance 'bk:bkblock :interval 0.05))
             (th (bt2:make-thread
                  (lambda ()
                    (bk:bkblock-run b (lambda () (incf count)))))))
        (sleep 0.3)
        (bt2:destroy-thread th)
        (is (>= count 3))))

;;; defblock

(test defblock-registers-block
      "defblock adds one named bkblock to the registry."
      (let ((bk:*blocks* nil))
        (bk:defblock clock :interval 1)
        (is (= 1 (length bk:*blocks*)))
        (is (eq 'clock (car (first bk:*blocks*))))
        (is (typep (cdr (first bk:*blocks*)) 'bk:bkblock))))

(test defblock-sets-keywords
      "defblock passes interval and trigger to the bkblock."
      (let ((bk:*blocks* nil))
        (bk:defblock clock :interval 2 :trigger 3)
        (let ((b (cdr (assoc 'clock bk:*blocks*))))
          (is (= 2 (bk:bkblock-interval b)))
          (is (= 3 (bk:bkblock-trigger b))))))

(test defblock-unset-keys-use-defaults
      "Keys left out of defblock keep the bkblock defaults."
      (let ((bk:*blocks* nil))
        (bk:defblock clock)
        (let ((b (cdr (assoc 'clock bk:*blocks*))))
          (is (eq :persistent (bk:bkblock-interval b)))
          (is-false (bk:bkblock-trigger b))
          (is-false (bk:bkblock-click-handler b)))))

(test defblock-body-plist
      "The :body plist becomes the block's swblock."
      (let ((bk:*blocks* nil))
        (bk:defblock clock :body (:color "#ffffff"))
        (let ((b (cdr (assoc 'clock bk:*blocks*))))
          (is (string= "#ffffff"
                       (sp:swblock-color (bk:bkblock-body b)))))))

(test defblock-sets-swblock-name
      "The block's name becomes the swblock name."
      (let ((bk:*blocks* nil))
        (bk:defblock clock)
        (let ((b (cdr (assoc 'clock bk:*blocks*))))
          (is (string= "clock"
                       (sp:swblock-name (bk:bkblock-body b)))))))

(test defblock-redefine-replaces-in-place
      "Redefining a block replaces it and keeps its position."
      (let ((bk:*blocks* nil))
        (bk:defblock clock :interval 1)
        (bk:defblock mail)
        (bk:defblock clock :interval 5)
        (is (equal '(clock mail) (mapcar #'car bk:*blocks*)))
        (is (= 5 (bk:bkblock-interval
                  (cdr (assoc 'clock bk:*blocks*)))))))

;;; defgenerics

(test name-of-swblock
      "name-of returns an swblock's name."
      (is (string= "clock"
                   (sp:name-of (make-instance 'sp:swblock :name "clock")))))

(test name-of-unnamed-swblock
      "name-of returns NIL for an swblock without a name."
      (is-false (sp:name-of (make-instance 'sp:swblock))))

(test name-of-click-event
      "name-of returns a click event's name."
      (is (string= "clock"
                   (sp:name-of
                    (sp:json->click-event "{\"name\":\"clock\"}")))))

(test name-of-unnamed-click-event
      "name-of returns NIL for a click event without a name."
      (is-false (sp:name-of (sp:json->click-event "{\"button\":1}"))))


;;; defkind

(defun registered (name)
  "Return the bkblock registered under NAME."
  (cdr (assoc name bk:*blocks*)))

(test defkind-registers-kind
      "defkind adds the kind to the registry under a lowercase string."
      (let ((bk:*kinds* nil))
        (bk:defkind clock () (list))
        (is (= 1 (length bk:*kinds*)))
        (is (assoc "clock" bk:*kinds* :test #'string=))))

(test defkind-redefine-replaces
      "Defining a kind twice keeps one entry."
      (let ((bk:*kinds* nil))
        (bk:defkind clock () (list :interval 1))
        (bk:defkind clock () (list :interval 2))
        (is (= 1 (length bk:*kinds*)))))

(test defblock-uses-kind-named-after-block
      "A block named after a kind takes that kind's defaults."
      (let ((bk:*blocks* nil)
            (bk:*kinds* nil))
        (bk:defkind clock ()
          (list :trigger 3 :body (list :color "#111111")))
        (bk:defblock clock)
        (is (= 3 (bk:bkblock-trigger (registered 'clock))))
        (is (string= "#111111"
                     (sp:swblock-color
                      (bk:bkblock-body (registered 'clock)))))))

(test defblock-config-overrides-kind-defaults
      "Keys and body given to defblock win over the kind's defaults."
      (let ((bk:*blocks* nil)
            (bk:*kinds* nil))
        (bk:defkind clock ()
          (list :interval 1 :body (list :color "#111111")))
        (bk:defblock clock :interval 9 :body (:color "#ffffff"))
        (is (= 9 (bk:bkblock-interval (registered 'clock))))
        (is (string= "#ffffff"
                     (sp:swblock-color
                      (bk:bkblock-body (registered 'clock)))))))

(test defblock-passes-extra-keys-to-kind
      "Keys that are not bkblock keys reach the kind's lambda list."
      (let ((bk:*blocks* nil)
            (bk:*kinds* nil))
        (bk:defkind label (&key text)
          (list :body (list :full-text text)))
        (bk:defblock label :text "hi")
        (is (string= "hi"
                     (sp:swblock-full-text
                      (bk:bkblock-body (registered 'label)))))))

(test defblock-kind-option-defaults
      "A kind option left out of defblock takes the kind's default."
      (let ((bk:*blocks* nil)
            (bk:*kinds* nil))
        (bk:defkind label (&key (text "none"))
          (list :body (list :full-text text)))
        (bk:defblock label)
        (is (string= "none"
                     (sp:swblock-full-text
                      (bk:bkblock-body (registered 'label)))))))

(test defblock-kind-key-allows-repeats
      "Two blocks of one kind keep their own names, text and bodies."
      (let ((bk:*blocks* nil)
            (bk:*kinds* nil))
        (bk:defkind label (&key text)
          (list :body (list :full-text text)))
        (bk:defblock guixos :kind label :text "GuixOS")
        (bk:defblock sep :kind label :text "|")
        (let ((a (bk:bkblock-body (registered 'guixos)))
              (b (bk:bkblock-body (registered 'sep))))
          (is (equal '("guixos" "sep")
                     (list (sp:swblock-name a) (sp:swblock-name b))))
          (is (equal '("GuixOS" "|")
                     (list (sp:swblock-full-text a)
                           (sp:swblock-full-text b))))
          (is (not (eq a b))))))

(test defblock-without-kind-is-plain
      "A block whose name is no kind is built from its own keys."
      (let ((bk:*blocks* nil)
            (bk:*kinds* nil))
        (bk:defblock custom :interval 2)
        (is (= 2 (bk:bkblock-interval (registered 'custom))))))

(test defblock-unknown-kind-errors
      "An unregistered :kind signals an error and registers nothing."
      (let ((bk:*blocks* nil)
            (bk:*kinds* nil))
        (signals error (bk:defblock tag :kind nope))
        (is (null bk:*blocks*))))

(test defblock-unknown-option-errors
      "An option no kind accepts signals an error."
      (let ((bk:*blocks* nil)
            (bk:*kinds* nil))
        (signals error (bk:defblock plain :bogus 1))
        (is (null bk:*blocks*))))

(test defblock-unknown-option-for-kind-errors
      "An option the kind's lambda list lacks signals an error."
      (let ((bk:*blocks* nil)
            (bk:*kinds* nil))
        (bk:defkind label (&key text)
          (list :body (list :full-text text)))
        (signals error (bk:defblock label :bogus 1))))
