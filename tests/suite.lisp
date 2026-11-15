(defpackage #:barker/tests/suite
  (:use #:cl #:5am)
  (:export #:suite)
  (:documentation "Root test suite for barker."))

(in-package #:barker/tests/suite)


(def-suite suite
  :description "All barker unit tests.")
