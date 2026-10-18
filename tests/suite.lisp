;;;; SPDX-License-Identifier: LGPL-2.1-only WITH LLGPL
;;;; Copyright (C) 2026 Erik P Almaraz
;;;;
;;;; Licensed under the LGPL-2.1-only with the LLGPL preamble;
;;;; see the LICENSE file in this repository.

(defpackage #:barker/tests/suite
  (:use #:cl #:5am)
  (:export #:suite)
  (:documentation "Root test suite for barker."))

(in-package #:barker/tests/suite)


(def-suite suite
  :description "All barker unit tests.")
