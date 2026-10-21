;;; SPDX-License-Identifier: LGPL-2.1-only WITH LLGPL
;;; Copyright (C) 2026 Erik P Almaraz
;;;
;;; Development recipe: builds barker from this checkout.
;;;
;;;   guix build -f guix.scm      ; build and run the tests
;;;   guix shell -D -f guix.scm   ; development environment
;;;   guix shell -f guix.scm      ; try the built barker

(use-modules (gnu packages lisp-xyz)
             (gnu packages lisp-check)
             (guix build-system asdf)
             (guix gexp)
             (guix git)
             (guix packages)
             (guix utils)
             ((guix licenses) #:prefix license:))

(define %source-dir
  (dirname (current-filename)))

;;; The last expression is the package `guix build -f` builds.
(package
  (name "barker")
  (version "0.1.0-dev")
  (source
   (local-file %source-dir "barker-checkout"
               #:recursive? #t
               #:select? (git-predicate %source-dir)))
  (build-system asdf-build-system/sbcl)
  (arguments
   (list
    #:asd-systems ''("barker")
    #:asd-test-systems ''("barker/tests")
    #:phases
    #~(modify-phases %standard-phases
        ;; Dump a standalone image, entry point (barker:main).
        (add-after 'create-asdf-configuration 'build-program
          (lambda* (#:key outputs #:allow-other-keys)
            (build-program
             (string-append (assoc-ref outputs "out") "/bin/barker")
             outputs
             #:dependencies '("barker")
             #:entry-program '((barker:main))
             #:compress? #t))))))
  (native-inputs
   (list sbcl-fiveam))
  (inputs
   (list sbcl-bordeaux-threads
         sbcl-jzon
         sbcl-trivial-gray-streams))
  (home-page "https://codeberg.org/logoraz/barker")
  (synopsis "Swaybar status generator configured in Common Lisp")
  (description
   "Barker generates the status line for swaybar using the swaybar
protocol.  The bar is described in Common Lisp through a small macro DSL,
with blocks for battery, brightness, clock, network-manager, volume,
keyboard layout, and plain labels.")
  (license license:lgpl2.1))
