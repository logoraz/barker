(defsystem "barker"
  :description "A hackable swaybar generator, configured in Common Lisp."
  :author "Erik P. Almaraz"
  :license "LGPL-2.1-only WITH LLGPL"
  :version (:read-file-form "data/version.sexp" :at (0 1))
  :depends-on ("sb-posix"
               "sb-concurrency"
               "bordeaux-threads"
               "trivial-gray-streams"
               "com.inuoe.jzon")
  :components ((:module "lib"
                :pathname "code/lib"
                :components ((:file "syntax")
                             (:file "platform")
                             (:file "jzon-ext")))
               (:module "base"
                :pathname "code/base"
                :depends-on ("lib")
                :components ((:file "swaybar-protocol")
                             (:file "bkblock" :depends-on ("swaybar-protocol"))))
               (:module "blocks"
                :pathname "code/blocks"
                :depends-on ("lib" "base")
                :components ((:file "support")
                             (:file "label")
                             (:file "clock" :depends-on ("support"))
                             (:file "battery" :depends-on ("support"))
                             (:file "volume" :depends-on ("support"))
                             (:file "brightness" :depends-on ("support"))
                             (:file "network-manager" :depends-on ("support"))
                             (:file "xkb-layout" :depends-on ("support"))))
               (:module "interface"
                :pathname "code"
                :depends-on ("lib" "base" "blocks")
                :components ((:file "listeners")
                             (:file "barker" :depends-on ("listeners")))))
  :in-order-to ((test-op (test-op "barker/tests")))
  :build-operation "program-op"
  :build-pathname "barker"
  :entry-point "barker:main")


;;; Subsystems

(defsystem "barker/tests"
  :description "Unit Tests."
  :depends-on ("barker" "fiveam" "sb-posix")
  :components
  ((:module "tests"
    :components
    ((:file "suite")
     (:file "lib" :depends-on ("suite"))
     (:file "base" :depends-on ("suite"))
     (:file "interface" :depends-on ("suite"))
     (:file "blocks" :depends-on ("suite")))))
  :perform (test-op (o c) (symbol-call :fiveam :run!
                                       (find-symbol "SUITE"
                                                    :barker/tests/suite))))
