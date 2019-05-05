#|
  This file is a part of practical-common-lisp project.
|#

(defsystem "practical-common-lisp-test"
  :defsystem-depends-on ("prove-asdf")
  :author ""
  :license ""
  :depends-on ("practical-common-lisp"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "practical-common-lisp"))))
  :description "Test system for practical-common-lisp"

  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
