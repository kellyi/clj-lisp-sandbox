#|
  This file is a part of ansi-common-lisp project.
|#

(defsystem "ansi-common-lisp-test"
  :defsystem-depends-on ("prove-asdf")
  :author ""
  :license ""
  :depends-on ("ansi-common-lisp"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "ansi-common-lisp"))))
  :description "Test system for ansi-common-lisp"

  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
