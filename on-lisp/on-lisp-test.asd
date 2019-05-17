#|
  This file is a part of on-lisp project.
|#

(defsystem "on-lisp-test"
  :defsystem-depends-on ("prove-asdf")
  :author ""
  :license ""
  :depends-on ("on-lisp"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "on-lisp"))))
  :description "Test system for on-lisp"

  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
