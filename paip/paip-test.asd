#|
  This file is a part of paip project.
|#

(defsystem "paip-test"
  :defsystem-depends-on ("prove-asdf")
  :author ""
  :license ""
  :depends-on ("paip"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "paip"))))
  :description "Test system for paip"

  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
