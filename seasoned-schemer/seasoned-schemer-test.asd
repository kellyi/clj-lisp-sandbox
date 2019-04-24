#|
  This file is a part of seasoned-schemer project.
|#

(defsystem "seasoned-schemer-test"
  :defsystem-depends-on ("prove-asdf")
  :author ""
  :license ""
  :depends-on ("seasoned-schemer"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "seasoned-schemer"))))
  :description "Test system for seasoned-schemer"

  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
