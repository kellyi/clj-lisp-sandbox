#|
  This file is a part of let-over-lambda project.
|#

(defsystem "let-over-lambda-test"
  :defsystem-depends-on ("prove-asdf")
  :author ""
  :license ""
  :depends-on ("let-over-lambda"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "let-over-lambda"))))
  :description "Test system for let-over-lambda"

  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
