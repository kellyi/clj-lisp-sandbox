#|
  This file is a part of fun-data-structures-lisp project.
|#

(defsystem "fun-data-structures-lisp-test"
  :defsystem-depends-on ("prove-asdf")
  :author ""
  :license ""
  :depends-on ("fun-data-structures-lisp"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "fun-data-structures-lisp"))))
  :description "Test system for fun-data-structures-lisp"

  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
