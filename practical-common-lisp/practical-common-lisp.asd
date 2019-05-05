#|
  This file is a part of practical-common-lisp project.
|#

(defsystem "practical-common-lisp"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "practical-common-lisp"))))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "practical-common-lisp-test"))))
