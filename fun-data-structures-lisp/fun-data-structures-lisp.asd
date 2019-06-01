#|
  This file is a part of fun-data-structures-lisp project.
|#

(defsystem "fun-data-structures-lisp"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "fun-data-structures-lisp"))))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "fun-data-structures-lisp-test"))))
