#|
  This file is a part of ansi-common-lisp project.
|#

(defsystem "ansi-common-lisp"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "ansi-common-lisp"))))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "ansi-common-lisp-test"))))
