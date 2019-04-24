#|
  This file is a part of seasoned-schemer project.
|#

(defsystem "seasoned-schemer"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "seasoned-schemer"))))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "seasoned-schemer-test"))))
