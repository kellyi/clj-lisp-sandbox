#|
  This file is a part of let-over-lambda project.
|#

(defsystem "let-over-lambda"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "let-over-lambda"))))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "let-over-lambda-test"))))
