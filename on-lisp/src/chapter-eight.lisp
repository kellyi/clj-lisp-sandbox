(defpackage chapter-eight
  (:use :cl))

(defun 1+-fn (x) (+ 1 x))

(defmacro 1+-mac (x) `(+ 1 ,x))

;; macros can do two things that functions can't:
;;
;; - they can control or prevent evaluation of their arguments
;; - they are expanded right into the calling context

;; "Any operator that needs access to its parameters before they are evaluated
;; should be written as a macro, because there is no other choice."

(defun avg-fn (&rest args)
  (/ (apply #'+ args) (length args)))

(defmacro avg-mac (&rest args)
  `(/ (+ ,@args) ,(length args)))

(defmacro our-defun (name parms &body body)
  `(progn
     (setf (symbol-function ',name)
           #'(lambda ,parms (block ,name ,@body)))
     ',name))

(our-defun increment (x) (1+ x))
(increment 1) ;; -> 2
