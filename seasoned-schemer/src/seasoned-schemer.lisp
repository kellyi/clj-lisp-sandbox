(defpackage seasoned-schemer
  (:use :cl))
(in-package :seasoned-schemer)

(defun atom? (x)
  (not (listp x)))

(defun natural-int-p (x)
  "Check whether x is a natural integer."
  (cond
    ((not (numberp x)) nil)
    ((< x 0) nil)
    (t t)))

(defun add1 (x)
  "Return the next natural integer greater than x if x is a natural integer."
  (cond
    ((not (natural-int-p x)) nil)
    (t (1+ x))))

(defun sub1 (x)
  "Return the next natural integer less than x if x is a natural integer."
  (cond
    ((not (natural-int-p x)) nil)
    ((zerop x) nil)
    (t (1- x))))

;; from https://stackoverflow.com/a/21075628/3397606
(defmacro defalias (new-name prev-name)
  `(defmacro ,new-name (&rest args)
     `(,',prev-name ,@args)))

(defalias null? null)
(defconstant f nil)
