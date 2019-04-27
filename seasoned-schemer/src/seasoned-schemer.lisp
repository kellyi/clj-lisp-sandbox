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
(defalias eq? eq)
(defconstant f nil)
(defconstant else t)

(defun eqlist? (l1 l2)
  "Is l1 the same list as l2?"
  (cond
    ((and (null? l1) (null? l2)) t)
    ((and (atom? (car l1))
          (atom? (car l2))
          (eq? (car l1) (car l2))) (eqlist? (cdr l1) (cdr l2)))
    ((or (atom? (car l1))
         (atom? (car l2))) f)
    (else (and (eqlist? (car l1) (car l2))
               (eqlist? (cdr l1) (cdr l2))))))

(defalias zero? zerop)

(defun member? (a lat)
  "Check whether a is a member of lat."
  (labels ((yes? (l)
             (cond
               ((null? l) f)
               ((eq? (car l) a) t)
               (t (yes? (cdr l))))))
    (yes? lat)))
