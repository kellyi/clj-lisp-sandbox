(defpackage chapter-thirteen
  (:use :cl))

(declaim (inline single?))

(defun single? (lst)
  (and (consp lst) (null (cdr lst))))

(defun foo (x)
  (single? (bar x)))

(disassemble 'foo)
(disassemble 'single?)

(defun poly (a b x)
  (declare (fixnum a b x))
  (the fixnum (+ (* a (expt x 2)) (* b x))))

;; Exercise 2
(defun foo (x)
  (if (zerop x)
      0
      (+ 1 (foo (1- x)))))

(time (foo 10))

(defun foo* (x &optional (acc 0))
  (if (zerop x)
      acc
      (foo* (1- x) (1+ acc))))

(time (foo* 10))
