(defpackage lowlevelefficiency
  (:use :cl))
(in-package :paip)

(defun square (x) (* x x))

(defun sum-squares (vect)
  (declare (type (simple-array fixnum *) vect)
           (inline square) (optimize speed (safety 0)))
  (let ((sum 0))
    (declare (fixnum sum))
    (dotimes (i (length vect))
      (declare (fixnum i))
      (incf sum (the fixnum (square (svref vect i)))))
    sum))

;; (sum-squares #(1 2 3 4 5))

(defun f (x y)
  (declare (fixnum x y) (optimize (safety 0) (speed 3)))
  (the fixnum (+ x y)))

(defun g (x y) (+ x y))

(disassemble 'f)

(disassemble 'g)

(defun reg (a b c d) (list a b c d))

(defun rst (a b c &rest d) (list* a b c d))

(defun opt (&optional a b (c 1) (d (sqrt a))) (list a b c d))

;; (defun key (&key a b (c 1) (d (sqrt a))) (list a b c d)

(disassemble 'reg)

(disassemble 'rst)

(disassemble 'opt)
