(defpackage streams
  (:use :cl))

;; adapted from Paradigms of AI Programming, chapter 9
(defstruct delay (value nil) (function nil))

(defmacro delay (&rest body)
  "A computation that can be executed later."
  `(make-delay :function #'(lambda () . ,body)))

(defun force (x)
  "Find the value of x, by computing it if it is a delay."
  (if (not (delay-p x))
      x
      (progn
        (when (delay-function x)
          (setf (delay-value x)
                (funcall (delay-function x)))
          (setf (delay-function x) nil))
        (delay-value x))))

(defun $cons (x y)
  "Lazily cons x & y; from the streams chapter of Purely Functional Data Structures"
  (cons (delay x) (delay y)))

(let ((x (cons
          (delay 1)
          (delay '(1 2 3)))))
  (prog2
    (format t "~a~%" (car x))
    (force (car x))
    (format t "~a~%" (car x))
    (format t "~a~%" (cdr x))
    (force (cdr x))
    (format t "~a~%" (cdr x))))

(defun lazy-append (l elem)
  "Lazily append an element elem to list l."
  (if (null l)
      elem
      ($cons (car l) (lazy-append (cdr l) elem))))

(defun lazy-take (n l)
  "Lazily take n elements from list l."
  (cond
    ((zerop n) (delay nil))
    ((null l) (delay nil))
    (t ($cons (car l) (lazy-take (1- n) (cdr l))))))

(defun lazy-drop (n l)
  "Lazily drop n elements from list l."
  (cond
    ((zerop n) l)
    ((null l) nil)
    (t (lazy-drop (1- n) (cdr (force car l))))))
