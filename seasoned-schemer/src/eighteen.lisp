(defpackage eighteen
  (:use :cl))
(in-package :seasoned-schemer)

;; (defun kar (c)
;;   (funcall c (lambda (a d) a)))

;; (defparameter kar-identity (kar #'identity))

;; (funcall #'kar-identity '(a z))

(defun adder-with-selector (c)
  (funcall c (lambda (x) (+ x 1))))

(defparameter id-adder (adder-with-selector #'identity))

(funcall id-adder 10)

(defun kar (c)
  (funcall c (lambda (a d) a)))

(defparameter id-kar (kar #'identity))

(funcall id-kar 1 2)

(defun kdr (c)
  (funcall c (lambda (a d) d)))

(defparameter id-kdr (kdr #'identity))

(funcall id-kdr 1 2)

(defun kons (kar kdr)
  (lambda (selector)
    (funcall selector kar kdr)))

(defun lots (m)
  (cond
    ((zero? m) '())
    (else (cons 'egg
                (lots (sub1 m))))))

(lots 3)

(defun lenkth (l)
  (cond
    ((null? l) 0)
    (else (add1 (lenkth (cdr l))))))

(lenkth (lots 5))

(lenkth (lots 15))
