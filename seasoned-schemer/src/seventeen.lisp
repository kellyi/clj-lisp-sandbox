(defpackage seventeen
  (:use :cl))
(in-package :seasoned-schemer)

(defun consC (x y)
  (let ((N 0))
    (setq N (add1 N))
    (cons x y)))

(consC 1 2)

(defun deep-consC (m)
  (if (zero? m)
      'pizza
      (consC (deep-consC (sub1 m))
             '())))

(deep-consC 5)

(defparameter counter nil)
(defparameter N 0)

(defun consC-prime (x y)
  (setq counter (lambda () N))
  (setq N (add1 N))
  (cons x y))

(consC-prime 1 2)

(funcall counter)

(defun deep-consC-prime (m)
  (if (zero? m)
      'pizza
      (consC-prime (deep-consC-prime (sub1 m))
                   '())))

(deep-consC-prime 5)

(funcall counter)

(deep-consC-prime 7)

(funcall counter)

;; (defun supercounter (fn)
;;   (labels ((S (n)
;;              (if (zero? n) (funcall fn n)
;;                  (progn
;;                    (funcall fn n)
;;                    (S (sub1 n))))))
;;     (S 1000)
;;     (funcall counter)))

;; (supercounter #'deep-consC-prime)

(defparameter counter-prime nil)
(defparameter set-counter-prime nil)
(defparameter N-prime 0)

(defun consC-prime-prime (x y)
  (setq counter-prime (lambda () N))
  (setq set-counter-prime (lambda (x) (setq N x)))
  (setq N (add1 N))
  (cons x y))

(consC-prime-prime 1 2)

(funcall counter-prime)
(funcall set-counter-prime 20)
(funcall counter-prime)
