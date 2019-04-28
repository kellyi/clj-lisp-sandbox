(defpackage nineteen
  (:use :cl))
(in-package :seasoned-schemer)

(defun deep-prime (m)
  (cond
    ((zero? m) 'pizza)
    (else (cons (deep-prime (sub1 m))
                '()))))

(deep-prime 6)

(defun six-layers (p)
  (cons
   (cons
    (cons
     (cons
      (cons
       (cons p '())
       '())
      '())
     '())
    '())
   '()))

(six-layers 'pizza)

(defun four-layers (p)
  (cons
   (cons
    (cons
     (cons p '())
     '())
    '())
   '()))

(six-layers 'pizza)
(four-layers 'pizza)

(defparameter toppings nil)

(defun deep&co (m k)
  (cond
    ((zero? m) (funcall k 'pizza))
    (else
     (deep&co (sub1 m)
              (lambda (x)
                (funcall k (cons x '())))))))

(deep&co 0 (lambda (x) x))

(deep&co 6 (lambda (x) x))

(defun deep&coB (m k)
  (cond
    ((zero? m) (progn
                 (setq toppings k)
                 (funcall k 'pizza)))
    (else (deep&coB (sub1 m)
                    (lambda (x)
                      (funcall k (cons x '())))))))

(deep&coB 2 (lambda (x) x))
toppings

(deep&coB 6 (lambda (x) x))
toppings

(funcall toppings 'cake)

(cons (funcall toppings 'cake)
      (funcall toppings 'cake))

(cons (funcall toppings 'cake)
      (cons (funcall toppings 'mozzarella)
            (cons (funcall toppings 'pizza)
                  '())))
