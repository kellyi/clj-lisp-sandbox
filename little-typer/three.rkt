#lang pie

(claim step-+
       (-> Nat
           Nat))
(define step-+
  (lambda (+n-1)
    (add1 +n-1)))

(claim +
       (-> Nat Nat
           Nat))
(define +
  (lambda (n j)
    (iter-Nat n
              j
              step-+)))

(claim step-zerop
       (-> Nat Atom
           Atom))
(define step-zerop
  (lambda (n-1 zerop-n-1)
    'nil))

(claim zerop
       (-> Nat
           Atom))
(define zerop
  (lambda (n)
    (rec-Nat n
             't
             step-zerop)))

(claim step-gauss
       (-> Nat Nat
           Nat))
(define step-gauss
  (lambda (n-1 gauss-n-1)
    (+ (add1 n-1) gauss-n-1)))

(claim gauss
       (-> Nat
           Nat))
(define gauss
  (lambda (n)
    (rec-Nat n
             0
             step-gauss)))

(claim step-*
       (-> Nat Nat Nat
           Nat))
(define step-*
  (lambda (j n-1 *n-1)
    (+ j *n-1)))

(claim *
       (-> Nat Nat
           Nat))
(define *
  (lambda (n j)
    (rec-Nat n
             0
             (step-* j))))
