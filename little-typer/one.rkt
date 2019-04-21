#lang pie

;; frame 21
(cons 'ratatouille 'baguette)

;; frame 25
(cdr
 (cons 'ratatouille 'baguette))

;; frame 38
(car
 (cons 'ratatouille 'baguette))

;; frame 39
(cdr
 (cons 'ratatouille 'baguette))

;; frame 40
(car
 (cons
  (cons 'aubergine 'gourgette)
  'tomato))

;; frame 41
(car
 (cdr
  (cons 'ratouille
        (cons 'baguette 'olive-oil))))

;; frame 46

;; "If two expressions are the same according to their type, then they have
;; identical normal forms. ... we can check whether two expressions are the
;; same by comparing their normal forms"

;; frame 47
(car
 (cons
  (cons 'aubergine 'courgette)
  'tomato))

;; frame 56

;; "Every expression that is a type has a normal form, which is the most direct
;; way of writing that type. If two expressions are the same type, then they
;; have identical normal forms, and if two types have identical normal forms,
;; then they are the same type."

;; frame 63
(natural? 1)

;; frame 64
(natural? 1729)

;; frame 65
(natural? -1)

;; frame 66
(natural? -23)

;; frame 77
(claim zero
       Nat)
(define zero
  0)

(claim one
       Nat)
(define one
  (add1 zero))

;; frame 79
(claim two
       Nat)
(define two
  (add1 one))

;; frame 81
(claim four
       Nat)
(define four
  (add1
   (add1
    (add1
     (add1 zero)))))

;; frame 88
(+ (add1
    (add1 zero))
   (add1 zero))

;; frame 91
(add1
 (+ (add1 zero)
    (add1
     (add1 zero))))

(add1
 (+ (add1 zero) (add1 one)))

;; frame 98
(add1
 (+ (add1 zero)
    (add1
     (add1 zero))))

;; frame 102
(add1 (+ 0 1))
(add1 (+ 1 0))

;; frame 110
(car
 (cons (+ 3 5) 'baguette))

;; frame 120
(cons 'basil
      (cons 'thyme 'oregano))

