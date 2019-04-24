#lang pie

(claim elim-Pair
       (Pi ((A U)
            (D U)
            (X U))
           (-> (Pair A D)
               (-> A D
                   X)
               X)))
(define elim-Pair
  (lambda (A D X)
    (lambda (p f)
      (f (car p) (cdr p)))))

(claim kar
       (-> (Pair Nat Nat)
           Nat))
(define kar
  (lambda (p)
    (elim-Pair
     Nat Nat
     Nat
     p
     (lambda (a _d)
       a))))

(claim kdr
       (-> (Pair Nat Nat)
           Nat))
(define kdr
  (lambda (p)
    (elim-Pair
     Nat Nat
     Nat
     p
     (lambda (_a d)
       d))))

(claim swap
       (-> (Pair Nat Atom)
           (Pair Atom Nat)))
(define swap
  (lambda (p)
    (elim-Pair
     Nat Atom
     (Pair Atom Nat)
     p
     (lambda (a d)
       (cons d a)))))

(claim flip
       (Pi ((A U)
            (D U))
           (-> (Pair A D)
               (Pair D A))))
(define flip
  (lambda (A D)
    (lambda (p)
      (cons (cdr p) (car p)))))

(claim twin-Nat
       (-> Nat
           (Pair Nat Nat)))
(define twin-Nat
  (lambda (x)
    (cons x x)))

(claim twin-Atom
       (-> Atom
           (Pair Atom Atom)))
(define twin-Atom
  (lambda (x)
    (cons x x)))

(claim twin
       (Pi ((Y U)) ;; Pi used to build type abstractions
           (-> Y
               (Pair Y Y))))
(define twin
  (lambda (Y)
    (lambda (x)
      (cons x x))))

(claim twin-Atom-prime
       (-> Atom
           (Pair Atom Atom)))
(define twin-Atom-prime
  (twin Atom)) ;; curries fn with type
