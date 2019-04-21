#lang pie

;; "`car` is an eliminator; eliminators take apart values built by constructors"

;; "Applying a function to arguments is the eliminator for functions."

(lambda (flavor)
  (cons flavor 'lentils))

((lambda (flavor)
   (cons flavor 'lentils))
 'garlic)

((lambda (root)
   (cons root
         (cons (+ 1 2) root)))
 'potato)

((lambda (root)
   (cons root
         (lambda (root)
           root)))
 'carrot)

(lambda (y)
  (car
   (cons y y)))

(lambda (x)
  x)

'(lambda (x)
  (car x))

'(lambda (y)
  (car y))

(lambda (a d)
  (cons a d))

(claim vegetables
       (Pair Atom Atom))
(define vegetables
  (cons 'celery 'carrot))

(cons (car vegetables)
      (cdr vegetables))

(claim five
       Nat)
(define five
  (+ 7 2))

(which-Nat zero 'naught (lambda (n) 'more))

(which-Nat 5
           0
           (lambda (n)
             (+ 6 n)))

(claim gauss (-> Nat Nat))

(claim forever (-> Nat Atom))
(define forever
  (lambda (and-ever)
    (forever and-ever)))

(claim Pear U)
(define Pear
  (Pair Nat Nat))

(claim Pear-maker U)
(define Pear-maker
  (-> Nat Nat Pear))

(claim elim-Pear
       (-> (Pair Nat Nat)
           (-> Nat Nat
               (Pair Nat Nat))
           (Pair Nat Nat)))

(claim pearwise+
       (-> Pear Pear Pear))
(define pearwise+
  (lambda (anjou bosc)
    (elim-Pear anjou
               (lambda (a1 d1)
                 (elim-Pear bosc
                            (lambda (a2 d2)
                              (cons
                               (+ a1 a2)
                               (+ d1 d2))))))))
