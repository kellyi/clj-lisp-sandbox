(defpackage fourteen
  (:use :cl))
(in-package :seasoned-schemer)

(defun leftmost-initial (l)
  "Get the leftmost atom from a non-empty list of s-expressions."
  (cond
    ((atom? (car l)) (car l))
    (else (leftmost-initial (car l)))))

(leftmost-initial '(((a) ()) () (e)))

;; (leftmost-initial '(((() a) ()))) -> won't work

(defun leftmost (l)
  "Get the leftmost atom from a list of s-expressions."
  (cond
    ((null? l) '())
    ((atom? (car l)) (car l))
    (else (cond
            ((atom? (leftmost (car l))) (leftmost (car l)))
            (else (leftmost (cdr l)))))))

(leftmost '(((a) ()) () (e)))
(leftmost '(((() a) ())))

(defun leftmost-prime (l)
  "Get the leftmost atom from a list of s-expressions."
  (cond
    ((null? l) '())
    ((atom? (car l)) (car l))
    (else (let ((a (leftmost-prime (car l))))
            (cond
              ((atom? a) a)
              (else (leftmost-prime (cdr l))))))))

(leftmost-prime '(((a) ()) () (e)))
(leftmost-prime '(((() a) ())))

(defun rember1* (a l)
  "Remove atoms a from list l."
  (labels ((R (l)
             (cond
               ((null? l) '())
               ((atom? (car l)) (cond
                                  ((eq? (car l) a) (cdr l))
                                  (else (cons (car l)
                                              (R (cdr l))))))
               (else (let ((av (R (car l))))
                       (cond
                         ((eqlist? (car l) av) (cons (car l) (R (cdr l))))
                         (else (cons av (cdr l)))))))))
    (R l)))

(rember1* 'salad '((Swedish rye)
                   (French (mustard salad turkey))
                   (salad)))

(rember1* 'meat '((pasta meat)
                  pasta
                  (noodles meat sauce)
                  meat tomatoes))

(defun depth* (l)
  "Find the depth of a nested list."
  (cond
    ((null? l) 1)
    ((atom? (car l)) (depth* (cdr l)))
    (else
     (let ((a (add1 (depth* (car l))))
           (d (depth* (cdr l))))
       (cond
         ((> d a) d)
         (else a))))))

(depth* '((pickled) peppers (peppers pickled)))

(depth* '(margarine
          ((bitter butter)
           (makes)
           (batter (bitter)))
          butter))

(defun depth-prime* (l)
  "Find the depth of a nested list."
  (cond
    ((null? l) 1)
    ((atom? (car l)) (depth-prime* (cdr l)))
    (else (max
           (add1 (depth-prime* (car l)))
           (depth-prime* (cdr l))))))

(depth-prime* '((pickled) peppers (peppers pickled)))

(depth-prime* '(margarine
                ((bitter butter)
                 (makes)
                 (batter (bitter)))
                butter))

(defun leftmost-prime-prime (l)
  "Find the leftmost atom in a list of s-expressions."
  (catch 'skip
    (labels ((lm (l)
               (cond
                 ((null? l) '())
                 ((atom? (car l)) (throw 'skip (car l)))
                 (else (progn
                         (lm (car l))
                         (lm (cdr l)))))))
      (lm l))))

(leftmost-prime-prime '(((a)) b (c)))
