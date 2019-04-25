(defpackage twelve
  (:use :cl))
(in-package :seasoned-schemer)

(defun id (b)
  "Identity function."
  b)

(id 1)
(id 'one)

(defun add-one (x)
  "Try out using labels keyword."
  (labels ((add-one-prime (x)
             (add1 x)))
    (add-one-prime x)))

(add-one 10)

(defun multirember (a lat)
  "Remove all instances of atom a from list lat."
  (labels ((mr (lat)
             (cond
               ((null? lat) '())
               ((eq a (car lat)) (mr (cdr lat)))
               (t (cons (car lat)
                        (mr (cdr lat)))))))
    (mr lat)))

(multirember 'tuna '(shrimp salad tuna salad and tuna))

(defun adder (n)
  "Trying out defining nested lambda expressions."
  #'(lambda (x) (+ x n)))

(funcall (adder 2) 1)
(apply (adder 2) '(1))

(defun rember-f-a? (test?)
  "Remove atom a from list l if test? is true."
  #'(lambda (a l)
      (cond
        ((null? l) '())
        ((test? (car l) a) (cdr l))
        (t (funcall (rember-f-a? test?)
                    a
                    (cdr l))))))

(defun multirember-f-a? (test?)
  "Remove all atoms a from list l if test? is true."
  #'(lambda (a lat)
      (cond
        ((null? lat) '())
        ((test? (car lat) a) (funcall (multirember-f-a? test?)
                                      a
                                      (cdr lat)))
        (t (cons (car lat)
                 (funcall (multirember-f-a? test?)
                          a
                          (cdr lat)))))))

(defun member-prime? (a lat)
  "Check whether a is a member of lat."
  (labels ((yes? (l)
             (cond
               ((null? l) f)
               ((eq? (car l) a) t)
               (t (yes? (cdr l))))))
    (yes? lat)))

(member-prime? 'hello '(hello world))

(member-prime? 'hello '(foo bar baz))

(defun union-prime? (set1 set2)
  "Return the union of set1 and set2."
  (labels ((U (set)
             (cond
               ((null? set) set2)
               ((member-prime? (car set) set2) (U (cdr set)))
               (t (cons (car set)
                        (U (cdr set)))))))
    (U set1)))

(union-prime? '(hello world) '(foo bar baz))

(union-prime? '(foo bar baz) '(foo bar baz quux))

(defun two-in-a-row-prime? (lat)
  "Check whether a list lat contains two consecutive identical elements."
  (labels ((W (a lat)
             (cond
               ((null? lat) f)
               (t (or (eq? (car lat) a)
                      (W (car lat)
                         (cdr lat)))))))
    (cond
      ((null? lat) f)
      (t (W (car lat) (cdr lat))))))

(two-in-a-row-prime? '(1 2 3 4 5))

(two-in-a-row-prime? '(1 2 3 4 3 3 4 5))

(defun sum-of-prefixes-prime (tup)
  "Return a list of intermediate results from reducing a list of natural ints to a sum."
  (labels ((S (sss tup)
             (cond
               ((null? tup) '())
               (t (cons (+ sss (car tup))
                        (S (+ sss (car tup))
                           (cdr tup)))))))
    (S 0 tup)))

(sum-of-prefixes-prime '(2 1 9 17 0))
(sum-of-prefixes-prime '(1 1 1 1 1))
