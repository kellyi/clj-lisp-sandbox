(defpackage eleven
  (:use :cl))
(in-package :seasoned-schemer)

(defun member? (a lat)
  "Check whether a is a member of lat."
  (cond
    ((null? lat) f)
    (t (or (eq a (car lat))
           (member? a (cdr lat))))))

(member? 'sardines '(Italian sardines spaghetti parsley))
(member? 'oregano '(Italian sardines spaghetti parsley))

(defun is-first-initial? (a lat)
  "Check whether a is equal to (car lat)."
  (cond
    ((null? lat) f)
    (t (eq (car lat) a))))

(is-first-initial? 'sardines '(Italian sardines))
(is-first-initial? 'sardines '(sardines spaghetti))

(defun two-in-a-row-initial? (lat)
  "Check whether a list lat has two identical consecutive elements."
  (cond
    ((null? lat) f)
    (t (or (is-first-initial? (car lat) (cdr lat))
           (two-in-a-row-initial? (cdr lat))))))

(two-in-a-row-initial? '(Italian sardines sardines spaghetti parsley))
(two-in-a-row-initial? '(Italian sardines and more sardines spaghetti))

(defun is-first-b? (a lat)
  "Check whether a is equal to (car lat)."
  (cond
    ((null? lat) f)
    (t (or (eq (car lat) a)
           (two-in-a-row-initial? lat)))))

(defun two-in-a-row-b? (preceding lat)
  "Check whether a list lat has two identical consecutive elements or if
  (car lat) is the same element as preceding."
  (cond
    ((null? lat) f)
    (t (or (eq (car lat) preceding)
           (two-in-a-row-b? (car lat) (cdr lat))))))

(defun two-in-a-row? (lat)
  "Check whether a list lat has two identical consecutive elements."
  (cond
    ((null? lat) f)
    (t (two-in-a-row-b? (car lat)
                        (cdr lat)))))

(two-in-a-row? '(Italian sardines sardines spaghetti parsley))
(two-in-a-row? '(Italian sardines and more sardines spaghetti))
(two-in-a-row? '(1 2 3 4 5 5))
(two-in-a-row? '(b d e i i a g))

(defun sum-of-prefixes-b (sonssf tup)
  "Create a list of intermediate accumulators when reducing a list to a sum."
  (cond
    ((null? tup) '())
    (t (cons (+ sonssf (car tup))
             (sum-of-prefixes-b
              (+ sonssf (car tup))
              (cdr tup))))))

(defun sum-of-prefixes (tup)
  "Create a list of intermediate accumulators when reducing a list to a sum."
  (sum-of-prefixes-b 0 tup))

(sum-of-prefixes '(2 1 9 17 0))
(sum-of-prefixes '(1 1 1 1 1))

(defun one? (n)
  "Is n equal to one?"
  (= 1 n))

(defun pick (n lat)
  "Pick the n-th element from a list lat."
  (cond
    ((one? n) (car lat))
    (t (pick (sub1 n) (cdr lat)))))

(pick 4 '(4 3 1 1 1))
(pick 2 '(2 4 3 1 1 1))

(defun scramble-b (tup rev-pre)
  (cond
    ((null? tup) '())
    (t (cons (pick (car tup)
                   (cons (car tup) rev-pre))
             (scramble-b (cdr tup)
                         (cons (car tup) rev-pre))))))

(defun scramble (tup)
  "Scramble list tup."
  (scramble-b tup '()))


(scramble '(1 1 1 3 4 2 1 1 9 2))
(scramble '(1 2 3 4 5 6 7 8 9))
(scramble '(1 2 3 1 2 3 4 1 8 2 10))
