(defpackage efficiency
  (:use :cl))
(in-package :paip)

(+ 1 2)

(defun fib (n)
  "Compute the nth number in the Fibonacci sequence."
  (if (<= n 1) 1
      (+ (fib (- n 1)) (fib (- n 2)))))

(fib 20)

(defun memo (fn)
  "Return a memo-function of fn."
  (let ((table (make-hash-table)))
    #'(lambda (x)
        (multiple-value-bind (val found-p)
            (gethash x table)
          (if found-p
              val
              (setf (gethash x table) (funcall fn x)))))))

;; (setq memo-fib (memo #'fib))

;; (funcall memo-fib 3)

(defun memoize (fn-name)
  "Replace fn-name's global definition with a memoized version."
  (setf (symbol-function fn-name) (memo (symbol-function fn-name))))

(memoize 'fib)

(fib 40)

;; (defmacro defun-memo (fn args &body body)
;;   "Define a memoized function."
;;   `(memoize (defun ,fn ,args . ,body)))

(defun rule-lhs (rule)
  "The lefthand side of a rule."
  (first rule))

(defun rule-rhs (rule)
  "The righthand side of a rule."
  (rest (rest rule)))

(defun one-of (set)
  "Pick one element of a set, and make a list of it."
  (list (random-elt set)))

(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))

(defun compile-rule (rule)
  "Translate a grammar rule into a Lisp function definition."
  (let ((rhs (rule-rhs rule)))
    `(defun ,(rule-lhs rule) ()
       ,(cond ((every #'atom rhs) `(one-of `,rhs))
              ((length=1 rhs) (build-code (first rhs)))
              (t `(case (random ,(length rhs))
                    ,@(build-cases 0 rhs)))))))

(defun length=1 (x)
  "Is x a list of length 1?"
  (and (consp x) (null (rest x))))

(defun build-code (choice)
  "Append together multiple constituents."
  (cond ((null choice) nil)
        ((atom choice) (list choice))
        (t `(append ,@(mapcar #'build-code choice)))))

(defun build-cases (number choices)
  "Return a list of case-clauses."
  (when choices
    (cons (list number (build-code (first choices)))
          (build-cases (+ 1 number) (rest choices)))))

(defmacro defrule (&rest rule)
  "Define a grammar rule"
  (compile-rule rule))

(defrule Sentence -> (NP VP))
(defrule NP -> (Art Noun))
(defrule VP -> (Verb NP))
(defrule Art -> the a)
(defrule Noun -> man ball cat table)
(defrule Verb -> saw liked took ate)

(compile-rule '(Sentence -> (NP VP)))

(compile-rule '(Noun -> man ball cat table))

(macroexpand '(defrule Adj* -> () Adj (Adj Adj*)))

(macroexpand '(defrule Noun -> man ball cat table (chow chow)))

(defstruct delay (value nil) (function nil))

(defmacro delay (&rest body)
  "A computation that can be executed later by FORCE."
  `(make-delay :function #'(lambda () . ,body)))

(defun force (x)
  "Find the value of x, by computing it if it is a delay."
  (if (not (delay-p x))
      x
      (progn
        (when (delay-function x)
          (setf (delay-value x)
                (funcall (delay-function x)))
          (setf (delay-function x) nil))
        (delay-value x))))

(defparameter x (list (print 1) (delay (print 2))))

(force (second x))

x

(force (second x))

(defmacro make-pipe (head tail)
  "Create a pipe by evaluating head and delaying tail."
  `(cons ,head #'(lambda () ,tail)))

(defconstant empty-pipe nil)

(defun head (pipe) (first pipe))

(defun tail (pipe)
  "Return tail of pipe or list, and destructively update the tail if it is a function."
  (if (functionp (rest pipe))
      (setf (rest pipe) (funcall (rest pipe)))
      (rest pipe)))

(defun pipe-elt (pipe i)
  "The i-th element of a pipe, 0-based."
  (if (= i 0)
      (head pipe)
      (pipe-elt (tail pipe) (- i 1))))

(defun integers (&optional (start 0) end)
  "A pipe of integers from START to END.
  If END is nil, this is an infinite pipe."
  (if (or (null end) (<= start end))
      (make-pipe start (integers (+ start 1) end))
      nil))

;; (defparameter c (integers 0))

;; (pipe-elt c 0)

;; (pipe-elt c 5)

;; c

(defparameter c-prime (integers 0))

(pipe-elt c-prime 0)

(pipe-elt c-prime 5)

c-prime

(defparameter i (integers 0 10))

(pipe-elt i 10)

(pipe-elt i 11)

i

(defun enumerate (pipe &key count key (result pipe))
  "Go through all (or count) elements of pipe,
  possibly applying KEY function. (Try PRINT.)"
  ;; Returns RESULT, which defaults to the pipe itself.
  (if (or (eq pipe empty-pipe) (eql count 0))
      result
      (progn
        (unless (null key) (funcall key (head pipe)))
        (enumerate (tail pipe) :count (if count (- count 1))
                   :key key :result result))))

(defun filter (pred pipe)
  "Keep only items in pipe satisfying need."
  (if (funcall pred (head pipe))
      (make-pipe (head pipe)
                  (filter pred (tail pipe)))
      (filter pred (tail pipe))))

(defun sieve (pipe)
  (make-pipe (head pipe)
             (filter #'(lambda (x) (/= (mod x (head pipe)) 0))
                     (sieve (tail pipe)))))

(defvar *primes* (sieve (integers 2)))

*primes*

(enumerate *primes* :count 20)

(enumerate *primes* :count 500)
