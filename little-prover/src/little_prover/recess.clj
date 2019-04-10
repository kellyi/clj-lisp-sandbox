(ns little-prover.recess
  (:require [little-prover.jbob :refer :all]))

;; frame 3
(J-Bob-step (prelude) ;; list of axioms
            '(car (cons 'ham '(cheese))) ;; expression to rewrite
            '()) ;; list of steps

;; frame 7
(J-Bob-step (prelude)
            '(car (cons 'ham '(cheese)))
            '((() (car-cons 'ham '(cheese)))))

;; frame 9
(declare a b)
(J-Bob-step (prelude)
            '(equal 'flapjack (atom (cons a b)))
            '(((2) (atom-cons a b))
              (() (equal 'flapjack 'nil))))

;; frame 10
(declare p q)
(J-Bob-step (prelude)
            '(atom (cdr (cons (car (cons p q)) '())))
            '(((1 1 1) (car-cons p q))
              ((1) (cdr-cons p '()))
              (() (atom '()))))

;; frame 14
(declare c)
(J-Bob-step (prelude)
            '(if a c c)
            '())

;; frame 15
(J-Bob-step (prelude)
            '(if a c c)
            '((() (if-same a c))))

;; frame 16
(J-Bob-step (prelude)
            '(if a c c)
            '((() (if-same a c))
              (()
               (if-same
                (if (equal a 't)
                  (if (equal 'nil 'nil) a b)
                  (equal 'or
                         (cons 'black '(coffee))))
                c))))

;; frame 17
(J-Bob-step (prelude)
            '(if a c c)
            '((() (if-same a c))
              (()
               (if-same
                (if (equal a 't)
                  (if (equal 'nil 'nil) a b)
                  (equal 'or
                         (cons 'black '(coffee))))
                c))
              ((Q E 2) (cons 'black '(coffee)))))

;; frame 22
(J-Bob-step (prelude)
            '(if a c c)
            '((() (if-same a c))
              (()
               (if-same
                (if (equal a 't)
                  (if (equal 'nil 'nil)
                    a
                    b)
                  (equal 'or
                         (cons 'black '(coffee))))
                c))
              ((Q E 2) (cons 'black '(coffee)))
              ((Q A Q) (equal-same 'nil))))

;; frame 23
(J-Bob-step (prelude)
            '(if a c c)
            '((() (if-same a c))
              (()
               (if-same
                (if (equal a 't)
                  (if (equal 'nil 'nil)
                    a
                    b)
                  (equal 'or
                         (cons 'black '(coffee))))
                c))
              ((Q E 2) (cons 'black '(coffee)))
              ((Q A Q) (equal-same 'nil))
              ((Q A 2) (if-true a b))))

;; frame 27
(J-Bob-step (prelude)
            '(if a c c)
            '((() (if-same a c))
              (()
               (if-same
                (if (equal a 't)
                  (if (equal 'nil 'nil)
                    a
                    b)
                  (equal 'or
                         (cons 'black '(coffee))))
                c))
              ((Q E 2) (cons 'black '(coffee)))
              ((Q A Q) (equal-same 'nil))
              ((Q A) (if-true a b))))

;; frame 29
(J-Bob-prove (prelude)
             '())

;; frame 30
(J-Bob-prove (prelude)
             '(((defun pair (x y)
                  (cons x (cons y '()))) ;; first arg is a list of definitions
                nil))) ;; second arg is a list of proof attempts

;; frame 33
(J-Bob-prove (prelude)
             '(((defun pair (x y)
                  (cons x (cons y '())))
                nil)
               ((defun first-of (x)
                  (car x))
                nil)
               ((defun second-of (x)
                  (car (cdr x)))
                nil)))

;; frame 34
(J-Bob-prove (prelude)
             '(((defun pair (x y)
                  (cons x (cons y '())))
                nil)
               ((defun first-of (x)
                  (car x))
                nil)
               ((defun second-of (x)
                  (car (cdr x)))
                nil)
               ((dethm first-of-pair (a b)
                       (equal (first-of (pair a b)) a))
                nil)))

;; frame 35
(J-Bob-prove (prelude)
             '(((defun pair (x y)
                  (cons x (cons y '())))
                nil)
               ((defun first-of (x)
                  (car x))
                nil)
               ((defun second-of (x)
                  (car (cdr x)))
                nil)
               ((dethm first-of-pair (a b)
                       (equal (first-of (pair a b)) a))
                nil
                ((1 1) (pair a b)))))

;; frame 38
(J-Bob-prove (prelude)
             '(((defun pair (x y)
                  (cons x (cons y '())))
                nil)
               ((defun first-of (x)
                  (car x))
                nil)
               ((defun second-of (x)
                  (car (cdr x)))
                nil)
               ((dethm first-of-pair (a b)
                       (equal (first-of (pair a b)) a))
                nil
                ((1 1) (pair a b))
                ((1) (first-of (cons a (cons b '()))))
                ((1) (car-cons a (cons b '())))
                (() (equal-same a)))))

;; frame 42
(defun prelude+first-of-pair ()
  (J-Bob-define (prelude)
                '(((defun pair (x y)
                     (cons x (cons y '())))
                   nil)
                  ((defun first-of (x)
                     (car x))
                   nil)
                  ((defun second-of (x)
                     (car (cdr x)))
                   nil)
                  ((dethm first-of-pair (a b)
                          (equal (first-of (pair a b)) a))
                   nil
                   ((1 1) (pair a b))
                   ((1) (first-of (cons a (cons b '()))))
                   ((1) (car-cons a (cons b '())))
                   (() (equal-same a))))))

;; frame 43
(J-Bob-prove (prelude)
             '(((dethm second-of-pair (a b)
                       (equal (second-of (pair a b)) b))
                nil)))

;; frame 45
(J-Bob-prove (prelude+first-of-pair)
             '(((dethm second-of-pair (a b)
                       (equal (second-of (pair a b)) b))
                nil)))

;; frame 46
(J-Bob-prove (prelude+first-of-pair)
             '(((dethm second-of-pair (a b)
                       (equal (second-of (pair a b)) b))
                nil)
               ((defun in-pair? (xs)
                  (if (equal (first-of xs) '?)
                    't
                    (equal (second-of xs) '?)))
                nil)
               ((dethm in-first-of-pair (b)
                       (equal (in-pair? (pair '? b)) 't))
                nil)
               ((dethm in-second-of-pair (a)
                       (equal (in-pair? (pair a '?)) 't))
                nil)))

;; frame 50
(J-Bob-prove (prelude)
             '(((defun list? (x)
                  (if (atom x)
                    (equal x '())
                    (list? (cdr x))))
                nil)))

;; frame 52
(J-Bob-prove (prelude)
             '(((defun list? (x)
                  (if (atom x)
                    (equal x '())
                    (list? (cdr x))))
                (size x))))

;; frame 53
(J-Bob-prove (prelude)
             '(((defun list? (x)
                  (if (atom x)
                    (equal x '())
                    (list? (cdr x))))
                (size x)
                ((Q) (natp-size x))
                (()
                 (if-true
                  (if (atom x)
                    't
                    (< (size (cdr x)) (size x)))
                  'nil))
                ((E) (size-cdr x))
                (() (if-same (atom x) 't)))))

;; frame 55
(J-Bob-prove (prelude)
             '(((defun memb? (xs)
                  (if (atom xs)
                    'nil
                    (if (equal (car xs) '?)
                      't
                      (memb? (cdr xs)))))
                (size xs))
               ((defun remb (xs)
                  (if (atom xs)
                    '()
                    (if (equal (car xs) '?)
                      (remb (cdr xs))
                      (cons (car xs) (remb (cdr xs))))))
                (size xs))))

;; frame 64
(J-Bob-prove (prelude)
             '(((defun sub (x y)
                  (if (atom y)
                    (if (equal y '?) x y)
                    (cons (sub x (car y))
                          (sub x (cdr y)))))
                (size y))
               ((defun ctx? (x)
                  (if (atom x)
                    (equal x '?)
                    (if (ctx? (car x))
                      't
                      (ctx? (cdr x)))))
                (size x))
               ((dethm ctx-sub (x y)
                       (if (ctx? x)
                         (if (ctx? y)
                           (equal (ctx? (sub x y)) 't)
                           't)
                         't))
                (star-induction y))))
