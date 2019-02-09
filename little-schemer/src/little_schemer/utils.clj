(ns little-schemer.utils
  (:require [clojure.spec.alpha :as spec]))

(defn atom?
  "Check whether an argument is an atom: e.g. not a list"
  [x]
  (-> x list? not))

(atom? 'atom)
(atom? 'turkey)
(atom? 1492)
(atom? "u")
(atom? '*abc$)
(atom? '(:atom))
(atom? '(atom turkey or))
(list? '(atom turkey or))
(list? '(how are you doing so far))
(list? '(((how) are) ((you) (doing so)) far))
(list? '())
(atom? '())
(list? '(() () () ()))


(defn car
  "Get first element of a non-empty list"
  [l]
  (cond
    (atom? l) nil
    (nil? l) nil
    (empty? l) nil
    :else (first l)))

(car '((a b c) x y z))
(car '())
(car '(((hotdogs)) (and) (pickle) relish))
(-> '(((hotdogs)) (and)) car car)

(defn cdr
  "Get tail of a list"
  [l]
  (cond
    (atom? l) nil
    (nil? l) nil
    (empty? l) nil
    :else (rest l)))

(cdr '(a b c))
(cdr '((a b c) x y z))
(cdr '(hamburger))
(cdr '((x) t r))
(cdr 'hotdogs)
(cdr '())

(-> '((b) (x y) ((c))) cdr car)
(-> '((b) (x y) ((c))) cdr cdr)
(-> '(a (b (c)) d) car cdr)

(cons 'peanut '(butter and jelly))
(cons '(banana and) '(peanut butter and jelly))
(cons '((help) this) '(is very ((hard) to learn)))
(cons '(a b (c)) '())
(cons 'a '())
;; (cons '((a b c)) 'b) -> no answer / invalid
;; (cons 'a 'b) -> no answer / invalid

(cons 'a (car '((b) c d)))
(cons 'a (cdr '((b) c d)))

(defn null?
  "Check whether a list has zero s expressions (e.g. `empty?`)"
  [l]
  (empty? l))

(null? '())
(null? (quote ()))
(null? '(a b c))
;; (null? 'spaghetti) -> no answer / invalid
(atom? 'Harry)
(atom? '(Harry had a heap of apples))
(-> '(Harry had a heap of apples) car atom?)
(-> '(Harry had a heap of apples) cdr atom?)
(-> '(Harry) cdr atom?)
(-> '(swing low sweet cherry oat) cdr car atom?)
(-> '(swing (low sweet) cherry oat) cdr car atom?)
(= 'Harry 'Harry)

(defn eq?
  "Check equality between x & y"
  [x y]
  (cond
    (-> x atom? not) nil
    (-> y atom? not) nil
    (number? x) nil
    (number? y) nil
    :else (= x y)))

(eq? 'Harry 'Harry)
(eq? 'margarine 'butter)
(eq? '() '(strawberry))
(eq? 6 7)
(eq? (car '(Mary had a little lamb chop)) 'Mary)
(eq? (cdr '(soured milk)) 'milk)
(eq? (car '(beans beans we need jelly beans)) (-> '(beans beans we need jelly beans) cdr car))
