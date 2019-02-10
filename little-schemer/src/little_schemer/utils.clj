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

(defn lat?
  "Look at each s-expression in a list and check if it's an atom until the end of the s-expressions"
  [l]
  (cond
    (nil? l) true
    (-> l car atom?) (recur (cdr l))
    :else false))

(lat? '(Jack Sprat could eat no chicken fat))
(lat? '((Jack) Sprat could eat no chicken fat))
(lat? '(Jack (Sprat could) eat no chicken fat))
(lat? '())
(lat? '(bacon (and eggs)))
(lat? '(mashed potatoes and gravy))

(defn rember?
  "Remove an atom from a list of atoms"
  [a lat]
  (cond
    (nil? lat) '()
    (eq? (car lat) a) (cdr lat)
    :else (cons (car lat) (rember? a (cdr lat)))))

(rember? 'mint '(lamb chops and mint jelly))

(defn firsts
  "Takes a list which is either a null list or contains only non-empty lists.
  Returns a list of each list's first atoms"
  [lol]
  (cond
    (null? lol) '()
    :else (cons (-> lol car car) (-> lol cdr firsts))))

(firsts '((a) (b) (c) (d)))
(firsts '())

(defn insert-r
  "Inserts a new atom to the right of the old atom in a list of atoms"
  [new old lat]
  (cond
    (null? lat) '()
    (eq? old (car lat)) (cons old (cons new (cdr lat)))
    :else (cons (car lat) (insert-r new old (cdr lat)))))

(insert-r 'topping 'fudge '(ice cream with fudge for dessert))

(defn insert-l
  "Inserts a new atom to the left of the old atom in a list of atoms"
  [new old lat]
  (cond
    (null? lat) '()
    (eq? old (car lat)) (cons new (cons old (cdr lat)))
    :else (cons (car lat) (insert-l new old (cdr lat)))))

(insert-l 'fudge 'topping '(ice cream with topping for dessert))

(defn subst
  "Replaces the first occurence of old in lat with new"
  [new old lat]
  (cond
    (null? lat) '()
    (eq? old (car lat)) (cons new (cdr lat))
    :else (cons (car lat) (subst new old (cdr lat)))))

(subst 'fudge 'topping '(ice cream with topping for dessert))

(defn subst2
  "Replaces either the first occurence of o1 or the first occurence of o2 in lat with new"
  [new o1 o2 lat]
  (cond
    (null? lat) '()
    (or (eq? o1 (car lat)) (eq? o2 (car lat))) (cons new (cdr lat))
    :else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))

(subst2 'vanilla 'chocolate 'banana '(banana ice cream with chocolate topping))

(defn multirember
  "Return a new list of atoms by removing all atoms a from lat"
  [a lat]
  (cond
    (null? lat) '()
    (eq? a (car lat)) (multirember a (cdr lat))
    :else (cons (car lat) (multirember a (cdr lat)))))

(multirember 'cup '(coffee cup tea cup and milk cup))

(defn multiinsert-r
  "Insert a new atom to the right of each old atom in a list of atoms"
  [new old lat]
  (cond
    (null? lat) '()
    (eq? old (car lat)) (cons (car lat) (cons new (multiinsert-r new old (cdr lat))))
    :else (cons (car lat) (multiinsert-r new old (cdr lat)))))

(multiinsert-r 'cup 'tea '(tea tea tea tea tea tea tea tea))

(defn multiinsert-l
  "Insert a new atom to the left of each old atom in a list of atoms"
  [new old lat]
  (cond
    (null? lat) '()
    (eq? old (car lat)) (cons new (cons (car lat) (multiinsert-l new old (cdr lat))))
    :else (cons (car lat) (multiinsert-l new old (cdr lat)))))

(multiinsert-l 'coffee 'cup '(cup cup cup cup cup cup cup))

(defn multisubst
  "Replace all occurences of old atom in lat with new"
  [new old lat]
  (cond
    (null? lat) '()
    (eq? old (car lat)) (cons new (multisubst new old (cdr lat)))
    :else (cons (car lat) (multisubst new old (cdr lat)))))

(multisubst 'coffee 'tea '(tea cup tea shop tea house tea pot))
