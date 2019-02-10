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

(atom? 14)

(defn add1
  "Add one to a natural integer"
  [n]
  (cond
    (-> n nat-int? not) nil
    :else (inc n)))

(defn sub1
  "Subtract one from a natural integer"
  [n]
  (cond
    (-> n nat-int? not) nil
    (zero? n) nil
    :else (dec n)))

(zero? 0)

(defn plus
  "Add two natural integers"
  [n m]
  (cond
    (zero? m) n
    :else (add1 (plus n (sub1 m)))))

(plus 2 2)
(plus 30 5)

(defn minus
  "Subtract m from n"
  [n m]
  (cond
    (zero? m) n
    :else (sub1 (minus n (sub1 m)))))

(minus 30 5)
(minus 2 2)

(defn tup?
  "Check whether a list is a tuple of numbers"
  [l]
  (cond
    (null? l) true
    (-> l car nat-int? not) false
    :else (-> l cdr tup?)))

(tup? '(1 2 3))
(tup? '(a b c))
(tup? '(4 5 6 x y z))

(defn addtup
  "Add a tuple of numbers"
  [t]
  (cond
    (null? t) 0
    :else (plus (car t) (-> t cdr addtup))))

(addtup '(1 2 3 4 5))
(addtup '(10 20 30 5))

(defn multiply
  "Multiply n m times"
  [n m]
  (cond
    (zero? m) 0
    :else (plus n (multiply n (sub1 m)))))

(multiply 3 3)
(multiply 10 5)

(defn tup-plus
  "Return a new tuple which adds each element of t1 to the element at the same index in t2"
  [t1 t2]
  (cond
    (and (null? t1) (null? t2)) '()
    (null? t1) t2
    (null? t2) t1
    :else (cons (plus (car t1) (car t2)) (tup-plus (cdr t1) (cdr t2)))))

(tup-plus '(1 1 1 1 1) '(10 10 10 10 10))
(tup-plus '(1 1 1) '(10 10 10 10 10 10 10 10))

(defn gt
  "Check whether n is greater than m"
  [n m]
  (cond
    (zero? n) false
    (zero? m) true
    :else (recur (sub1 n) (sub1 m))))

(gt 10 5)
(gt 5 10)
(gt 10 10)

(defn lt
  "Check whether n is less than m"
  [n m]
  (cond
    (zero? m) false
    (zero? n) true
    :else (recur (sub1 n) (sub1 m))))

(lt 10 5)
(lt 5 10)
(lt 10 10)

(defn equals
  "Check whether n equals m"
  [n m]
  (cond
    (gt n m) false
    (lt n m) false
    :else true))

(equals 10 10)
(equals 5 5)
(equals 1 2)

(defn divide
  "Divide n by m"
  [n m]
  (cond
    (lt n m) 0
    :else (add1 (divide (minus n m) m))))

(divide 10 5)
(divide 20 2)
(divide 20 3)

(defn lat-length
  "Count the length of lat"
  [lat]
  (cond
    (null? lat) 0
    :else (add1 (lat-length (cdr lat)))))

(lat-length '(1 2 3 4 5))
(lat-length '(hello world foo bar baz))
(lat-length '())

(defn pick
  "Pick the element at index from lat"
  [index lat]
  (cond
    (zero? (sub1 index)) (car lat)
    :else (pick (sub1 index) (cdr lat))))

(pick 4 '(hello world foo bar baz))

(defn rempick
  "Pick and remove the element at index from lat"
  [index lat]
  (cond
    (zero? (sub1 index)) (cdr lat)
    :else (cons (car lat) (rempick (sub1 index) (cdr lat)))))

(rempick 3 '(hotdogs with hot mustard))

(defn no-nums
  "Return a version of lat with all natural integers removed"
  [lat]
  (cond
    (null? lat) '()
    (nat-int? (car lat)) (-> lat cdr no-nums)
    :else (cons (car lat) (-> lat cdr no-nums))))

(no-nums '(1 2 3 a b c))
(no-nums '(a b c))
(no-nums '(1.2 2 3.4 a))

(defn all-nums
  "Return a version of lat which only includes its natural integers"
  [lat]
  (cond
    (null? lat) '()
    (-> lat car nat-int? not) (-> lat cdr all-nums)
    :else (cons (car lat) (-> lat cdr all-nums))))

(all-nums '(1 2 3 a b c))
(all-nums '(a b c))
(all-nums '())
(all-nums '(1.2 2 3.4 4 a))

(defn eqan?
  "Check number or atom equality"
  [a1 a2]
  (cond
    (and (nat-int? a1) (nat-int? a2)) (equals a1 a2)
    (or (nat-int? a1) (nat-int? a2)) false
    :else (eq? a1 a2)))

(eqan? 1 2)
(eqan? 1 '1)
(eqan? 1.0 1)
(eqan? 'a 'a)
(eqan? 'a 1)

(defn occur
  "Count the number of times atom a occurs in lat"
  [a lat]
  (cond
    (null? lat) 0
    (eqan? (car lat) a) (add1 (occur a (cdr lat)))
    :else (occur a (cdr lat))))

(occur 1 '(1 2 3 4 1 2 3 4))
(occur 'a '(1 a 2 a 3 a 4 a))
(occur 'b '(1 2 3 4 5 6))
