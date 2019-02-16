(ns little-schemer.utils
  (:require [clojure.spec.alpha :as spec]))

(declare null? equal? eqlist?)

(defn atom?
  "Check whether an argument is an atom: e.g. not a list"
  [x]
  (-> x list? not))

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

(defn equal?
  ""
  [s1 s2]
  (cond
    (and (atom? s1) (atom? s2)) (eqan? s1 s2)
    (or (atom? s1) (atom? s2)) false
    :else (eqlist? s1 s2)))

(defn eqlist?
  "Determine whether two lists are equal"
  [l1 l2]
  (cond
    (and (null? l1) (null? l2)) true
    (or (null? l1) (null? l2)) false
    :else (and (equal? (car l1) (car l2))
               (eqlist? (cdr l1) (cdr l2)))))

(eqlist? '(strawberry ice cream) '(strawberry ice cream))

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
    (null? lat) '()
    (equal? (car lat) a) (cdr lat)
    :else (cons (car lat) (rember? a (cdr lat)))))

(rember? 'mint '(lamb chops and mint jelly))

(defn firsts
  "Takes a list which is either a null list or contains only non-empty lists.
  Returns a list of each list's first atoms"
  [lol]
  (cond
    (null? lol) '()
    :else (cons (car (car lol))
                (firsts (cdr lol)))))

(firsts '((a z) (b y) (c x) (d)))
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
    (equal? a (car lat)) (multirember a (cdr lat))
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

(defn rember*
  "Remove atom a from nested list l"
  [a l]
  (cond
    (null? l) '()
    (atom? (car l)) (cond
                      (eq? a (car l)) (rember* a (cdr l))
                      :else (cons (car l) (rember* a (cdr l))))
    :else (cons (rember* a (car l)) (rember* a (cdr l)))))

(rember* 'cup '((coffee) cup ((tea) cup) (and (milk) cup)))
(rember* 'sauce '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce)))

(defn insert-r*
  "Insert a new atom to the right of old atom in a nested list l"
  [new old l]
  (cond
    (null? l) '()
    (atom? (car l)) (cond
                      (eq? (car l) old) (cons old (cons new (insert-r* new old (cdr l))))
                      :else (cons (car l) (insert-r* new old (cdr l))))
    :else (cons (insert-r* new old (car l)) (insert-r* new old (cdr l)))))

(insert-r* 'roast
           'chuck
           '(((how much (wood))
              could
              ((a (wood) chuck))
              (((chuck)))
              (if (a) ((wood chuck)))
              could
              chuck
              wood)))

(defn occur*
  "Count occurences of atom a in nested list l"
  [a l]
  (cond
    (null? l) 0
    (atom? (car l)) (cond
                      (eq? (car l) a) (add1 (occur* a (cdr l)))
                      :else (occur* a (cdr l)))
    :else (plus (occur* a (car l)) (occur* a (cdr l)))))

(occur* 'banana '((banana)
                  (split ((((banana ice))) (cream (banana)) sherbet))
                  (banana)
                  (bread)
                  (banana brandy)))

(defn subst*
  "Replace occurences of old atom in nested list l with new atom"
  [new old l]
  (cond
    (null? l) '()
    (atom? (car l)) (cond
                      (eq? (car l) old) (cons new (subst* new old (cdr l)))
                      :else (cons (car l) (subst* new old (cdr l))))
    :else (cons (subst* new old (car l)) (subst* new old (cdr l)))))

(subst* 'orange 'banana '((banana)
                          (split ((((banana ice))) (cream (banana)) sherbet))
                          (banana)
                          (bread)
                          (banana brandy)))

(defn insert-l*
  "Insert a new atom to the left of each occurence of old atom in nested list l"
  [new old l]
  (cond
    (null? l) '()
    (atom? (car l)) (cond
                      (eq? (car l) old) (cons new (cons old (insert-l* new old (cdr l))))
                      :else (cons (car l) (insert-l* new old (cdr l))))
    :else (cons (insert-l* new old (car l)) (insert-l* new old (cdr l)))))

(insert-l* 'pecker
           'chuck
           '(((how much (wood))
              could
              ((a (wood) chuck))
              (((chuck)))
              (if (a) ((wood chuck)))
              could
              chuck
              wood)))

(defn member*
  "Check whether atom a appears in nested list l"
  [a l]
  (cond
    (null? l) false
    (atom? (car l)) (or (equal? (car l) a) (member* a (cdr l)))
    :else (or (member* a (car l)) (member* a (cdr l)))))

(member* 'chips '((potato (chips ((with) fish) (chips)))))

(defn leftmost
  "Find the leftmost atom in a non-empty list of s-expressions that doesn't contain the empty list"
  [l]
  (cond
    (atom? (car l)) (car l)
    :else (leftmost (car l))))

(leftmost '((potato (chips ((with) fish) (chips)))))
(leftmost '(((hot) (tuna (and))) cheese))
(leftmost '())

(defn numbered?
  [aexp]
  (cond
    (atom? aexp) (nat-int? aexp)
    :else (and (numbered? (car aexp))
               (numbered? (-> aexp cdr cdr car)))))

(numbered? 1)
(numbered? '(1 2 3))

(defn arrow
  [x y]
  (cond
    (= x 3) x
    :else y))

(defn first-sub-exp
  [aexp]
  (-> aexp cdr car))

(defn second-sub-exp
  [aexp]
  (-> aexp cdr cdr car))

(defn operator
  [aexp]
  (car aexp))

(defn value*
  [nexp]
  (cond
    (atom? nexp) nexp
    (eq? (operator nexp) 'plus) (plus (-> nexp first-sub-exp value*)
                                      (-> nexp second-sub-exp value*))
    (eq? (operator nexp) 'multiply) (multiply (-> nexp first-sub-exp value*)
                                              (-> nexp second-sub-exp value*))
    :else (arrow (-> nexp first-sub-exp value*)
                 (-> nexp second-sub-exp value*))))

(defn sero?
  [n]
  (null? n))

(defn edd1
  [n]
  (cons '() n))

(sero? '())
(-> '() edd1 edd1 edd1 edd1)

(defn zub1
  [n]
  (cdr n))

(-> '(() ()) zub1 zub1)

(defn plus-plus
  [n m]
  (cond
    (sero? m) n
    :else (edd1 (plus-plus n (zub1 m)))))

(plus-plus '(() ()) '(() () ()))

(defn set?*
  "Check whether a list is a set"
  [lat]
  (cond
    (null? lat) true
    (member* (car lat) (cdr lat)) false
;;    (eq? (car lat) (-> lat cdr car)) false
    :else (set?* (cdr lat))))

(set?* '(1 2 3))
(set?* '(1 2 3 1))

(defn makeset
  "Create a set from a list"
  [lat]
  (cond
    (null? lat) '()
    (member* (car lat) (cdr lat)) (makeset (cdr lat))
    :else (cons (car lat) (makeset (cdr lat)))))

(makeset '(1 2 3 1))

(defn subset?
  "Check whether s1 is a subset of s2"
  [s1 s2]
  (cond
    (null? s1) true
    (member* (car s1) s2) (subset? (cdr s1) s2)
    :else false))

(subset? '(1 2 3) '(1 2 3 4 5 6))
(subset? '(1 2 3) '(2 3 4 5 6))

(defn eqset?
  "Check whether two sets are equal"
  [s1 s2]
  (and (subset? s1 s2)
       (subset? s2 s1)))

(eqset? '(1 2 3) '(1 2 3))
(eqset? '(1 2 3) '(1 2 3 4))

(defn intersect?
  "Check whether two sets intersect"
  [s1 s2]
  (cond
    (null? s1) false
    :else (or (member* (car s1) s2)
              (intersect? (cdr s1) s2))))

(intersect? '(1 2 3) '(3 4 5))
(intersect? '(1 2 3) '(4 5 6))

(defn intersection
  "Return the intersection of two sets"
  [s1 s2]
  (cond
    (null? s1) '()
    (member* (car s1) s2) (cons (car s1) (intersection (cdr s1) s2))
    :else (intersection (cdr s1) s2)))

(intersection '(stewed tomatoes and macaroni)
              '(macaroni and cheese))

(intersection '() '())

(intersection '(hello world) '(hello))

(defn union
  "Return the union of two sets"
  [s1 s2]
  (cond
    (null? s1) s2
    (member* (car s1) s2) (union (cdr s1) s2)
    :else (cons (car s1) (union (cdr s1) s2))))

(defn difference
  "Return the difference of two sets"
  [s1 s2]
  (cond
    (null? s1) '()
    (member* (car s1) s2) (difference (cdr s1) s2)
    :else (cons (car s1) (difference (cdr s1) s2))))

(difference '(stewed tomatoes and macaroni)
            '(macaroni and cheese))

(union '(1 2 3) '(4 5 6))
(union '(100 200 300) '(a b c))

(defn intersectall
  "Find the intersection of a list of sets"
  [l-set]
  (cond
    (null? (cdr l-set)) (car l-set)
    :else (intersection (car l-set) (-> l-set cdr intersectall))))

(intersectall '((a b c)
                (b e e a)
                (c a d e)))

(intersectall '((six pears and)
                (three peaches and six peppers)
                (six ducks and horses)))

(defn a-pair?
  "Check whether a list is a pair"
  [x]
  (cond
    (atom? x) false
    (null? x) false
    (null? (cdr x)) false
    (null? (cdr (cdr x))) true
    :else false))

(a-pair? '(1 2))
(a-pair? '((1) (2)))
(a-pair? '((1) (2) (3)))

(defn third
  "Get third element from a list"
  [l]
  (-> l cdr cdr car))

(third '(1 2 3))
(third '(1 2))
(third '(1 2 3 4 5))

(defn is-set?
  [s]
  (cond
    (empty? s) true
    (member* (car s) (cdr s)) false
    (eq? (car s) (-> s cdr car)) false
    :else (-> s cdr is-set?)))

(defn fun?
  ""
  [rel]
  (-> rel firsts set?*))

;;;;

(defn eq?-c
  [a]
  (fn
    [x]
    (eq? x a)))

(def eq?-salad (eq?-c 'salad))
(eq?-salad 'salad)
(eq?-salad 'tuna)

(defn rember-f
  [test?]
  (fn
    [a l]
    (cond
      (null? l) '()
      (test? (car l) a) (cdr l)
      :else (cons (car l) ((rember-f test?) a (cdr l))))))

(def rember-eq? (rember-f eq?))
(rember-eq? 'tuna '(tuna salad is good))
(rember-eq? 'tuna '(shrimp salad and tuna salad))
((rember-f eq?) 'tuna '(shrimp salad and tuna salad))

(defn insert-g
  [sequencer]
  (fn
    [new old l]
    (cond
      (null? l) '()
      (eq? (car l) old) (sequencer new old (cdr l))
      :else (cons (car l) ((insert-g sequencer) new old (cdr l))))))

(def insertL (insert-g (fn
                         [new old l]
                         (cons new (cons old l)))))

(def insertR (insert-g (fn
                         [new old l]
                         (cons old (cons new l)))))

(def subst3 (insert-g (fn
                        [new old l]
                        (cons new l))))

(defn yyy
  [a l]
  ((insert-g (fn
               [new old l]
               l)) false a l))

(yyy 'sausage '(pizza with sausage and bacon))

(defn atom-to-function
  [x]
  (cond
    (eq? x 'plus) plus
    (eq? x 'multiply) multiply
    :else arrow))

(defn value-prime
  [nexp]
  (cond
    (atom? nexp) nexp
    :else ((atom-to-function (operator nexp))
           (value-prime (first-sub-exp nexp))
           (value-prime (second-sub-exp nexp)))))

(defn multirember-f
  [test?]
  (fn
    [a lat]
    (cond
      (null? lat) '()
      (test? a (car lat)) ((multirember-f test?) a (cdr lat))
      :else (cons (car lat) ((multirember-f test?) a (cdr lat))))))

((multirember-f eq?) 'tuna '(tuna salad tuna steak and tuna))

(def eq?-tuna (eq?-c 'tuna))

(defn multiremberT
  [test? lat]
  (cond
    (null? lat) '()
    (test? (car lat)) (multiremberT test? (cdr lat))
    :else (cons (car lat) (multiremberT test? (cdr lat)))))

(multiremberT eq?-tuna '(shrimp salad tuna salad and tuna))

(defn multirember&co
  [a lat col]
  (cond
    (null? lat) (col '() '())
    (eq? (car lat) a) (multirember&co a (cdr lat) (fn
                                                    [newlat seen]
                                                    (col newlat (cons (car lat) seen))))
    :else (multirember&co a (cdr lat) (fn
                                        [newlat seen]
                                        (col (cons (car lat) newlat) seen)))))

(defn a-friend
  [x y]
  (null? y))

(multirember&co 'tuna '() a-friend)
(multirember&co 'tuna '(tuna) a-friend)
(multirember&co 'tuna '(strawberries tuna and swordfish) a-friend)

(defn multiinsertLR
  [new oldL oldR lat]
  (cond
    (null? lat) '()
    (eq? (car lat) oldL) (cons new (cons oldL (multiinsertLR new oldL oldR (cdr lat))))
    (eq? (car lat) oldR) (cons oldR (cons new (multiinsertLR new oldL oldR (cdr lat))))
    :else (cons (car lat) (multiinsertLR new oldL oldR (cdr lat)))))

(defn multiinsertLR&co
  [new oldL oldR lat col]
  (cond
    (null? lat) (col '() 0 0)
    (eq? (car lat) oldL) (multiinsertLR&co new
                                           oldL
                                           oldR
                                           (cdr lat)
                                           (fn
                                             [newlat L R]
                                             (col (cons new (cons oldL newlat)) (add1 L) R)))
    (eq? (car lat) oldR) (multiinsertLR&co new
                                           oldL
                                           oldR
                                           (cdr lat)
                                           (fn
                                             [newlat L R]
                                             (col (cons oldR (cons new newlat)) L (add1 R))))
    :else (multiinsertLR&co new oldL oldR (cdr lat) (fn
                                                      [newlat L R]
                                                      (col (cons (car lat) newlat) L R)))))

(defn evens-only*
  [l]
  (cond
    (null? l) '()
    (atom? (car l)) (cond
                      (even? (car l)) (cons (car l) (evens-only* (cdr l)))
                      :else (evens-only* (cdr l)))
    :else (cons (evens-only* (car l)) (evens-only* (cdr l)))))

(evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6)))

(defn keep-looking
  [a sorn lat]
  (cond
    (nat-int? sorn) (keep-looking a (pick sorn lat) lat)
    :else (eq? sorn a)))

(defn looking
  [a lat]
  (keep-looking a (pick 1 lat) lat))

(looking 'caviar '(6 2 4 caviar 5 7 3))
(looking 'caviar '(6 2 grits caviar 5 7 3))
(looking 'caviar '(6 2 4 caviar 5 7 3))
(pick 6 '(6 2 4 caviar 5 7 3))
(pick 7 '(6 2 4 caviar 5 7 3))
(keep-looking 'caviar 3 '(6 2 4 caviar 5 7 3))
;; 'looking is a partial function that never completes
;; for elements of its range for which its domain is undefined
;; (looking 'caviar '(7 1 2 caviar 5 6 3))

;; 'eternity is a totally partial function: it never completes for any of its
;; possible inputs
(defn eternity
  "A partial f"
  [x]
  (eternity x))

(defn build
  [s1 s2]
  (cons s1 (cons s2 '())))

(build '(1 2 3) '(4 5 6))

(defn shift
  [pair]
  (build (-> pair first first)
         (build (-> pair first second)
                (second pair))))

(shift '((a b) c))
(shift '((a b) (c d)))

(defn align
  [pora]
  (cond
    (atom? pora) pora
    (a-pair? (first pora)) (-> pora shift align)
    :else (build (first pora)
                 (-> pora second align))))

(defn length*
  [pora]
  (cond
    (atom? pora) 1
    :else (plus (-> pora first length*)
                (-> pora second length*))))

(defn weight*
  [pora]
  (cond
    (atom? pora) 1
    :else (plus (multiply (-> pora first weight*) 2)
                (-> pora second weight*))))

(defn revpair
  [pair]
  (build (second pair) (first pair)))

(defn shuffle*
  [pora]
  (cond
    (atom? pora) pora
    (-> pora first a-pair?) (-> pora revpair shuffle*)
    :else (build (first pora)
                 (-> pora second shuffle*))))

(shuffle* '(a (b c)))
(shuffle* '(a b))
(shuffle* '((a b) (c d)))

(defn one?
  [n]
  (= n 1))

(defn collatz
  [n]
  (cond
   (one? n) 1
   (even? n) (collatz (/ n 2))
   :else (collatz (add1 (multiply 3 n)))))

(collatz 5)
(collatz 1)
(collatz 10)
(collatz 50)

(defn ackermann
  [n m]
  (cond
    (zero? n) (add1 m)
    (zero? m) (ackermann (sub1 n) 1)
    :else (ackermann (sub1 n)
                     (ackermann n (sub1 m)))))

(ackermann 1 0)
(ackermann 1 1)
(ackermann 2 2)
;; (ackermann 4 3) -> no answer

(defn will-stop?
  "Hypothetical function to check whether a function f will stop (whether it is total)"
  [f]
  true)

(defn last-try
  "Arg for hypothetical will-stop function"
  [x]
  (and (will-stop? last-try)
       (eternity x)))

;; (last-try '())

(def length0
  (fn
    [l]
    (cond
      (null? l) 0
      :else (-> l cdr eternity add1))))

(length0 '())

(def length1
  (fn
    [l]
    (cond
      (null? l) 0
      :else (-> l cdr length0 add1))))

(length1 '(1))

(def lengthLTE1
  (fn
    [l]
    (null? l) 0
    :else (add1
           ((fn
              [l]
              (cond
                (null? l) 0
                :else (add1 (eternity (cdr l))))))
           (cdr l))))

(def length0-2
  ((fn
    [len]
    (fn
      [l]
      (cond
        (null? l) 0
        :else (add1 (len (cdr l))))))
    eternity))

(length0-2 '())

(def length1-2
  ((fn
     [f]
     (fn
       [l]
       (cond
         (null? l) 0
         :else (add1 (f (cdr l))))))
   ((fn
      [g]
      (fn
        [l]
        (cond
          (null? l) 0
          :else (add1 (g (cdr l))))))
    eternity)))

(length1-2 '())
(length1-2 '(1))

(def length2-2
  ((fn
     [f]
     (fn
       [l]
       (cond
         (null? l) 0
         :else (add1 (f (cdr l))))))
   ((fn
      [f]
      (fn
        [l]
        (cond
          (null? l) 0
          :else (add1 (f (cdr l))))))
    ((fn
       [f]
       (fn
         [l]
         (cond
           (null? l) 0
           :else (add1 (f (cdr l))))))
     eternity))))

(length2-2 '())
(length2-2 '(1))
(length2-2 '(1 2))

;; ...

(def Y
  (fn
    [le]
    ((fn
       [f]
       (f f))
     (fn
       [f]
       (le (fn
             [x]
             ((f f) x)))))))

;; (Y Y)
