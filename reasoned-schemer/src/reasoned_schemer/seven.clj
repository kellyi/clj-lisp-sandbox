(ns reasoned-schemer.seven
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic]
        [reasoned-schemer.utils :as utils :only [pair?
                                                 pairo
                                                 null?
                                                 nullo
                                                 car
                                                 cdr
                                                 caro
                                                 cdro]]))

;; frame 3
(defn bit?
  [x]
  (case x
    0 true
    1 true
    false))

;; frame 5
(defn bit-xoro
  [x y r]
  (conde
   [(== 0 x) (== 0 y) (== 0 r)]
   [(== 0 x) (== 1 y) (== 1 r)]
   [(== 1 x) (== 0 y) (== 1 r)]
   [(== 1 x) (== 1 y) (== 0 r)]))

;; frame 6
(run* [x y]
  (bit-xoro x y 0))

;; frame 8
(run* [x y]
  (bit-xoro x y 1))

;; frame 9
(run* [x y r]
  (bit-xoro x y r))

;; frame 10
(defn bit-ando
  [x y r]
  (conde
   [(== 0 x) (== 0 y) (== 0 r)]
   [(== 1 x) (== 0 y) (== 0 r)]
   [(== 0 x) (== 1 y) (== 0 r)]
   [(== 1 x) (== 1 y) (== 1 r)]))

;; frame 11
(run* [x y]
  (bit-ando x y 1))

;; frame 12
(defn half-addero
  [x y r c]
  (all
    (bit-xoro x y r)
    (bit-ando x y c)))

(run* [r]
  (half-addero 1 1 r 1)) ;; -> (0)

(defn half-addero-prime
  [x y r c]
  (conde
   [(== 0 x) (== 0 y) (== 0 r) (== 0 c)]
   [(== 1 x) (== 0 y) (== 1 r) (== 0 c)]
   [(== 0 x) (== 1 y) (== 1 r) (== 0 c)]
   [(== 1 x) (== 1 y) (== 0 r) (== 1 c)]))

(run* [r]
  (half-addero-prime 1 1 r 1)) ;; -> (0)

;; frame 13
(run* [x y r c]
  (half-addero x y r c))

;; frame 15
(defn full-addero
  [b x y r c]
  (fresh [w xy wz]
    (half-addero x y w xy)
    (half-addero w b r wz)
    (bit-xoro xy wz c)))

(run* [r c]
  (full-addero 0 1 1 r c))

;; frame 16
(run* [r c]
  (full-addero 1 1 1 r c))

;; frame 17
(run* [b x y r c]
  (full-addero b x y r c))

;; frame 42
(defn build-num
  [n]
  (cond
    (zero? n) '()
    (even? n) (cons 0 (build-num (/ n 2)))
    :else (cons 1 (build-num (/ (- n 1) 2)))))

;; frame 43
(defn build-num-prime
  [n]
  (cond
    (odd? n) (cons 1 (build-num (/ (- n 1) 2)))
    (and (-> n zero? not)
         (even? n)) (cons 0 (build-num (/ n 2)))
    :else '()))

;; frame 39
(build-num 0)
(build-num-prime 0)

;; frame 40
(build-num 36)
(build-num-prime 36)

;; frame 41
(build-num 19)
(build-num-prime 19)

;; frame 77
(defn poso
  [n]
  (fresh [a d]
    (== (llist a d) n)))

(run* [q]
  (poso (list 0 1 1)))

;; frame 78
(run* [q]
  (poso (list 1)))

;; frame 79
(run* [q]
  (poso (list)))

;; frame 80
(run* [r]
  (poso r))

;; frame 83
(defn >1o
  [n]
  (fresh [a ad dd]
    (== (llist a ad dd) n)))

(run* [q]
  (>1o (list 0 1 1)))

;; frame 84
(run* [q]
  (>1o (list 0 1)))

;; frame 85
(run* [q]
  (>1o (list 1)))

;; frame 86
(run* [q]
  (>1o (list)))

;; frame 87
(run* [r]
  (>1o r))

;; frame 89
(declare addero gen-addero)

;; frame 90
(run 3 [x y r]
  (addero 0 x y r))

;; frame 94
(run 19 [x y r]
  (addero 0 x y r))

;; frame 104
(defn gen-addero
  [b n m r]
  (fresh [a c d e x y z]
     (== (llist a x) n)
     (== (llist d y) m) (poso y)
     (== (llist c z) r) (poso z)
     (full-addero b a d c e)
     (addero e x y z)))

(defn addero
  [b n m r]
  (conde
   [(== 0 b) (== (list) m) (== n r)]
   [(== 0 b) (== (list) m) (== m r) (poso m)]
   [(== 1 b) (== (list) m) (addero 0 n (list 1) r)]
   [(== 1 b) (== (list) n) (poso m) (addero 0 (list 1) m r)]
   [(== (list 1) n) (== (list 1) m) (fresh [a c]
                                      (== [a c] r)
                                      (full-addero b 1 1 a c))]
   [(== (list 1) n) (gen-addero b n m r)]
   [(== (list 1) m) (>1o n) (>1o r) (addero b (list 1) n r)]
   [(>1o n) (gen-addero b n m r)]))

;; frame 106
(run* [s]
  (gen-addero 1 (list 0 1 1) (list 1 1) s))

;; frame 112
(run* [x y]
  (addero 0 x y '(1 0 1)))

;; frame 114
(defn pluso
  [n m k]
  (addero 0 n m k))

;; frame 115
(run* [x y]
  (pluso x y (list 1 0 1)))

;; frame 116
(defn minuso
  [n m k]
  (pluso m k n))

;; frame 117
(run* [q]
  (minuso '(0 0 0 1) '(1 0 1) q))

;; frame 118
(run* [q]
  (minuso '(0 1 1) '(0 1 1) q))

;; frame 119
(run* [q]
  (minuso '(0 1 1) '(0 0 0 1) q))

;; frame 120
(defn lengtho
  [l n]
  (conde
   [(nullo l) (== (list) n)]
   [(fresh [d res]
      (cdro l d)
      (pluso (list 1) res n)
      (lengtho d res))]))

;; frame 121
;; (run 1 [n]
;;   (lengtho '(jicama rhubarb guava) n))
;; overflows stack

;; frame 122
(run* [ls]
  (lengtho ls '(1 0 1)))

;; frame 123
(run* [ls]
  (lengtho '(1 0 1) 3))

;; frame 125
;; (run 4 [q]
;;   (lengtho q q))
;; overflows stack
