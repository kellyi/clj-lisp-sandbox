(ns reasoned-schemer.eight
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
(defn bit-xoro
  [x y r]
  (conde
   [(== 0 x) (== 0 y) (== 0 r)]
   [(== 0 x) (== 1 y) (== 1 r)]
   [(== 1 x) (== 0 y) (== 1 r)]
   [(== 1 x) (== 1 y) (== 0 r)]))

(defn half-addero
  [x y r c]
  (conde
   [(== 0 x) (== 0 y) (== 0 r) (== 0 c)]
   [(== 1 x) (== 0 y) (== 1 r) (== 0 c)]
   [(== 0 x) (== 1 y) (== 1 r) (== 0 c)]
   [(== 1 x) (== 1 y) (== 0 r) (== 1 c)]))

(defn full-addero-prime
  [b x y r c]
  (fresh [w xy wz]
    (half-addero x y w xy)
    (half-addero w b r wz)
    (bit-xoro xy wz c)))

(defn full-addero
  [b x y r c]
  (conde
   [(== 0 b) (== 0 x) (== 0 y) (== 0 r) (== 0 c)]
   [(== 1 b) (== 0 x) (== 0 y) (== 1 r) (== 0 c)]
   [(== 0 b) (== 1 x) (== 0 y) (== 1 r) (== 0 c)]
   [(== 1 b) (== 1 x) (== 0 y) (== 0 r) (== 1 c)]
   [(== 0 b) (== 0 x) (== 1 y) (== 1 r) (== 0 c)]
   [(== 1 b) (== 0 x) (== 1 y) (== 0 r) (== 1 c)]
   [(== 0 b) (== 1 x) (== 1 y) (== 0 r) (== 1 c)]
   [(== 1 b) (== 1 x) (== 1 y) (== 1 r) (== 1 c)]))

(defn poso
  [n]
  (fresh [a d]
    (== (llist a d) n)))

(defn >1o
  [n]
  (fresh [a ad dd]
    (== (llist a ad dd) n)))

(declare bound-timeso odd-timeso timeso addero gen-addero)

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
                                      (== `(~a ~c) r)
                                      (full-addero b 1 1 a c))]
   [(== (list 1) n) (gen-addero b n m r)]
   [(== (list 1) m) (>1o n) (>1o r) (addero b (list 1) n r)]
   [(>1o n) (gen-addero b n m r)]))

(defn pluso
  [n m k]
  (addero 0 n m k))

(defn minuso
  [n m k]
  (pluso m k n))

;; frame 24
(defn bound-timeso
  [q p n m]
  (conde
   [(== (list) q) (poso p)]
   [(fresh [a0 a1 a2 a3 x y z]
      (== (llist a0 x) q)
      (== (llist a1 y) p)
      (conde
       [(== (list) n) (== (llist a2 z) m) (bound-timeso x y z (list))]
       [(== (llist a3 z) n) (bound-timeso x y z m)]))]))

;; frame 17
(defn odd-timeso
  [x n m p]
  (fresh [q]
    (bound-timeso q p n m)
    (timeso x m q)
    (pluso (llist 0 q) m p)))

;; frame 9
(defn timeso
  [n m p]
  (conde
   [(== (list) n) (== (list) p)]
   [(poso n) (== (list) m) (== (list) p)]
   [(== (list 1) n) (poso m) (== m p)]
   [(>1o n) (== (list 1) m) (== n p)]
   [(fresh [x z]
      (== (llist 0 x) n) (poso x)
      (== (llist 0 z) p) (poso z)
      (>1o m)
      (timeso x m z))]
   [(fresh [x y]
      (== (llist 1 x) n) (poso x)
      (== (llist 0 y) m) (poso y)
      (timeso m n p))]
   [(fresh [x y]
      (== (llist 1 x) n) (poso x)
      (== (llist 1 y) m) (poso y)
      (odd-timeso x n m p))]))

;; frame 1
(run 10 [x y r]
  (timeso x y r))

;; frame 3
(run* [p]
  (timeso (list 0 1) (list 0 0 1) p))

;; frame 19
(run 1 [n m]
  (timeso n m (list 1)))

;; frame 20
(run 1 [n m]
  (>1o n)
  (>1o m)
  (timeso n m (list 1 1)))

;; frame 25
(run 2 [n m]
  (timeso n m (list 1)))

;; frame 26
(run* [p]
  (timeso '(1 1 1) '(1 1 1 1 1 1) p))
;; ?

;; frame 28
(defn =lo
  [n m]
  (conde
   [(== (list) n) (== (list) m)]
   [(== (list 1) n) (== (list 1) m)]
   [(fresh [a x b y]
      (== (llist a x) n) (poso x)
      (== (llist b y) m) (poso y)
      (=lo x y))]))

;; frame 29
(run* [w x y]
  (=lo (llist 1 w x y) '(0 1 1 0 1)))

;; frame 30
(run* [b]
  (=lo (list 1) `(~b)))

;; frame 31
(run* [n]
  (=lo (llist 1 0 1 n) '(0 1 1 0 1)))

;; frame 32
(run 5 [x y]
  (=lo (llist 1 y) (llist 1 x)))

;; frame 33
(run 5 [y z]
  (=lo (llist 1 y) (llist 0 z)))

;; frame 35
(run 5 [y z]
  (=lo (llist 1 y) (llist 0 1 1 0 1 z)))

;; frame 36
(defn <lo
  [n m]
  (conde
   [(== (list) n) (poso m)]
   [(== (list 1) n) (>1o m)]
   [(fresh [a x b y]
      (== (llist a x) n) (poso x)
      (== (llist b y) m) (poso y)
      (<lo x y))]))

;; frame 37
(run 8 [y z]
  (<lo (llist 1 y) (llist 0 1 1 0 1 z)))

;; frame 39
;; (run 1 [n]
;;   (<lo n n))
;; no value

;; frame 40
(defn <=lo
  [n m]
  (conde
   [(=lo n m)]
   [(<lo n m)]))

;; frame 41
(run 8 [n m]
  (<=lo n m))

;; frame 42
(run 1 [n m]
  (<=lo n m)
  (timeso n (list 0 1) m))

;; frame 43
(run 10 [n m]
  (<=lo n m)
  (timeso n (list 0 1) m))

;; frame 44
(run 9 [n m]
  (<=lo n m))

;; frame 46
(defn <o
  [n m]
  (conde
   [(<lo n m)]
   [(=lo n m) (fresh [x]
                (poso x)
                (pluso n x m))]))

(defn <=o
  [n m]
  (conde
   [(== n m)]
   [(<o n m)]))

;; frame 47
(run* [q]
  (<o (list 1 0 1) (list 1 1 1)))
;; ?

;; frame 48
(run* [q]
  (<o (list 1 1 1) (list 1 0 1)))

;; frame 49
(run* [q]
  (<o (list 1 0 1) (list 1 0 1)))

;; frame 50
(run 6 [n]
  (<o n (list 1 0 1)))

;; frame 51
(run 6 [m]
  (<o (list 1 0 1) m))

;; frame 52
;; (run* [n]
;;   (<o n n))
;; no value

(declare divideso)

;; frame 53
(run 4 [n m q r]
  (divideso n m q r))

;; frame 54
(defn divideso
  [n m q r]
  (conde
   [(== (list) q) (== n r) (<o n m)]
   [(== (list 1) q) (== (list) r) (== n m) (<o r m)]
   [(<o m n) (<o r m) (fresh [mq]
                        (<=o mq n)
                        (timeso m q mq)
                        (pluso mq r n))]))

;; frame 62
(run* [m]
  (fresh [r]
    (divideso (list 1 0 1) m (list 1 1 1) r)))

;; frame 64
;; (defn divideso-prime
;;   [n m q r]
;;   (fresh [mq]
;;     (<o r m)
;;     (<=lo mq n)
;;     (timeso m q mq)
;;     (pluso mq r n)))

;; frame 69
(defn splito
  [n r l h]
  (conde
   [(== (list) n) (== (list) h) (== (list) l)]
   [(fresh [b n-prime]
      (== (llist 0 b n-prime) n) (== (list) r)
      (== (llist b n-prime) h) (== (list) l))]
   [(fresh [n-prime]
      (== (llist 1 n-prime) n) (== (list) r)
      (== n-prime h) (== (list 1) l))]
   [(fresh [b n-prime a r-prime]
      (== (llist 0 b n-prime) n)
      (== (llist a r-prime) r) (== (list) l)
      (splito (llist b n-prime) r-prime (list) h))]
   [(fresh [n-prime a r-prime]
      (== (llist 1 n-prime) n)
      (== (llist a r-prime) r) (== (list 1) l)
      (splito n-prime r-prime (list) h))]
   [(fresh [b n-prime a r-prime l-prime]
      (== (llist b n-prime) n)
      (== (llist a r-prime) r)
      (== (llist b l-prime) l)
      (poso l)
      (splito n-prime r-prime l-prime h))]))

;; frame 73
(run* [l h]
  (splito (list 0 0 1 0 1) (list 1) l h))

;; frame 74
(run* [l h]
  (splito '(0 0 1 0 1) '(1) l h))

;; frame 75
(run* [l h]
  (splito '(0 0 1 0 1) '(0 1) l h))

;; frame 76
(run* [l h]
  (splito '(0 0 1 0 1) '(1 1) l h))

;; frame 77
(run* [r l h]
  (splito '(0 0 1 0 1) r l h))

(declare divideso-prime)

;; frame 81
(defn n-wider-than-mo
  [n m q r]
  (fresh [n-high n-low q-high q-low]
    (fresh [mq-low mrq-low rr r-high]
      (splito n r n-low n-high)
      (splito q r q-low q-high)
      (conde
       [(== (list) n-high) (== (list) q-high) (minuso n-low r mq-low) (timeso m q-low mq-low)]
       [(poso n-high)
        (timeso m q-low mq-low)
        (pluso r mq-low mrq-low)
        (minuso mrq-low n-low rr)
        (splito rr r (list) r-high)
        (divideso-prime n-high m q-high r-high)]))))

(defn divideso-prime
  [n m q r]
  (conde
   [(== (list) q) (== r n) (<o n m)]
   [(== (list 1) q) (=lo m n) (pluso r m n) (<o r m)]
   [(poso q) (<lo m n) (<lo r m) (n-wider-than-mo n m q r)]))

;; frame 82
(run 3 [y z]
  (divideso-prime (llist 1 0 y) '(0 1) z (list)))

(declare exp2o logo base-three-or-moreo repeated-mulo)

;; frame 84
(defn repeated-mulo
  [n q nq]
  (conde
   [(poso n) (== (list) q) (== (list 1) nq)]
   [(== (list 1) q) (== n nq)]
   [(>1o q) (fresh [q1 nq1]
              (pluso q1 (list 1) q)
              (repeated-mulo n q1 nq1)
              (timeso nq1 n nq))]))

(defn base-three-or-moreo
  [n b q r]
  (fresh [bw1 bw nw nw1 q-low1 q-low s]
    (exp2o b (list) bw1)
    (pluso bw1 (list 1) bw)
    (<lo q n)
    (fresh [q1 bwq1]
      (pluso q (list 1) q1)
      (timeso bw q1 bwq1)
      (<o nw1 bwq1))
    (exp2o n (list) nw1)
    (pluso nw1 (list 1) nw)
    (divideso-prime nw bw q-low1 s)
    (pluso q-low (list 1) q-low1)
    (<=lo q-low q)
    (fresh [bq-low q-high s qd-high qd]
      (repeated-mulo b q-low bq-low)
      (divideso-prime nw bw1 q-high s)
      (pluso q-low qd-high q-high)
      (pluso q-low qd q)
      (<=o qd qd-high)
      (fresh [bqd bq1 bq]
        (repeated-mulo b qd bqd)
        (timeso bq-low bqd bq)
        (timeso b bq bq1)
        (pluso bq r n)
        (<o n bq1)))))

(defn exp2o
  [n b q]
  (conde
   [(== (list 1) n) (== (list) q)]
   [(>1o n) (== (list 1) q) (fresh [s]
                              (splito n b s (list 1)))]
   [(fresh [q1 b2]
      (== (llist 0 q1) q) (poso q1)
      (<lo b n)
      (appendo b (llist 1 b) b2)
      (exp2o n b2 q1))]
   [(fresh [q1 n-high b2 s]
      (== (llist 1 q1) q) (poso q1)
      (poso n-high)
      (splito n b s n-high)
      (appendo b (llist 1 b) b2)
      (exp2o n-high b2 q1))]))

(defn logo
  [n b q r]
  (conde
   [(== (list) q) (<=o n b) (pluso r (list 1) n)]
   [(== (list 1) q) (>1o b) (=lo n b) (pluso r b n)]
   [(== (list 1) b) (poso q) (pluso r (list 1) n)]
   [(== (list) b) (poso q) (== r n)]
   [(== '(0 1) b) (fresh [a ad dd]
                    (poso dd)
                    (== (llist a ad dd) n)
                    (exp2o n (list) q)
                    (fresh [s]
                      (splito n dd r s)))]
   [(<=o '(1 1) b) (<lo b n) (base-three-or-moreo n b q r)]))

;; frame 91
(run* [r]
  (logo '(0 1 1 1) '(0 1) '(1 1) r))

;; frame 92
(run 9 [b q r]
  (logo '(0 0 1 0 0 0 1) b q r)
  (>1o q))

;; frame 93
(defn expo
  [b q n]
  (logo n b q (list)))

;; frame 94
(run* [t]
  (expo '(1 1) '(1 0 1) t))
;; ?
