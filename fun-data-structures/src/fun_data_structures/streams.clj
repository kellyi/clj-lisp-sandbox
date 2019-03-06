(ns fun-data-stuctures.streams
  (:require [clojure.spec.alpha :as spec]
            [clojure.spec.gen.alpha :as gen]))

(defn stream-cons
  "cons value onto stream"
  [value stream]
  (lazy-seq (cons value stream)))

(defn ++
  "Concat two streams"
  [s1 s2]
  (cond
    (nil? s1) s2
    :else (lazy-seq (stream-cons (first s1) (++ (next s1) s2)))))

(defn stream-take
  "Take n elements from a stream"
  [n stream]
  (cond
    (zero? n) nil
    (nil? stream) nil
    :else (lazy-seq (stream-cons (first stream) (stream-take (dec n) (next stream))))))

(defn stream-drop
  "Drop n elements from a stream"
  [n stream]
  (cond
    (zero? n) stream
    (nil? stream) nil
    :else (recur (dec n) (next stream))))

(defn stream-reverse
  "Reverse a stream"
  [stream]
  (letfn [(stream-reverse-prime
            [s r]
            (cond
              (nil? s) r
              :else (stream-reverse-prime (next s) (lazy-seq (stream-cons (first s) r)))))]
    (stream-reverse-prime stream nil)))

(def one (stream-cons 1 nil))
(def two (stream-cons 2 nil))
(def three (stream-cons 3 nil))
(def four (stream-cons 4 nil))
(def test-stream (++ one (++ two (++ three four))))

(stream-drop 1 test-stream)
(stream-reverse test-stream)
