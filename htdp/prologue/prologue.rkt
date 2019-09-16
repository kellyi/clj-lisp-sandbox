#lang racket

;; (+ 1 1)
;; (+ 2 2)
;; (* 3 3)
;; (- 4 2)
;; (/ 6 2)
;; (sqr 4)
;; (expt 2 3)
;; (sin 0)
;; (cos pi)

;; (string-append "hello " "world")
;; (+ (string-length "hello world")
;;    20)

;; (number->string 42)
;; (string->number "hello world")
;; (and #true #true)
;; (or #false #false)
;; (not #false)

;; (require 2htdp/image)
;; (circle 10 "solid" "red")
;; (rectangle 30 20 "outline" "blue")
;; (overlay (circle 5 "solid" "red")
;;          (rectangle 20 20 "solid" "blue"))

;; (overlay (rectangle 20 20 "solid" "blue")
;;          (circle 5 "solid" "red"))

;; (image-width (square 10 "solid" "red"))

;; (place-image (circle 5 "solid" "green")
;;              50 80
;;              (empty-scene 100 100))

;; (define (y x) (* x x))
;; (y 1)
;; (y 2)

;; (empty-scene 100 60)

(define (sign x)
  (cond
    [(> x 0) 1]
    [(= x 0) 0]
    [(< x 0) -1]))

(sign 10)
(sign -5)
(sign 0)

(define rocket-emoji "🚀")
rocket-emoji
