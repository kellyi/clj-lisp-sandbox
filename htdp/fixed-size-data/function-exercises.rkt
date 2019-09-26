#lang racket
(require rackunit)

;; exercise 34
(define (string-first s)
  "Extract the first char from a non-empty string."
  (car (string->list s)))

(check-eq? #\s (string-first "string"))

;; exercise 35
(define (string-last s)
  "Extract the last char from a non-empty string."
  (last (string->list s)))

(check-eq? #\g (string-last "string"))

;; exercise 36
(require 2htdp/image)
(define (image-area i)
  (* (image-height i)
     (image-width i)))

(check-eq? (image-area (square 10 "solid" "red"))
           100)

;; exercise 37
(define (string-rest s)
  (list->string
   (cdr
    (string->list
     s))))

(check-equal? "tring" (string-rest "string"))

;; exercise 38
(define (string-remove-last s)
  (list->string
   ((lambda (xs) (reverse
                  (cdr
                   (reverse
                    xs))))
    (string->list
     s))))

(check-equal? "strin" (string-remove-last "string"))
