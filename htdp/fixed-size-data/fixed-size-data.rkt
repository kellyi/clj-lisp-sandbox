#lang racket

(define (distance-from-origin x y)
  (sqrt
   (+ (sqr x)
      (sqr y))))

(distance-from-origin 12 5)

(define prefix "hello")
(define suffix "world")

(define (hello-world)
  (string-append prefix
                 (string-append "_" suffix)))

(hello-world)

(define (add-underscore-at-position input position)
  (string-append (substring input 0 position)
                 (string-append "_"
                                (substring input position))))

(add-underscore-at-position "helloworld" 5)

(define (delete-char-at-ith-position input position)
  (string-append (substring input 0 position)
                 (substring input (+ 1 position))))

(delete-char-at-ith-position (hello-world) 5)

(define (in input)
  (cond
    [(string? input) (string-length input)]
    [(number? input) (if (< input 1)
                         input
                         (- input 1))]
    [(false? input) 20]
    [else 10]))

(in "hello world")
(in 5)
(in 0)
(in #false)
(in #true)

(define (cvolume side-length)
  (* side-length side-length side-length))

(cvolume 5)

(define (string-is-empty? input)
  (< (string-length input)
     0))

(define (string-first input)
  (if (string-is-empty? input)
      #false
      (substring input 0 1)))

(string-first "hello")

(define (string-last input)
  (if (string-is-empty? input)
      #false
      (substring input (- (string-length input)
                          1))))

(string-last "hello")

(define (attendees ticket-price)
  (- 120
     (* (- ticket-price 5.0)
        (/ 15.0 0.1))))

(define (revenue ticket-price)
  (* ticket-price (attendees ticket-price)))

(define (cost ticket-price)
  (+ 180
     (* 0.04
        (attendees ticket-price))))

(define (profit ticket-price)
  (- (revenue ticket-price)
     (cost ticket-price)))

(define (find-max-profit ticket-price max-profit-ticket-price max-profit)
  (if (< 5 ticket-price)
      (cons max-profit-ticket-price max-profit)
      (let [(ticket-price-profit (profit ticket-price))
            (next-ticket-price (+ 0.1 ticket-price))]
        (if (< max-profit ticket-price-profit)
            (find-max-profit next-ticket-price
                             ticket-price
                             ticket-price-profit)
            (find-max-profit next-ticket-price
                             max-profit-ticket-price
                             max-profit)))))

(find-max-profit 1 0 0)












