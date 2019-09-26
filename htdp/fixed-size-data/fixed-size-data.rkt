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

(define price-sensitivity (/ 15.0 0.1))

(define (attendees ticket-price)
  (- 120
     (* (- ticket-price 5.0)
        price-sensitivity)))

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

(require 2htdp/batch-io)

;; (write-file "sample.dat" "212")
;; (read-file "sample.dat")

;; (write-file 'stdout "212\n")

(define (C f)
  (* 5/9 (- f 32)))

(C 32)
(C 212)
(C -40)

(define (convert in out)
  (write-file out
              (string-append
               (number->string
                (C
                 (string->number
                  (read-file in))))
               "\n")))

;; (write-file "sample.dat" "212")
;; (convert "sample.dat" 'stdout)
;; (convert "sample.dat" "out.dat")
;; (read-file "out.dat")

(define (letter fst lst signature-name)
  (string-append
   (opening fst)
   "\n\n"
   (body fst lst)
   "\n\n"
   (closing signature-name)))

(define (opening fst)
  (string-append "Dear " fst ","))

(define (body fst lst)
  (string-append
   "We have discovered that all people with the\n"
   "last name "
   lst
   " have won our lottery. So, \n"
   fst
   ", hurry and pick up your prize."))

(define (closing signature-name)
  (string-append
   "Sincerely,\n\n"
   signature-name
   "\n"))

(letter "Kelly" "Innes" "Kelly Innes")

(define (write-letter in-fst in-lst in-signature out)
  (write-file out
              (letter (read-file in-fst)
                      (read-file in-lst)
                      (read-file in-signature))))

(write-file "first-name.dat" "John")
(write-file "last-name.dat" "Smith")
(write-file "signature.dat" "John Smith")
(write-letter "first-name.dat" "last-name.dat" "signature.dat" 'stdout)
