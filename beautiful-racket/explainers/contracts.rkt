#lang br

;; (define (our-div num denom)
;;   (when (zero? denom)
;;     (error "our-div: denom argument needs to be non-zero"))
;;   (/ num denom))

;; (our-div 42 0)

;; (require racket/contract)
;; (define/contract (our-div num denom)
;;   (number? (and/c number? (not/c zero?)) . -> . number?)
;;   (/ num denom))

;; (our-div 42 0)

(module divs br
  (require racket/contract)
  (provide
   (contract-out
    [int-div (integer? integer? . -> . integer?)]))
  (define (int-div num denom)
    (/ num denom))

  (with-handlers ([exn:fail? (lambda (e) 'inner-breach)])
    (displayln (int-div 42 2.5))))

(require (submod "." divs))
(with-handlers ([exn:fail? (lambda (e) 'outer-breach)])
  (displayln (int-div 42 2.5)))

(require racket/contract)
;; (define/contract (f str int)
;;   (-> string? integer? number?)
;;   42)

(define/contract (f str int)
  (string? integer? . -> . number?)
  42)

(define/contract (divisible-by-3? x)
  (any/c . -> . boolean?)
  (zero? (modulo x 3)))

(define/contract (g x)
  (divisible-by-3? . -> . integer?)
  x)

(g 6)
(g 3)
(g 4)
