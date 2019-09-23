#lang br

; (car 42)

(with-handlers ([exn:fail:contract? (lambda (exn) 'ignore)])
  (car 42))

(with-handlers ([exn:fail? (lambda (exn) 'got-exn-fail)]
                [integer? (lambda (exn) 'got-integer)])
  (raise (exn:fail)))

(with-handlers ([exn:fail? (lambda (exn) 'got-exn-fail)]
                [integer? (lambda (exn) 'got-integer)])
  (raise 42))

;; (define (my-div n d)
;;   (when (zero? d)
;;     (raise 'zero-division-error))
;;   (/ n d))

;; (my-div 42 0)

(define (my-div n d)
  (when (zero? d)
    (raise-argument-error 'my-div "nonzero denominator" d))
  (/ n d))

(with-handlers ([exn:fail:contract? (lambda (exn) 'undefined)])
  (my-div 42 0))

(struct exn:no-vowels exn:fail ())

(define (raise-no-vowels-error word)
  (raise (exn:no-vowels
          (format "word ~v has no vowels" word)
          (current-continuation-marks))))

(define (f word)
  (unless (regexp-match #rx"[aeiou]" word)
    (raise-no-vowels-error word))
  (displayln word))

(with-handlers ([exn:no-vowels? (lambda (exn) 'no-vowels-in-word)])
  (f "strtd"))

(struct exn:all-zero exn ())
(define (signal-all-zero)
  (raise (exn:all-zero "all zero"
                       (current-continuation-marks))))

(define (a) (b (random 10)))
(define (b a-val) (c a-val (random 10)))
(define (c a-val b-val)
  (when (andmap zero?
                (list a-val b-val (random 10)))
    (signal-all-zero)))

(with-handlers ([integer? (lambda (trial) trial)])
  (for ([trial (in-naturals)])
    (with-handlers ([exn:all-zero? (lambda (exn) (raise trial))])
      (a))))
