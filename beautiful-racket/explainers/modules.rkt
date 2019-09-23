#lang br

(module my-module br
  (* 6 7))

(module mod-name br
  (format-datum '(hello world)))

(module tired br
  (sleep 10000)
  (module awake br
    (define greeting "good morning")
    (provide greeting)
    (module also-tired br
      (sleep 10000))))

(require (submod "." tired awake))
greeting

(provide launch-rocket)
(define (launch-rocket) (displayln "whee"))
(displayln "4, 3, 2, 1...")
(module+ main
  (launch-rocket))
