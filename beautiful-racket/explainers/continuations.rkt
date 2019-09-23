;; #lang br

;; (define c #f)

;; (+ 1
;;    (+ 2
;;       (+ 3
;;          (+ (let/cc here
;;               (set! c here)
;;               4)
;;             5))))

;; (c 40)

;; (for/sum ([i (in-list '(1 2 3 4 5))])
;;   (if (= i 4)
;;       (let/cc here (set! c here) i)
;;       i))

;; (c 20)

;; (define-macro (define/return ID+ARG BODY)
;;   (with-syntax ([return (datum->syntax caller-stx 'return)])
;;     #'(define ID+ARG
;;         (let/cc return
;;           BODY))))

;; (define/return (find-random-multiple factor)
;;   (for ([num (in-list (shuffle (range 2000)))])
;;     (when (zero? (modulo num factor))
;;       (return num))))

;; (find-random-multiple 43)

#lang basic-demo
10 goto 40
20 print "appears first, prints second"
30 end
40 print "appears second, prints first"
50 goto 20
