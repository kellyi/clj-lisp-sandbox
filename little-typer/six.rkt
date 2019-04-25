#lang pie

(claim first-of-one
       (Pi ((E U))
           (-> (Vec E 1)
               E)))
(define first-of-one
  (lambda (E)
    (lambda (es)
      (head es))))

(claims first-of-two
        (Pi ((E U))
            (-> (Vec E 2)
                E)))
(define first-of-two
  (lambda (E)
    (lambda (es)
      (head es))))

(claim first
       (Pi ((E U)
            (l Nat))
           (-> (Vec E (add1 l))
               E)))
(define first
  (lambda (E l)
    (lambda (es)
      (head es))))

(claim rest
       (Pi ((E U)
            (l Nat))
           (-> (Vec E (add1 l))
               (Vec E l))))
(define rest
  (lambda (E l)
    (lambda (es)
      (tail es))))
