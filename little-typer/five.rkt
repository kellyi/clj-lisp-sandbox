#lang pie

(claim expectations
       (List Atom))
(define expectations
  (:: 'cooked
      (:: 'eaten
          (:: 'tried-cleaning
              (:: 'understood
                  (:: 'slept nil))))))

(claim rugbrod
       (List Atom))
(define rugbrod
  (:: 'rye-flour
      (:: 'rye-kernels
          (:: 'water
              (:: 'sourdough
                  (:: 'salt nil))))))

(claim step-length
       (Pi ((E U))
           (-> E (List E) Nat
               Nat)))
(define step-length
  (lambda (E)
    (lambda (_e _es length-es)
      (add1 length-es))))

(claim length
       (Pi ((E U))
           (-> (List E)
               Nat)))
(define length
  (lambda (E)
    (lambda (es)
      (rec-List es
                0
                (step-length E)))))

(claim length-Atom
       (-> (List Atom)
           Nat))
(define length-Atom
  (length Atom))

(claim step-append
       (Pi ((E U))
           (-> E (List E) (List E)
               (List E))))
(define step-append
  (lambda (E)
    (lambda (e _es append-es)
      (:: e append-es))))

(claim append
       (Pi ((E U))
           (-> (List E) (List E)
               (List E))))
(define append
  (lambda (E)
    (lambda (start end)
      (rec-List start
                end
                (step-append E)))))

(claim snoc
       (Pi ((E U))
           (-> (List E) E
               (List E))))
(define snoc
  (lambda (E)
    (lambda (start e)
      (rec-List start
                (:: e nil)
                (step-append E)))))

(claim step-concat
       (Pi ((E U))
           (-> E (List E) (List E)
               (List E))))
(define step-concat
  (lambda (E)
    (lambda (e _es concat-es)
      (snoc E concat-es e))))

(claim concat
       (Pi ((E U))
           (-> (List E) (List E)
               (List E))))
(define concat
  (lambda (E)
    (lambda (start end)
      (rec-List end
                start
                (step-concat E)))))

(claim step-reverse
       (Pi ((E U))
           (-> E (List E) (List E)
               (List E))))
(define step-reverse
  (lambda (E)
    (lambda (e _es reverse-es)
      (snoc E reverse-es e))))

(claim reverse
       (Pi ((E U))
           (-> (List E)
               (List E))))
(define reverse
  (lambda (E)
    (lambda (es)
      (rec-List es
                nil
                (step-reverse E)))))
