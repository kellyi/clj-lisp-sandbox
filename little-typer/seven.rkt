#lang pie

;; A type that is determined by something that is not a type is called a
;; dependent type.

(claim mot-peas
       (-> Nat
           U))
(define mot-peas
  (lambda (k)
    (Vec Atom k)))

(claim step-peas
       (Pi ((l-1 Nat))
           (-> (mot-peas l-1)
               (mot-peas (add1 l-1)))))
(define step-peas
  (lambda (l-1)
    (lambda (peas-l-1)
      (vec:: 'pea peas-l-1))))

(claim peas
       (Pi ((how-many-peas Nat))
           (Vec Atom how-many-peas)))
(define peas
  (lambda (how-many-peas)
    (ind-Nat how-many-peas
             mot-peas
             vecnil
             step-peas)))

(claim also-rec-Nat
       (Pi ((X U))
           (-> Nat
               X
               (-> Nat X
                   X)
               X)))
(define also-rec-Nat
  (lambda (X)
    (lambda (target base step)
      (ind-Nat target
               (lambda (k)
                 X)
               base
               step))))

(claim base-last
       (Pi ((E U))
           (-> (Vec E (add1 zero))
               E)))
(define base-last
  (lambda (E)
    (lambda (es)
      (head es))))

(claim mot-last
       (-> U Nat
           U))
(define mot-last
  (lambda (E k)
    (-> (Vec E (add1 k))
        E)))

(claim step-last
       (Pi ((E U)
            (l-1 Nat))
           (-> (mot-last E l-1)
               (mot-last E (add1 l-1)))))
(define step-last
  (lambda (E l-1)
    (lambda (last-l-1)
      (lambda (es)
        (last-l-1 (tail es))))))

(claim last
       (Pi ((E U)
            (l Nat))
           (-> (Vec E (add1 l))
               E)))
(define last
  (lambda (E l)
    (ind-Nat l
             (mot-last E)
             (base-last E)
             (step-last E))))

(claim base-drop-last
       (Pi ((E U))
           (-> (Vec E (add1 zero))
               (Vec E zero))))
(define base-drop-last
  (lambda (E)
    (lambda (es)
      vecnil)))

(claim mot-drop-last
       (-> U Nat
           U))
(define mot-drop-last
  (lambda (E k)
    (-> (Vec E (add1 k))
        (Vec E k))))

(claim step-drop-last
       (Pi ((E U)
            (l-1 Nat))
           (-> (mot-drop-last E l-1)
               (mot-drop-last E (add1 l-1)))))
(define step-drop-last
  (lambda (E l-1)
    (lambda (drop-last-l-1)
      (lambda (es)
        (vec:: (head es)
               (drop-last-l-1 (tail es)))))))

(claim drop-last
       (Pi ((E U)
            (l Nat))
           (-> (Vec E (add1 l))
               (Vec E l))))
(define drop-last
  (lambda (E l)
    (ind-Nat l
             (mot-drop-last E)
             (base-drop-last E)
             (step-drop-last E))))
