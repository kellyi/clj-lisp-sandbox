(defpackage sixteen
  (:use :cl))
(in-package :seasoned-schemer)

(defun sweet-tooth (food)
  (cons food
        (cons 'cake
              '())))

(sweet-tooth 'fruit)

(defparameter last-prime 'angelfood)

(defun sweet-toothL (food)
  (setq last-prime food)
  (cons food
        (cons 'cake
              '())))

(sweet-toothL 'chocolate)

last-prime

(defparameter ingredients '())

(defun sweet-toothR (food)
  (setq ingredients (cons food ingredients))
  (cons food
        (cons 'cake
              '())))

(sweet-toothR 'chocolate)
(sweet-toothR 'fruit)

ingredients

(sweet-toothR 'cheese)

ingredients

(sweet-toothR 'carrot)

ingredients

(defun deep (m)
  (cond
    ((zero? m) 'pizza)
    (else (cons (deepM (sub1 m))
                '()))))

(deep 3)

(deep 7)

(deep 0)

(defparameter Rs '())
(defparameter Ns '())

(defun deepR (n)
  (let ((result (deep n)))
    (setq Rs (cons result Rs))
    (setq Ns (cons n Ns))
    result))

;; (deepR 3)

;; Rs
;; Ns

;; (deepR 5)

;; Rs
;; Ns

;; (deepR 3)

;; Rs
;; Ns

(defun find-prime (n Ns Rs)
  (labels ((A (ns rs)
             (cond
               ((= (car ns) n) (car rs))
               (else (A (cdr ns) (cdr rs))))))
    (A Ns Rs)))

;; (find-prime 3 Ns Rs)

;; (find-prime 5 Ns Rs)

;; (find-prime 7 Ns Rs))

(defun deepM (n)
  (if (member? n Ns)
      (find-prime n Ns Rs)
      (let ((result (deep n)))
        (setq Rs (cons result Rs))
        (setq Ns (cons n Ns))
        result)))

;; (deepM 3)

;; (deepM 6)

;; (deepM 9)

;; Ns
;; Rs

;; (deepM 16)
;; Rs
;; Ns

(defun length-initial (l)
  (cond
    ((null? l) 0)
    (else (add1 (length-initial (cdr l))))))

(defun L (len)
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (funcall len (cdr l)))))))

;;(defparameter h (lambda (l) 0))

;; (defun Y! (fn)
;;   (labels ((h (funcall #'fn (arg) (funcall #'h arg))))
;;     h))

;; (deparameter length-prime (funcall Y! #'L))

(defparameter x1 0)

;; (defun biz (f)
;;   (setq x1 (add1 x1))
;;   (lambda (a)
;;     (if (= a x1)
;;         0
;;         (funcall f a))))
