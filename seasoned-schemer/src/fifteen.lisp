(defpackage fifteen
  (:use :cl))
(in-package :seasoned-schemer)

(defparameter *initial-x* (cons 'chicago
                        (cons 'pizza
                              '())))

(defparameter x 'skins)

(defparameter x 'rings)

(defun gourmet (food)
  (cons food
        (cons x '())))

(gourmet 'onion)

(defun gourmand (food)
  (setq x food)
  (cons food
        (cons x '())))

(gourmand 'potato)

(defun diner (food)
  (cons 'milkshake
        (cons food
              '())))

(diner 'onion)

(defun dinerR (food)
  (setq x food)
  (cons 'milkshake
        (cons food
              '())))

(dinerR 'pecanpie)

(gourmand 'onion)

x

(defun omnivore (food)
  (let ((x 'ministrone))
    (setq x food)
    (cons food
          (cons x
                '()))))

(omnivore 'bouillabaisse)

(defun gobbler (food)
  (let ((x 'ministrone))
    (setq x food)
    (cons food
          (cons x
                '()))))

(gobbler 'gumbo)

(defun nibbler (food)
  (let ((x 'donut))
    (setq x food)
    (cons food
          (cons x
                '()))))

(nibbler 'cheerio)

(defparameter food 'none)

(defun glutton (x)
  (setq food x)
  (cons 'more
        (cons x
              (cons 'more
                    (cons x
                          '())))))

(glutton 'garlic)

(defun chez-nous ()
  (setq food x)
  (setq x food))

(chez-nous)

food
x

(defun chez-nous-prime ()
  (let ((a food))
    (setq food x)
    (setq x a)))

(glutton 'garlic)

(gourmand 'potato)

(chez-nous-prime)

food
x
x
