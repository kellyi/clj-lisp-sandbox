(defpackage chapter-fourteen
  (:use :cl))

(defun is-even? (n) (evenp n))

(deftype even-integer ()
  `(and integer (satisfies is-even?)))

(defun left-or-right (n)
  (if (or (eq 'right n) (eq 'left n))
      t
      nil))

(left-or-right 'right)
(left-or-right 1)

(deftype either ()
  `(satisfies left-or-right))
