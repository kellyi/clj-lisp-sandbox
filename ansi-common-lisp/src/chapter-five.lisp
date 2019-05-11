(defpackage chapter-five
  (:use :cl))

(progn
  (format t "a")
  (format t "b")
  (+ 11 12))

(values 'a nil (+ 2 4))

((lambda () (values 1 2)))

(let ((x (values 1 2)))
  (format t "~a" x))

(multiple-value-call #'+ (values 1 2 3))

(multiple-value-list (values 1 2 3))

;; Exercise 3
(defun square-of-n-if-gt-5 (n)
  "Return the square of n if n is greater than 5."
  (if (> n 5)
    (* n n)
    nil))

;; Exercise 5
(defun precedes (element lst)
  "Return a list of all items immediately preceding element in lst."
  (labels ((P (e l acc)
             (cond
               ((or (null (car l))
                    (null (car (cdr l)))) acc)
               ((or (not (eq (car (cdr l)) e))
                    (member (car l) acc)) (P e (cdr l) acc))
               (t (P e
                     (cdr l)
                     (cons (car l) acc))))))
    (P element lst '())))

;; Exercise 6
(defun intersperse (element lst)
  "Intersperses element between each item in lst."
  (reduce #'(lambda (next acc)
              (cond
                ((null acc) (cons next acc))
                (t (cons next (cons element acc)))))
          lst
          :initial-value '()
          :from-end t))

;; Exercise 7
(defun successive-pair-diffs-are-one (lst)
  "Returns true if the difference between each successive pair in a list is 1."
  (labels ((diff-is-one (x y)
             (cond
               ((= (1- x) y) t)
               ((= (1+ x) y) t)
               (t nil)))
           (S (l)
             (cond
               ((or (null (car l))
                    (null (car (cdr l)))) t)
               ((not (diff-is-one (car l) (car (cdr l)))) nil)
               (t (S (cdr l))))))
    (if (null lst)
        nil
        (S lst))))
