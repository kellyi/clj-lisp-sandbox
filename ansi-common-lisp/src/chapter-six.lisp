(defpackage chapter-six
  (:use :cl))

(fboundp '+)

(symbol-function '+)

(defun philosoph (thing &optional property)
  (list thing 'is property))

(defun philosoph* (thing &optional (property 'fun))
  (list thing 'is property))

(defun keylist (a &key x y z)
  (list a x y z))

(keylist 'hello :y 'world)

(defun single? (lst)
  "Does lst have only a single element?"
  (and (consp lst) (null (cdr lst))))

(defun append1 (lst obj)
  "Append obj to lst."
  (append lst `(,obj)))

(defun map-int (fn n)
  "Create a list resulting from calling fn n times."
  (let ((acc nil))
    (dotimes (i n)
      (push (funcall fn i) acc))
    (nreverse acc)))

(defun filter (fn lst)
  "Filter lst by fn."
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val (push x acc))))
    (nreverse acc)))

(defun most (fn lst)
  "Return the element of lst with the highest score computed by fn."
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
             (max (funcall fn wins)))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (when (> score max)
              (setf wins obj
                    max score))))
        (values wins max))))

(defun combiner (x)
  "Choose appropriate combiner based on x's type."
  (typecase x
    (number #'+)
    (list #'append)
    (t #'list)))

(defun combine (&rest args)
  "Combine arguments appropriately based on their type."
  (apply (combiner (car args))
         args))

(defun foo (x) (1+ x))

(compiled-function-p #'foo)

;; Exercise 3
(defun count-arguments (&rest args)
  "Count the number of given arguments."
  (length args))

;; Exercise 6
(let ((max nil))
  (defun greatest-arg (x)
    "Return the greatest arg passed to this function so far."
    (cond
      ((null max) (progn
                    (setf max x)
                    x))
      ((> x max) (progn
                   (setf max x)
                   x))
      (t max))))

;; Exercise 7
(let ((last-arg nil))
  (defun greater-than-last-arg (x)
    "Returns true if x is greater than the last argument passed to this function."
    (cond
      ((null last-arg) (progn
                         (setf last-arg x)
                         nil))
      ((> x last-arg) (progn
                        (setf last-arg x)
                        t))
      (t (progn
           (setf last-arg x)
           nil)))))
