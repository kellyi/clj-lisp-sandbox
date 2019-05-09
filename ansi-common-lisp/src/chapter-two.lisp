(defpackage chapter-two
  (:use :cl))

(defun member-prime (el lst)
  "Check whether el is a member of lst."
  (if (null lst)
      nil
      (if (eql (car lst) el)
          lst
          (member-prime el (cdr lst)))))

(defun ask-question (question)
  "Print question then wait for user input answer."
  (format t "~A " question)
  (read))

(defun ask-for-number ()
  "Ask the user to enter a number. If a non number is entered, ask again."
  (format t "Please enter a number. ")
  (let ((val (read)))
    (if (numberp val)
        val
        (ask-for-number))))

(defun show-squares (start end)
  "Print all the squares from start to end."
  (do ((i start (+ i 1)))
      ((> i end) 'done)
    (format t "~A ~A~%" i (* i i))))

(defun show-squares-prime (i end)
  "Print all the squares from start to end."
  (if (> i end)
      'done
      (progn
        (format t "~A ~A~%" i (* i i))
        (show-squares-prime (+ i 1) end))))

(apply #'+ '(1 2 3))

(funcall #'+ 1 2 3)

;; Exercise 2
(cons 'a (cons 'b (cons 'c '())))
(cons 'a (cons 'b (cons 'c nil)))
(cons 'a '(b c))

;; Exercise 3
(defun fourth-prime (lst)
  "Get the fourth element of a list. Returns nil if list has fewer than four elements."
  (labels ((F (lst counter)
             (cond
               ((null lst) nil)
               ((zerop counter) (car lst))
               (t (F (cdr lst) (1- counter))))))
    (F lst 3)))

(fourth-prime '(1 2 3 4 5))
(fourth-prime nil)
(fourth-prime '(1 2 3))

;; Exercise 4
(defun greater-argument (x y)
  "Given two arguments, return whichever is greater."
  (if (> x y)
      x
      y))

;; Exercise 5
(defun M (x y)
  "Find index of x in list y."
  (if (null y)
      nil
      (if (eql (car y) x)
          0
          (let ((z (M x (cdr y))))
            (and z (+ z 1))))))

;; Exercise 7
(defun list-contains-list-p (l)
  "Check whether list l contains another list, using a constrained set of operators."
  (if (null l)
      nil
      (if (listp (car l))
          t
          (list-contains-list-p (cdr l)))))

;; Exercise 8
(defun print-dots-iterative (x)
  "Print x dots."
  (do ((i x (- i 1)))
      ((= 0 i) 'done)
    (format t ".")))

(defun print-dots-recursive (x)
  "Print x dots."
  (cond
    ((zerop x) 'done)
    (t (progn
         (format t ".")
         (print-dots-recursive (1- x))))))

(defun count-a-in-list-recursive (lst)
  "Count the occurences of 'a in lst."
  (labels ((F (lst counter)
             (cond
               ((null lst) counter)
               ((eql (car lst) 'a) (F (cdr lst) (1+ counter)))
               (t (F (cdr lst) counter)))))
    (F lst 0)))

;; Exercise 9
(defun summit (lst)
  (apply #'+ (remove nil lst)))

(defun summit-prime (lst)
  (if (null lst)
      0
      (let ((x (car lst)))
        (if (null x)
            (summit-prime (cdr lst))
            (+ x (summit-prime (cdr lst)))))))
