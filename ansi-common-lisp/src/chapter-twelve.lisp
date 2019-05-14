(defpackage chapter-twelve
  (:use :cl))

(defun make-queue ()
  "Create an empty queue."
  (cons nil nil))

(defun enqueue (obj q)
  "Add an element to a queue."
  (if (null (car q))
      (setf (cdr q) (setf (car q) (list obj)))
      (setf (cdr (cdr q)) (list obj)
            (cdr q) (cdr (cdr q)))))

(defun dequeue (q)
  "Remove an element from a queue."
  (pop (car q)))

(defvar q1 (make-queue))

(progn (enqueue 'a q1)
       (enqueue 'b q1)
       (enqueue 'c q1))

(dequeue q1)
(dequeue q1)
(dequeue q1)
(dequeue q1)
