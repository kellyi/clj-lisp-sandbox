(defpackage chapter-three
  (:use :cl))

(defun n-elts (element count)
  "Create a pair of count . element if count is greater than one.
  Otherwise, return the element."
  (if (> count 1)
      (list count element)
      element))

(defun compress (x)
  "Compress x with run length encoding."
  (labels ((compr (elt n lst)
             (if (null lst)
                 (list (n-elts elt n))
                 (let ((next (car lst)))
                   (if (eql next elt)
                       (compr elt (+ n 1) (cdr lst))
                       (cons (n-elts elt n)
                             (compr next 1 (cdr lst))))))))
    (if (consp x)
        (compr (car x) 1 (cdr x))
        x)))

(defun list-of (count element)
  "Create a list of count elements, returning nil if count is zero."
  (if (zerop count)
      nil
      (cons element (list-of (1- count) element))))

(defun uncompress (lst)
  "Uncompress a run length encoded list."
  (if (null lst)
      nil
      (let ((elt (car lst))
            (rest (uncompress (cdr lst))))
        (if (consp elt)
            (append (apply #'list-of elt)
                    rest)
            (cons elt rest)))))

(defvar *min* '((a b c) (b c) (c d)))

(defun new-paths (path node net)
  (mapcar #'(lambda (n)
              (cons n path))
          (cdr (assoc node net))))

(defun shortest-path (start finish network)
  "Find the shortest path between start and finish through network."
  (labels ((BFS (end queue net)
             (if (null queue)
                 nil
                 (let ((path (car queue)))
                   (let ((node (car path)))
                     (if (eql node end)
                         (reverse path)
                         (BFS end
                              (append (cdr queue)
                                      (new-paths path node net))
                              net)))))))
    (BFS finish (list (list start)) network)))

;; Exercise 2
(defun new-union (one two)
  "Union between list one and two, preserving the order of elements."
  (labels ((U (l new-list)
             (cond
               ((null l) new-list)
               ((member (car l) new-list) (U (cdr l) new-list))
               (t (U (cdr l) (cons (car l) new-list))))))
    (reverse (U (concatenate 'list one two) '()))))

;; Exercise 3
(defun occurences (lst)
  "Return a list of dotted pairs indicating the number of times each element
  occurs in the list, sorted from most to fewest."
  (labels ((count-atom-in-list (a l acc)
             (cond
               ((null (car l)) acc)
               ((eql a (car l)) (count-atom-in-list a (cdr l) (1+ acc)))
               (t (count-atom-in-list a (cdr l) acc)))))
    (sort
     (reduce #'(lambda (next acc)
                 (cond
                   ((assoc next acc) acc)
                   (t (acons
                       next
                       (count-atom-in-list next lst 0)
                       acc))))
             lst
             :initial-value '()
             :from-end t)
     #'>
     :key #'cdr)))

;; Exercise 8
(defun showdots (lst)
  "Given a list, print the list using cons dot notation."
  (if (null (car lst))
      (format t "nil)")
      (progn
        (format t "(~a . " (car lst))
        (showdots (cdr lst)))))
