(defpackage binomial-heap
  (:use :cl))

(defstruct bh-tree-node
  "A binomial heap tree node."
  (rank 0)
  (root nil)
  (tree nil))

(defun link (t1 t2)
  "Link two binomial heap trees."
  (if (<= (bh-tree-node-root t1) (bh-tree-node-root t2))
      (make-bh-tree-node :rank (1+ (bh-tree-node-rank t1))
                         :root (bh-tree-node-root t1)
                         :tree (cons t2 (bh-tree-node-tree t1)))
      (make-bh-tree-node :rank (1+ (bh-tree-node-rank t1))
                         :root (bh-tree-node-root t2)
                         :tree (cons t1 (bh-tree-node-tree t2)))))

(defun ins-tree (t* ts)
  "Insert a binomial heap tree t* into a binomial heap ts."
  (cond
    ((null ts) (list t*))
    ((< (bh-tree-node-rank t*) (bh-tree-node-rank (car ts))) (cons t* ts))
    (t (ins-tree (link t* (car ts)) (cdr ts)))))

(defun insert (x ts)
  "Insert a value x into a binomial heap ts."
  (ins-tree (make-bh-tree-node :root x) ts))

(defun merge-t (ts1 ts2)
  "Merge two binomial heaps."
  (cond
    ((null ts2) ts1)
    ((null ts1) ts2)
    (t
     (let* ((t1 (car ts1))
            (t2 (car ts2))
            (ts1* (cdr ts1))
            (ts2* (cdr ts2)))
       (cond
         ((< (bh-tree-node-rank t1) (bh-tree-node-rank t2))
          (cons t1 (merge-t ts1* ts2)))
         ((< (bh-tree-node-rank t2) (bh-tree-node-rank t1))
          (cons t2 (merge-t ts1 ts2*)))
         (t (ins-tree (link t1 t2) (merge-t ts1* ts2*))))))))

(defun remove-min-tree (tr)
  "Remove the minimum tree from a binomial heap, returning the minumum tree and
  the remaining heap."
  (cond
    ((null tr) nil)
    ((null (cdr tr)) tr)
    (t
     (let* ((result (remove-min-tree (cdr tr)))
            (t* (car result))
            (ts* (cdr result)))
       (if (<= (bh-tree-node-root (car tr)) (bh-tree-node-root t*))
           (cons (car tr) (cdr tr))
           (cons t* (cons (car tr) ts*)))))))

(defun find-min (ts)
  "Find the minimum value in a binomial heap ts."
  (bh-tree-node-root (car (remove-min-tree ts))))

(defun delete-min (ts)
  "Delete the minimum value in a binomial heap ts, returning the remaining heap."
  (let* ((result (remove-min-tree ts))
         (x (bh-tree-node-root (car result)))
         (ts1 (bh-tree-node-tree (car result)))
         (ts2 (cdr result)))
    (merge-t (reverse ts1) ts2)))

(defun create-random-binomial-heap (node-count &optional (heap nil))
  "Create a binomial heap of node-count trees with random root values."
  (if (zerop node-count)
      heap
      (create-random-binomial-heap (1- node-count)
                                   (insert (random 100000)
                                           heap))))
