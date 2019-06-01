(defpackage leftist-heap
  (:use :cl))

(defstruct lh-node
  "A leftist heap node."
  val
  (rank 1)
  (left nil)
  (right nil))

(defun lh-empty-p (lh)
  "Is a leftist heap empty?"
  (null lh))

(defun find-min (lh)
  "Find the minimum value in a leftist heap."
  (lh-node-val lh))

(defun get-rank-or-use-default (lh &optional (default-rank 1))
  "Get the rank of a leftist heap node, defaulting to default-rank if node is empty."
  (if (lh-empty-p lh)
      default-rank
      (lh-node-rank lh)))

(defun make-t (x left right)
  "Create a tree from a value x and two leftist heap nodes, left & right."
  (if (>= (get-rank-or-use-default left) (get-rank-or-use-default right))
      (make-lh-node :val x
                    :rank (1+ (get-rank-or-use-default right))
                    :left left
                    :right right)
      (make-lh-node :val x
                    :rank (1+ (get-rank-or-use-default left))
                    :left right
                    :right left)))

(defun merge-t (h1 h2)
  "Merge two leftist trees."
  (cond
    ((null h1) h2)
    ((null h2) h1)
    (t (let ((x (lh-node-val h1))
             (a1 (lh-node-left h1))
             (b1 (lh-node-right h1))
             (y (lh-node-val h2))
             (a2 (lh-node-left h2))
             (b2 (lh-node-right h2)))
         (if (<= x y)
             (make-t x a1 (merge-t b1 h2))
             (make-t y a2 (merge-t h1 b2)))))))

(defun delete-min (lh)
  "Delete the minumum value and return a new merged leftist heap."
  (merge-t (lh-node-left lh) (lh-node-right lh)))

(defun insert (x h)
  "Insert a value x into a leftist heap h."
  (merge-t (make-lh-node :rank 1 :val x) h))

(defun create-random-leftist-tree (node-count &optional (leftist-tree nil))
  "Create a leftist tree with node-count random nodes."
  (if (zerop node-count)
      leftist-tree
      (create-random-leftist-tree (1- node-count)
                                  (insert (random 100000) leftist-tree))))
