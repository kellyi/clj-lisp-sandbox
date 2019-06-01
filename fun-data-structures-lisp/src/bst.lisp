(defpackage bst
  (:use :cl))

(defstruct bst-node
  "A binary search tree node."
  val
  (left nil)
  (right nil))

(defun bst-member-p (value bst)
  "Is a value present in a binary search tree?"
  (if (null bst)
      nil
      (let ((left (bst-node-left bst))
            (right (bst-node-right bst))
            (val (bst-node-val bst)))
        (cond
          ((eq value val) value)
          ((< value val) (bst-member-p value left))
          (t (bst-member-p value right))))))

(defun insert (value bst)
  "Insert a new value into a binary search tree."
  (if (null bst)
      (make-bst-node :val value)
      (let ((left (bst-node-left bst))
            (right (bst-node-right bst))
            (val (bst-node-val bst)))
        (cond
          ((eql value val) bst)
          ((< value val) (make-bst-node :left (insert value left)
                                        :val val
                                        :right right))
          (t (make-bst-node :left left
                            :val val
                            :right (insert value right)))))))

(defun create-random-bst (node-count &optional (bst nil))
  "Create a binary search tree with node-count random nodes."
  (if (zerop node-count)
      bst
      (create-random-bst (1- node-count)
                         (insert (random 10000) bst))))

(trace create-random-bst)
