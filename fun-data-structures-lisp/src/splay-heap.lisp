(defpackage splay-heap
  (:use :cl))

(defstruct splay-tree
  elem
  (left nil)
  (right nil))

(defun empty-splay-tree-p (st)
  "Is a splay tree empty?"
  (null st))

(defun get-st-elem (st)
  "Get the element from a splay tree or return nil if tree is nil."
  (if (empty-splay-tree-p st)
      nil
      (splay-tree-elem st)))

(defun get-st-left (st)
  "Get the left tree from a splay tree or return nil if tree is nil."
  (if (empty-splay-tree-p st)
      nil
      (splay-tree-left st)))

(defun get-st-right (st)
  "Get the right tree from a splay tree or return nil if tree is nil."
  (if (empty-splay-tree-p st)
      nil
      (splay-tree-right st)))

(defun partition (pivot st)
  "Partition a splay tree st on pivot."
  (if (empty-splay-tree-p st)
      (values nil nil)
      (let ((a (get-st-left st))
            (b (get-st-right st))
            (x (get-st-elem st)))
        (if (<= x pivot)
            (cond
              ((empty-splay-tree-p b) (values st nil))
              ((<= (get-st-elem b) pivot)
               (multiple-value-bind (small big) (partition pivot (get-st-right b))
                 (values (make-splay-tree :elem (get-st-elem b)
                                          :left (make-splay-tree :elem x
                                                                 :left a
                                                                 :right b)
                                          :right small)
                         big)))
              (t
               (multiple-value-bind (small big) (partition pivot (get-st-left b))
                 (values (make-splay-tree :elem x
                                          :left a
                                          :right small)
                         (make-splay-tree :elem (get-st-elem b)
                                          :left big
                                          :right (get-st-right b))))))
            (cond
              ((empty-splay-tree-p a) (values nil st))
              ((<= (get-st-elem a) pivot)
               (multiple-value-bind (small big) (partition pivot (get-st-right a))
                 (values (make-splay-tree :elem (get-st-elem a)
                                          :left (get-st-left a)
                                          :right small)
                         (make-splay-tree :elem x
                                          :left big
                                          :right b))))
              (t
               (multiple-value-bind (small big) (partition pivot (get-st-left a))
                 (values small
                         (make-splay-tree :elem (get-st-elem a)
                                          :left big
                                          :right (make-splay-tree :elem x
                                                                  :left (get-st-right a)
                                                                  :right b))))))))))

(defun insert-st (x st)
  "Insert a value x into a splay tree st."
  (multiple-value-bind (a b) (partition x st)
    (make-splay-tree :elem x
                     :left a
                     :right b)))

(defun merge-st (st1 st2)
  "Merge two splay trees."
  (cond
    ((empty-splay-tree-p st1) st2)
    ((empty-splay-tree-p st2) st2)
    (t
     (let ((a (get-st-left st1))
           (b (get-st-right st1))
           (x (get-st-elem st1)))
       (multiple-value-bind (sta stb) (partition x st2)
         (make-splay-tree :elem x
                          :left (merge-st sta a)
                          :right (merge-st stb b)))))))

(defun find-min (st)
  "Find the minimum value in a splay heap."
  (cond
    ((empty-splay-tree-p st) nil)
    ((empty-splay-tree-p (get-st-left st)) (get-st-elem st))
    (t (find-min (get-st-left st)))))

(defun delete-min (st)
  "Delete the minimum value in a splay heap and return an new splay tree."
  (cond
    ((empty-splay-tree-p st) nil)
    ((empty-splay-tree-p (get-st-left st)) (get-st-right st))
    ((empty-splay-tree-p (get-st-left (get-st-left st)))
     (make-splay-tree :elem (get-st-elem st)
                      :left (get-st-right (get-st-left st))
                      :right (get-st-right st)))
    (t (make-splay-tree :elem (get-st-elem (get-st-left st))
                        :left (delete-min (get-st-left (get-st-left st)))
                        :right (make-splay-tree :elem (get-st-elem st)
                                                :left (get-st-right (get-st-left st))
                                                :right (get-st-right st))))))

(defun create-random-splay-tree (node-count &optional (st nil))
  "Create a splay tree of node-count random nodes."
  (if (zerop node-count)
      st
      (create-random-splay-tree (1- node-count)
                                (insert-st (random 100000) st))))
