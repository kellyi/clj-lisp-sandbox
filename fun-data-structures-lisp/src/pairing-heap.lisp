(defpackage pairing-heap
  (:use :cl))

(defstruct pheap
  :elem
  (:heap nil))

(defun empty-pairing-heap-p (ph)
  "Is a pairing heap empty?"
  (null ph))

(defun get-pheap-elem (ph)
  "Get a pairing heap's elem, returning nil if pairing heap is nil."
  (if (null ph)
      nil
      (pheap-elem ph)))

(defun get-pheap-heap (ph)
  "Get a pairing heaps's heap, returning nil if pairing heap is nil."
  (if (null ph)
      nil
      (pheap-heap ph)))

(defun merge-ph (ph1 ph2)
  "Merge two pairing heaps."
  (cond
    ((empty-pairing-heap-p ph1) ph2)
    ((empty-pairing-heap-p ph2) ph1)
    (t (let ((x (get-pheap-elem ph1))
             (phs1 (get-pheap-heap ph1))
             (y (get-pheap-elem ph2))
             (phs2 (get-pheap-heap ph2)))
         (if (< x y)
             (make-pheap :elem x
                         :heap (cons ph2 phs1))
             (make-pheap :elem y
                         :heap (cons ph1 phs2)))))))

(defun merge-pairs-ph (ph-list)
  "Merge the pairs in a pairing heap's heap list."
  (cond
    ((null ph-list) nil)
    ((null (cdr ph-list)) (car ph-list))
    (t
     (merge-ph
      (merge-ph (car ph-list) (cadr ph-list))
      (merge-pairs-ph (cddr ph-list))))))

(defun find-min-ph (ph)
  "Find the minimum value in a pairing heap."
  (if (empty-pairing-heap-p ph)
      nil
      (get-pheap-elem ph)))

(defun delete-min-ph (ph)
  "Delete the minimum value in a pairing heap."
  (if (empty-pairing-heap-p ph)
      nil
      (merge-pairs-ph (get-pheap-heap ph))))

(defun insert-ph (x ph)
  "Insert a new value x into a pairing heap ph."
  (merge-ph (make-pheap :elem x) ph))

(defun create-random-pairing-heap (node-count &optional (ph nil))
  "Create a pairing heap of node-count random nodes."
  (if (zerop node-count)
      ph
      (create-random-pairing-heap (1- node-count)
                                  (insert-ph (random 1000000)
                                             ph))))
