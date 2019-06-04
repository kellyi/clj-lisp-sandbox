(defpackage red-black-tree
  (:use :cl))

(defvar red 'red)
(defvar black 'black)

(defstruct rb-tree-node
  color
  (left nil)
  elem
  (right nil))

(defun get-left (rbt)
  "Get the left tree of a red black tree, returning nil if the tree is nil."
  (if (null rbt)
      nil
      (rb-tree-node-left rbt)))

(defun get-right (rbt)
  "Get the right tree of a red black tree, returning nil if the tree is nil."
  (if (null rbt)
      nil
      (rb-tree-node-right rbt)))

(defun get-elem (rbt)
  "Get the element of a red black tree, returning nil if the tree is nil."
  (if (null rbt)
      nil
      (rb-tree-node-elem rbt)))

(defun get-color (rbt)
  "Get the color of a red black tree node, returning nil if node is nil."
  (if (null rbt)
      nil
      (rb-tree-node-color rbt)))

(defun member-rb (x rb-t)
  "Is x a member of tree rb-t?"
  (cond
    ((null rb-t) nil)
    ((< x (get-elem rb-t)) (member-rb x (get-left rb-t)))
    ((< (get-elem rb-t) x) (member-rb x (get-right rb-t)))
    (t t)))

(defun balance-rb (rb-tree)
  "Balance a red-black tree."
  (cond
    ((and
      (eql red (get-color (get-left rb-tree)))
      (eql red (get-color (get-left
                                  (get-left rb-tree)))))
     (make-rb-tree-node :color red
                        :elem (get-elem (get-left rb-tree))
                        :left (make-rb-tree-node :color black
                                                 :elem (get-elem
                                                        (get-left
                                                         (get-left
                                                          rb-tree)))
                                                 :left (get-left
                                                        (get-left
                                                         (get-left
                                                          rb-tree)))
                                                 :right (get-right
                                                         (get-left
                                                          (get-left
                                                           rb-tree))))
                        :right (make-rb-tree-node :color black
                                                  :elem (get-elem rb-tree)
                                                  :left (get-right
                                                         (get-left
                                                          rb-tree))
                                                  :right (get-right rb-tree))))
    ((and
      (eql red (get-color (get-left rb-tree)))
      (eql red (get-color (get-right
                                  (get-left rb-tree)))))
     (make-rb-tree-node :color red
                        :elem (get-elem
                               (get-right
                                (get-left
                                 rb-tree)))
                        :left (make-rb-tree-node :color black
                                                 :elem (get-elem
                                                        (get-left
                                                         rb-tree))
                                                 :left (get-left
                                                        (get-left
                                                         rb-tree))
                                                 :right (get-left
                                                         (get-right
                                                          (get-left
                                                           rb-tree))))
                        :right (make-rb-tree-node :color black
                                                  :elem (get-elem rb-tree)
                                                  :left (get-right
                                                         (get-right
                                                          (get-left
                                                           rb-tree)))
                                                  :right (get-right rb-tree))))
    ((and
      (eql red (get-color (get-right rb-tree)))
      (eql red (get-color (get-left
                                  (get-right rb-tree)))))
     (make-rb-tree-node :color red
                        :elem (get-elem
                               (get-left
                                (get-right
                                 rb-tree)))
                        :left (make-rb-tree-node :color black
                                                 :elem (get-elem rb-tree)
                                                 :left (get-left rb-tree)
                                                 :right (get-left
                                                         (get-left
                                                          (get-right
                                                           rb-tree))))
                        :right (make-rb-tree-node :color black
                                                  :elem (get-elem
                                                         (get-right
                                                          rb-tree))
                                                  :left (get-right
                                                         (get-left
                                                          (get-right
                                                           rb-tree)))
                                                  :right (get-right
                                                          (get-right
                                                           rb-tree)))))
    ((and
      (eql red (get-color (get-right rb-tree)))
      (eql red (get-color (get-right
                                  (get-right rb-tree)))))
     (make-rb-tree-node :color red
                        :elem (get-elem (get-right rb-tree))
                        :left (make-rb-tree-node :color black
                                                 :elem (get-elem rb-tree)
                                                 :left (get-left rb-tree)
                                                 :right (get-left
                                                         (get-right
                                                          rb-tree)))
                        :right (make-rb-tree-node :color black
                                                  :elem (get-elem
                                                         (get-right
                                                          (get-right
                                                           rb-tree)))
                                                  :left (get-left
                                                         (get-right
                                                          (get-right
                                                           rb-tree)))
                                                  :right (get-right
                                                          (get-right
                                                           (get-right
                                                            rb-tree))))))
    (t rb-tree)))

(defun insert (x s)
  "Insert a new value x into a red/black tree s."
  (labels ((ins (s*)
             (cond
               ((null s*) (make-rb-tree-node :color red
                                             :elem x))
               ((< x (get-elem s*)) (balance-rb
                                              (make-rb-tree-node :color (get-color s*)
                                                                 :elem (get-elem s*)
                                                                 :left (ins
                                                                        (get-left
                                                                         s*))
                                                                 :right (get-right s*))))
               ((> x (get-elem s*)) (balance-rb
                                              (make-rb-tree-node :color (get-color s*)
                                                                 :elem (get-elem s*)
                                                                 :left (get-left s*)
                                                                 :right (ins
                                                                         (get-right
                                                                          s*)))))
               (t s*))))
    (let ((s-double-* (ins s)))
      (make-rb-tree-node :color black
                         :elem (get-elem s-double-*)
                         :left (get-left s-double-*)
                         :right (get-right s-double-*)))))

(defun create-random-red-black-tree (node-count &optional (rb-tree nil))
  "Create a red-black tree of node-count nodes with random values."
  (if (zerop node-count)
      rb-tree
      (create-random-red-black-tree (1- node-count)
                                    (insert (random 100000) rb-tree))))
