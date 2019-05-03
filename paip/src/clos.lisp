(defpackage clos
  (:use :cl))
(in-package :paip)

(defclass account ()
  ((name :initarg :name :reader name)
   (balance :initarg :balance :initform 0.00 :accessor balance)
   (interest-rate :allocation :class :initform .06
                  :reader interest-rate)))

(defvar a1 nil)
(setf a1 (make-instance 'account :balance 5000.00
                        :name "Fred"))

(name a1)
(balance a1)
(interest-rate a1)

(defmethod withdraw ((acct account) amt)
  (if (< amt (balance acct))
      (decf (balance acct) amt)
      'insufficient-funds))

(defclass audited-account (account)
  ((audit-trail :initform nil :accessor audit-trail)))

(defmethod withdraw :before ((acct audited-account) amt)
  (push (print `(withdrawing ,amt))
        (audit-trail acct)))

(defmethod withdraw :after ((acct audited-account) amt)
  (push (print `(widthdrawal (,amt) done))
        (audit-trail acct)))

(defvar a3 nil)
(setf a3 (make-instance 'audited-account :balance 1000.00))

(withdraw a3 100.00)

(withdraw a3 90.00)

(audit-trail a3)

(defclass problem ()
  ((states :initarg :states :accessor problem-states)))

(defmethod current-state ((prob problem))
  "The current state is the first of all possible states."
  (first (problem-states prob)))

(defmethod pop-state ((prob problem))
  "Remove and return the current state."
  (pop (problem-states prob)))

(defmethod no-states-p ((prob problem))
  "Are there any more unexplored states?"
  (null (problem-states prob)))

(defmethod searcher ((prob problem))
  "Find a state that solves the search problem."
  (cond ((no-states-p prob) fail)
        ((goal-p prob) (current-state prob))
        (t (let ((current (pop-state prob)))
             (setf (problem-states prob)
                   (problem-combiner
                    prob
                    (problem-successors prob current)
                    (problem-states prob))))
           (searcher prob))))

(defclass eql-problem (problem)
  ((goal :initarg :goal :reader problem-goal)))

(defmethod goal-p ((prob eql-problem))
  (eql (current-state prob) (problem-goal prob)))

(defclass dfs-problem (problem) ()
  (:documentation "Depth-first search problem."))

(defclass bfs-problem (problem) ()
  (:documentation "Breadth-first search problem."))

(defmethod problem-combiner ((prob dfs-problem) new old)
  "Depth-first search looks at new states first."
  (append new old))

(defmethod problem-combiner ((prob bfs-problem) new old)
  "Breadth-first search looks at old states first."
  (append old new))

(defclass binary-tree-problem (problem) ())

(defmethod problem-successors ((prob binary-tree-problem) state)
  (let ((n (* 2 state)))
    (list n (+ n 1))))

(defclass binary-tree-eql-bfs-problem
    (binary-tree-problem eql-problem bfs-problem) ())

(defvar p1 (make-instance 'binary-tree-eql-bfs-problem
                          :states '(1) :goal 12))

(searcher p1)

(defclass best-problem (problem) ()
  (:documentation "A best-first search problem."))

(defmethod problem-combiner ((prob best-problem) new old)
  "Best-first search sorts new and old according to cost-fn."
  (sort (append new old) #'<
        :key #'(lambda (state) (cost-fn prob state))))

(defmethod cost-fn ((prob eql-problem) state)
  (abs (- state (problem-goal prob))))

(defclass beam-problem (problem)
  ((beam-width :initarg :beam-width :initform nil
               :reader problem-beam-width)))

(defmethod problem-combiner :around ((prob beam-problem) new old)
  (let ((combined (call-next-method)))
    (subseq combined 0 (min (problem-beam-width prob)
                            (length combined)))))

(defclass binary-tree-eql-best-beam-problem
  (binary-tree-eql-bfs-problem best-problem beam-problem)
  ())

(defvar p3 (make-instance 'binary-tree-eql-best-beam-problem
                          :states '(1) :goal 12 :beam-width 3))

(searcher p3)
