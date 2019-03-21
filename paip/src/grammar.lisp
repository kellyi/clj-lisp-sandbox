(defpackage paip
  (:use :cl))
(in-package :paip)

(defun random-elt (choices)
  "Choose an element from a list at random"
  (elt choices (random (length choices))))

(defun one-of (set)
  "Pick one element from a set and make a list of it"
  (list (random-elt set)))

(defparameter *simple-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Article -> the a)
    (Noun -> man ball cat table)
    (Verb -> hit took saw liked))
  "A grammar for a trivial subset of English")

(defvar *grammar* *simple-grammar*
  "The grammar used by `generate`")

(defun rule-lhs (rule)
  "The left-hand side of a rule"
  (first rule))

(defun rule-rhs (rule)
  "The right-hand side of a rule"
  (rest (rest rule)))

(defun rewrites (category)
  "Return a list of the possible rewrites for this category"
  (rule-rhs (assoc category *grammar*)))

(defun generate (phrase)
  "Generate a random sentence or phrase"
  (cond
    ((listp phrase) (mappend #'generate phrase))
    ((rewrites phrase) (generate (random-elt (rewrites phrase))))
    (t (list phrase))))

(defparameter *bigger-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Adj* Noun PP*) (Name) (Pronoun))
    (verb-phrase -> (Verb noun-phrase PP*))
    (PP* -> () (PP PP*))
    (Adj* -> () (Adj Adj*))
    (PP -> (Prep noun-phrase))
    (Prep -> to in by with on)
    (Adj -> big little blue green adiabatic)
    (Article -> the a)
    (Name -> Pat Kim Lee Terry Robin)
    (Noun -> man ball cat table)
    (Verb -> ate took saw liked)
    (Pronoun -> he she it these those that)))

(setf *grammar* *bigger-grammar*)

(defun generate-tree (phrase)
  "Generate a random sentence or phrase with a complete phrase tree"
  (cond
    ((listp phrase) (mapcar #'generate-tree phrase))
    ((rewrites phrase) (cons phrase
                             (generate-tree (random-elt (rewrites phrase)))))
    (t (list phrase))))

(defun combine-all (xlist ylist)
  "Return a list of list formed by appending a y to an x"
  (mappend #'(lambda (y)
               (mapcar #'(lambda (x) (append x y)) xlist))
           ylist))

(defun generate-all (phrase)
  "Generate a list of all possible expansions of this phrase"
  (cond
    ((null phrase) (list nil))
    ((listp phrase) (combine-all (generate-all (first phrase))
                                 (generate-all (rest phrase))))
    ((rewrites phrase) (mappend #'generate-all (rewrites phrase)))
    (t (list (list phrase)))))

(setf *grammar* *simple-grammar*)

(generate-all 'sentence)
