(defpackage natlang
  (:use :cl))

(defvar *grammar* "The grammar used by GENERATE.")

(defparameter *grammar3*
  '((Sentence -> (NP VP))
    (NP -> (Art Noun))
    (VP -> (Verb NP))
    (Art -> the)
    (Art -> a)
    (Noun -> man)
    (Noun -> ball)
    (Noun -> cat)
    (Noun -> table)
    (Noun -> noun)
    (Noun -> verb)
    (Verb -> ate)
    (Verb -> took)
    (Verb -> saw)
    (Verb -> liked)))

(setf *grammar* *grammar3*)

(defstruct (rule (:type list)) lhs -> rhs)

(defstruct (parse) "A parse tree and a remainder." tree rem)

(defun new-tree (cat rhs) (cons cat rhs))

(defun tree-lhs (tree) (first tree))
(defun tree-rhs (tree) (rest tree))

(defun parse-lhs (parse) (tree-lhs (parse-tree parse)))


(defun find-all (item sequence &rest keyword-args
                 &key (test #'eql) test-not &allow-other-keys)
  "Find all those elements of sequence that match item,according to the
  keywords. Doesn't alter sequence."
  (if test-not
      (apply #'remove item sequence
             :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence
             :test (complement test) keyword-args)))

(setf (symbol-function 'find-all-if) #'remove-if-not)

(defun lexical-rules (word)
  "Return a list of rules with word on the right-hand side."
  (or (find-all word *grammar* :key #'rule-rhs :test #'equal)
      (mapcar #'(lambda (cat) `(,cat -> ,word)) *open-categories*)))

(defun first-or-nil (x)
  "The first element of x if it is a list else nil."
  (if (consp x) (first x) nil))

(Defun rules-starting-with (cat)
  "Return a list of rules where cat starts with the rhs."
  (find-all cat *grammar*
            :key #'(lambda (rule) (first-or-nil (rule-rhs rule)))))

(defun complete-parses (parses)
  "Those parses that are complete (have no remainder)."
  (find-all-if #'null parses :key #'parse-rem))

(defun append1 (items item)
  "Add item to end of list of items."
  (append items (list item)))

(defun extend-parse (lhs rhs rem needed)
  "Look for the categories needed to complete the parse."
  (if (null needed)
      (let ((parse (make-parse :tree (new-tree lhs rhs) :rem rem)))
        (cons parse
              (mapcan
               #'(lambda (rule)
                   (extend-parse (rule-lhs rule)
                                 (list (parse-tree parse))
                                 rem (rest (rule-rhs rule))))
               (rules-starting-with lhs))))
      (mapcan
       #'(lambda (p)
           (if (eq (parse-lhs p) (first needed))
               (extend-parse lhs (append1 rhs (parse-tree p))
                             (parse-rem p) (rest needed))))
       (parse rem))))

(defun parse (words)
  "Bottom-up parse, returning all parses of any prefix of words."
  (unless (null words)
    (mapcan #'(lambda (rule)
                (extend-parse (rule-lhs rule) (list (first words))
                              (rest words) nil))
            (lexical-rules (first words)))))

(memoize 'lexical-rules)
(memoize 'rules-starting-with)
(memoize 'parse :test #'eq)

(defun parser (words)
  "Return all complete parses of a list of words."
  (clear-memoize 'parse)
  (mapcar #'parse-tree (complete-parses (parse words))))

(parser '(the table))

(parser '(the noun took the verb))

(defparameter *grammar4*
  '((S -> (NP VP))
    (NP -> (D N))
    (NP -> (D A+ N))
    (NP -> (NP PP))
    (NP -> (Pro))
    (NP -> (Name))
    (VP -> (V NP))
    (VP -> (V))
    (VP -> (VP PP))
    (PP -> (P NP))
    (A+ -> (A))
    (A+ -> (A A+))
    (Pro -> I)
    (Pro -> you)
    (Pro -> he)
    (Pro -> she)
    (Pro -> it)
    (Pro -> me)
    (Pro -> him)
    (Pro -> her)
    (Name -> John)
    (Name -> Mary)
    (A -> big)
    (A -> little)
    (A -> striped)
    (A -> plaid)
    (A -> blue)
    (A -> green)
    (A -> orange)
    (A -> perspicuous)
    (D -> the)
    (D -> a)
    (D -> an)
    (N -> man)
    (N -> ball)
    (N -> cat)
    (N -> table)
    (N -> orange)
    (N -> saw)
    (N -> saws)
    (N -> noun)
    (N -> verb)
    (P -> with)
    (P -> for)
    (P -> at)
    (P -> on)
    (P -> by)
    (P -> of)
    (P -> in)
    (V -> ate)
    (V -> took)
    (V -> saw)
    (V -> liked)
    (V -> saws)))

(setf *grammar* *grammar4*)

(parser '(The man ate the table with the striped ball))

(parser '(The orange cat saw the man with the orange))

(defun memo (fn &key (key #'first) (test #'eql) name)
  "Return a memo-function of fn."
  (let ((table (make-hash-table :test test)))
    (setf (get name 'memo) table)
    #'(lambda (&rest args)
        (let ((k (funcall key args)))
          (multiple-value-bind (val found-p)
              (gethash k table)
            (if found-p val
                (setf (gethash k table) (apply fn args))))))))

(defun clear-memoize (fn-name)
  "Clear the hash table from a memo function."
  (let ((table (get fn-name 'memo)))
    (when table (clrhash table))))

(defun memoize (fn-name &key (key #'first) (test #'eql))
  "Replace fn-name's global definition with a memoized version."
  (clear-memoize fn-name)
  (setf (symbol-function fn-name)
        (memo (symbol-function fn-name)
              :name fn-name :key key :test test)))

;; (defun rewrites (category)
;;   "Return a list of possible rewrites for this category."
;;   (rule-rhs (assoc category *grammar*)))

;; (defun random-elt (choices)
;;   "Choose an element at random from a list."
;;   (elt choices (random (length choices))))

;; (defun mappend (fn the-list)
;;   "Apply fn to each element of the list and append the result."
;;   (if (null the-list)
;;       nil
;;       (append (funcall fn (first the-list))
;;               (mappend fn (rest the-list)))))

;; (defun generate (phrase)
;;   "Generate a random sentence or phrase"
;;   (cond ((listp phrase)
;;          (mappend #'generate phrase))
;;         ((rewrites phrase)
;;          (generate (random-elt (rewrites phrase))))
;;         (t (list phrase))))

(defun use (grammar)
  "Switch to a new grammar."
  (clear-memoize 'rules-starting-with)
  (clear-memoize 'lexical-rules)
  (length (setf *grammar* grammar)))

(defparameter *open-categories* '(N V A Name)
  "Categories to consider for unknown words.")

(parser '(John liked orange))
