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

(defstruct (rule (:type list)
                 (:constructor
                  rule (lhs -> rhs &optional sem score)))
  lhs -> rhs sem score)

(defstruct (tree (:type list) (:include rule) (:copier nil)
                 (:constructor new-tree (lhs sem score rhs))))

(defstruct (parse) "A parse tree and a remainder." tree rem)

;; (defun new-tree (cat rhs) (cons cat rhs))

;; (defun tree-lhs (tree) (first tree))
;; (defun tree-rhs (tree) (rest tree))

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

(defun length=1 (x)
  "Is x a list of length 1?"
  (and (consp x) (null (cdr x))))

(defun terminal-tree-p (tree)
  "Does this tree have a single word on the rhs?"
  (and (length=1 (tree-rhs tree))
       (atom (first (tree-rhs tree)))))

(defun apply-semantics (tree)
  "For terminal nodes, just fetch the semantics. Otherwise apply the sem
  function to its constituents."
  (if (terminal-tree-p tree)
      (tree-sem tree)
      (setf (tree-sem tree)
            (apply (tree-sem tree)
                   (mapcar #'tree-sem (tree-rhs tree))))))

(defun tree-score-or-0 (tree)
  (if (numberp (tree-score tree))
      (tree-score tree)
      0))

(defun apply-scorer (tree)
  "Compute the score for this tree."
  (let ((score (or (tree-score tree) 0)))
    (setf (tree-score tree)
          (if (terminal-tree-p tree)
              score
              (+ (sum (tree-rhs tree) #'tree-score-or-0)
                 (if (numberp score)
                     score
                     (or (apply score (tree-rhs tree)) 0)))))))

(defun extend-parse (lhs sem score rhs rem needed)
  "Look for the categories needed to complete the parse."
  (if (null needed)
      (let ((parse (make-parse :tree (new-tree lhs sem score rhs) :rem rem)))
        (unless (null (apply-semantics (parse-tree parse)))
          (apply-scorer (parse-tree parse))
          (cons parse
                (mapcan
                #'(lambda (rule)
                    (extend-parse (rule-lhs rule) (rule-sem rule)
                                  (rule-score rule) (list (parse-tree parse))
                                  rem (rest (rule-rhs rule))))
                (rules-starting-with lhs)))))
      (mapcan
       #'(lambda (p)
           (if (eq (parse-lhs p) (first needed))
               (extend-parse lhs sem score
                             (append1 rhs (parse-tree p))
                             (parse-rem p) (rest needed))))
       (parse rem))))

(defun parse (words)
  "Bottom-up parse, returning all parses of any prefix of words."
  (unless (null words)
    (mapcan #'(lambda (rule)
                (extend-parse (rule-lhs rule) (rule-sem rule)
                              (rule-score rule) (list (first words))
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

(defun use (grammar)
  "Switch to a new grammar."
  (clear-memoize 'rules-starting-with)
  (clear-memoize 'lexical-rules)
  (length (setf *grammar*
                (mapcar #'(lambda (r) (apply #'rule r))
                        grammar))))

(defparameter *open-categories* '(N V A Name)
  "Categories to consider for unknown words.")

(use *grammar4*)

(parser '(The man ate the table with the striped ball))

(parser '(Dana liked Dale))

(parser '(the rab zaggled the woogly qax))

(parser '(the slithy toves gymbled))

(parser '(the slithy toves gimbled on the wabe))

(defun meanings (words)
  "Return all possible meanings of a phrase. Throw away syntactic part."
  (remove-duplicates (mapcar #'tree-sem (parser words)) :test #'equal))

(defun infix-funcall (arg1 function arg2)
  "Apply the infix function to two arguments."
  (funcall function arg1 arg2))

(use
 '((NP -> (NP CONJ NP) infix-funcall)
   (NP -> (N) list)
   (NP -> (N P N) infix-funcall)
   (N -> (DIGIT) identity)
   (P -> to integers)
   (CONJ -> and union)
   (CONJ -> without set-difference)
   (N -> 1 1)
   (N -> 2 2)
   (N -> 3 3)
   (N -> 4 4)
   (N -> 5 5)
   (N -> 6 6)
   (N -> 7 7)
   (N -> 8 8)
   (N -> 9 9)
   (N -> 0 0)))

(meanings '(1 to 5 without 3))

(meanings '(1 to 4 and 7 to 9))

(meanings '(1 to 6 without 3 and 4))

(defun union* (x y)
  (if (null (intersection x y)) (append x y)))

(defun set-diff (x y) (if (subsetp y x) (set-difference x y)))

(defun 10*N+D (N D) (+ (* 10 N) D))

(use
 '((NP -> (NP CONJ NP) infix-funcall)
   (NP -> (N) list)
   (NP -> (N P N) infix-funcall)
   (N -> (DIGIT) identity)
   (N -> (N DIGIT) 10*N+D)
   (P -> to integers)
   (CONJ -> and union*)
   (CONJ -> without set-diff)
   (DIGIT -> 1 1)
   (DIGIT -> 2 2)
   (DIGIT -> 3 3)
   (DIGIT -> 4 4)
   (DIGIT -> 5 5)
   (DIGIT -> 6 6)
   (DIGIT -> 7 7)
   (DIGIT -> 8 8)
   (DIGIT -> 9 9)
   (DIGIT -> 0 0)))

(meanings '(1 to 6 without 3 and 4))

(meanings '(1 9 8 to 2 0 1))

(meanings '(1 and 3 to 7 and 9 without 5 and 2))

(defun prefer< (x y)
  (if (>= (sem x) (sem y)) -1))

(defun prefer-disjoint (x y)
  (if (intersection (sem x) (sem y)) -1))

(defun prefer-subset (x y)
  (+ (inv-span x) (if (subsetp (sem y) (sem x)) 0 -3)))

(defun prefer-not-singleton (x)
  (+ (inv-span x) (if (< (length (sem x)) 2) -4 0)))

(defun infix-scorer (arg1 scorer arg2)
  (funcall (tree-score scorer) arg1 arg2))

(defun rev-scorer (arg scorer)
  (funcall (tree-score scorer) arg))

(defun arg2 (a1 a2 &rest a-n)
  (declare (ignore a1 a-n))
  a2)

(defun rev-funcall (arg function)
  (funcall function arg))

(defun repeat (list n)
  "Append list n times."
  (if (= n 0)
      nil
      (append (list (repeat list (- n 1))))))

(defun span-length (tree)
  "How many words are there in tree?"
  (if (terminal-tree-p tree)
      1
      (sum (tree-rhs tree) #'span-length)))

(defun inv-span (tree) (/ 1 (span-length tree)))

(defun sem (tree) (tree-sem tree))

(defun integers (start end)
  "A list of integers from start to end."
  (cond ((< start end) (cons start (integers (+ start 1) end)))
        ((> start end) (cons start (integers (- start 1) end)))
        (t (list start))))

(defun sum (numbers &optional fn)
  "Sum the numbers, or sum (mapcar fn numbers)."
  (if fn
      (loop for x in numbers sum (funcall fn x))
      (loop for x in numbers sum x)))

(defun permute (bag)
  "Return a random permutation of the given input list."
  (if (null bag)
      nil
      (let ((e (random-elt bag)))
        (cons e (permute (remove e bag :count 1 :test #'eq))))))

(defun bracketing (tree)
  "Extract the terminals, bracketed with parens."
  (cond ((atom tree) tree)
        ((length=1 (tree-rhs tree))
         (bracketing (first (tree-rhs tree))))
        (t (mapcar #'bracketing (tree-rhs tree)))))

(defun all-parses (words)
  (format t "~%Score  Semantics~25T~a" words)
  (format t "~%=====  =========~25T====================~%")
  (loop for tree in (sort (parser words) #'> :key #'tree-score)
     do (format t "~5,1f  ~9a~25T~a~%" (tree-score tree) (tree-sem tree)
                (bracketing tree)))
  (values))

(use
 '((NP -> (NP CONJ NP) infix-funcall infix-scorer)
   (NP -> (N P N) infix-funcall infix-scorer)
   (NP -> (N) list)
   (NP -> ([ NP ]) arg2)
   (NP -> (NP ADJ) rev-funcall rev-scorer)
   (NP -> (NP OP N) infix-funcall)
   (N -> (D) identity)
   (N -> (N D) 10*N+D)
   (P -> to integers prefer<)
   ([ -> [ [)
   (] -> ] ])
   (OP -> repeat repeat)
   (CONJ -> and append prefer-disjoint)
   (CONJ -> without set-difference prefer-subset)
   (ADJ -> reversed reverse inv-span)
   (ADJ -> shuffled permute prefer-not-singleton)
   (D -> 1 1)
   (D -> 2 2)
   (D -> 3 3)
   (D -> 4 4)
   (D -> 5 5)
   (D -> 6 6)
   (D -> 7 7)
   (D -> 8 8)
   (D -> 9 9)
   (D -> 0 0)))

(all-parses '(1 to 6 without 3 and 4))

(all-parses '(1 and 3 to 7 and 9 without 5 and 6))

(all-parses '(1 and 3 to 7 and 9 without 5 and 2))

(all-parses '(1 to 5 without 3 and 7 repeat 2))
