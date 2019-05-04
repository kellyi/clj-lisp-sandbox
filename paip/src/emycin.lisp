(defpackage emycin
  (:use :cl))

(defconstant true +1.0)
(defconstant false -1.0)
(defconstant unknown 0.0)

(defun cf-or (a b)
  "Combine the certainty factors for the formula (A or B).
  This is used when two rules support the same conclusion."
  (cond ((and (> a 0) (> b 0))
         (+ a b (* -1 a b)))
        ((and (< a 0) (< b 0))
         (+ a b (* a b)))
        (t (/ (+ a b)
              (- 1 (min (abs a) (abs b)))))))

(defun cf-and (a b)
  "Combine the certainty factors for the formula (A and B)."
  (min a b))

(defconstant cf-cut-off 0.2
  "Below this certainty we cut off search.")

(defun cf-p (x)
  "Is X a valid numeric certainty factor."
  (and (numberp x) (<= false x true)))

(defun true-p (cf)
  "Is this certainty factor considered true?"
  (and (cf-p cf) (> cf cf-cut-off)))

(defun false-p (cf)
  "Is this certainty factor considered false?"
  (and (cf-p cf) (< cf (- cf-cut-off 1.0))))

(let ((db (make-hash-table :test #'equal)))
  (defun get-db (key) (gethash key db))
  (defun put-db (key val) (setf (gethash key db) val))
  (defun clear-db () (clrhash db)))

(defun get-vals (parm inst)
  "Return a list of (val cf) pairs for this (parm inst)."
  (get-db (list parm inst)))

(defun get-cf (parm inst val)
  "Look up the certainty factor or return unknown."
  (or (second (assoc val (get-vals parm inst)))
      unknown))

(defun update-cf (parm inst val cf)
  "Change the certainty factor for (parm inst is val),
  by combining the given cf with the old."
  (let ((new-cf (cf-or cf (Get-cf parm inst val))))
    (put-db (list parm inst)
            (cons (list val new-cf)
                  (remove val (get-db (list parm inst))
                          :key #'first)))))

(defconstant help-string
  "~&Type one of the following:
?     - to see possible answers for this parameter
rule  - to show the current rule
why   - to see why this question is asked
help  - to see this list
xxx   - (for some specific xxx) if there is a definite answer
(xxx .5 yyy .4) - If there are several answers with different certainty factors.")

(defstruct (parm (:constructor
                  new-parm (name &optional context type-restriction
                                 prompt ask-first reader)))
  name (context nil) (prompt "~&What is the ~*~a of ~2:*~a?")
  (ask-first nil) (type-restriction t) (reader 'read))

(defmacro defparm (parm &rest args)
  "Define a parameter."
  `(setf (get ',parm 'parm) (apply #'new-parm ',parm ',args)))

(defun get-parm (parm-name)
  "Look up the parameter structure with this name, or make one if there is none."
  (or (get parm-name 'parm)
      (setf (get parm-name 'parm) (new-parm parm-name))))

(defun parm-type (parm-name)
  "What type is expected for a value of this parameter?"
  (parm-type-restriction (get-parm parm-name)))

(deftype yes/no () '(member yes no))

(defun inst-name (inst)
  "The name of this instance."
  (or (first (first (get-vals 'name inst)))
      inst))

(defun rest2 (x)
  "The rest of a list after the first two elements."
  (rest (rest x)))

(defun parse-reply (reply)
  "Convert the reply into a list of (value cf) pairs."
  (cond ((null reply) nil)
        ((atom reply) `((,reply ,true)))
        (t (cons (list (first reply) (second reply))
                 (parse-reply (rest2 reply))))))

(defun check-reply (reply parm inst)
  "If reply is valid for this parm, update the DB.
  Reply should be a val or (val1 cf1 val2 cf2 ...).
  Each val must be of the right type for this parm."
  (let ((answers (parse-reply reply)))
    (when (every #'(lambda (pair)
                     (and (typep (first pair) (parm-type parm))
                          (cf-p (second pair))))
                 answers)
      ;; Add replies to the database
      (dolist (pair answers)
        (update-cf parm inst (first pair) (second pair)))
      answers)))

(defun prompt-and-read-vals (parm inst)
  "Print the prompt for this parameter (or make one up) and read the reply."
  (fresh-line)
  (format t (parm-prompt (get-parm parm)) (inst-name inst) parm)
  (princ " ")
  (finish-output)
  (funcall (parm-reader (get-parm parm))))

(defstruct context
  "A context is a sub-domain, a type."
  name (number 0) initial-data goals)

(defmacro defcontext (name &optional initial-data goals)
  "Define a context."
  `(make-context :name ',name :initial-data ',initial-data
                 :goals ',goals))

(defun new-instance (context)
  "Create a new instance of this context."
  (let ((instance (format nil "~a-~d"
                          (context-name context)
                          (incf (context-number context)))))
    (format t "~&------ ~a ------~&" instance)
    (put-db (context-name context) instance)
    (put-db 'current-instance instance)))

(defun print-condition (condition stream number)
  "Print a single condition in pseudo-English."
  (format stream "~&    ~d)~{ ~a~}" number
          (let ((parm (first condition))
                (inst (second condition))
                (op (third condition))
                (val (fourth condition)))
            (case val
              (YES `(the ,inst ,op ,parm))
              (NO `(the ,inst ,op not ,parm))
              (T `(the ,parm of the ,inst ,op ,val))))))

(defun cf->english (cf)
  "Convert a certainty factor to an English phrase."
  (cond ((= cf 1.0) "there is certain evidence.")
        ((> cf .8) "there is strongly suggestive evidence.")
        ((> cf .5) "there is suggestive evidence.")
        ((> cf .0) "there is weakly suggestive evidence.")
        ((= cf 0.0) "there is no evidence either way.")
        ((< cf 0.0) (concatenate 'string (cf->english (- cf))
                                 " AGAINST the conclusion"))))

(defun print-conditions (conditions &optional (stream t) (num 1))
  "Print a list of conditions."
  (dolist (condition conditions)
    (print-condition condition stream num)))

(defun print-rule (rule &optional (stream t) depth)
  (declare (ignore depth))
  (Format stream "~&Rule ~a:~&  If" (rule-number rule))
  (print-conditions (rule-premises rule) stream)
  (format stream "~&  Then ~a (~a) that"
          (cf->english (rule-cf rule)) (rule-cf rule))
  (print-conditions (rule-conclusions rule) stream))

(defstruct (rule (:print-function print-rule))
  number premises conclusions cf)

(let ((rules (make-hash-table)))
  (defun put-rule (rule)
    "Put the rule in a table, indexed under each parm in the conclusion."
    (dolist (concl (rule-conclusions rule))
      (push rule (gethash (first concl) rules)))
    rule)
  (defun get-rules (parm)
    "A list of rules that help determine this parameter."
    (gethash parm rules))
  (defun clear-rules () (clrhash rules)))

(defun parse-condition (condition)
  "A condition is of the form (parm inst op val)."
  (values (first condition)
          (get-db (second condition))
          (third condition)
          (fourth condition)))

(defun is (a b) (equal a b))

(defun reject-premise (premise)
  "A premise is rejected if it is already known false."
  (false-p (eval-condition premise nil)))

(defun conclude (conclusion cf)
  "Add a conclusion with a specified cf to db."
  (multiple-value-bind (parm inst op val)
      (parse-condition conclusion)
    (declare (ignore op))
    (update-cf parm inst val cf)))

(defun eval-condition (condition &optional (find-out-p t))
  "See if this condition is true, optionally using find-out to determine unknown
  parameters."
  (multiple-value-bind (parm inst op val)
      (parse-condition condition)
    (when find-out-p
      (find-out parm inst))
    (loop for pair in (get-vals parm inst)
       when (funcall op (first pair) val)
         sum (second pair))))

(defun satisfy-premises (premises cf-so-far)
  "A list of premises is satisfied if they are all true.
  Returns combined certainty factor."
  (cond ((null premises) cf-so-far)
        ((not (true-p cf-so-far)) false)
        (t (satisfy-premises
            (rest premises)
            (cf-and cf-so-far
                    (eval-condition (first premises)))))))

(defun use-rule (rule)
  "Apply a rule to the current situation."
  ;; Keep track of the rule for the explanation system:
  (put-db 'current-rule rule)
  ;; If any premise is false, give up
  ;; If every premise can be proved true, then draw weighted conclusions.
  (unless (some #'reject-premise (rule-premises rule))
    (let ((cf (satisfy-premises (rule-premises rule) true)))
      (when (true-p cf)
        (dolist (conclusion (rule-conclusions rule))
          (conclude conclusion (* cf (rule-cf rule))))
        cf))))

(defun use-rules (parm)
  "Try every rule associated with this parameter,
  Return true if one of the rules returns true."
  (some #'true-p (mapcar #'use-rule (get-rules parm))))

(defun find-out (parm &optional (inst (get-db 'current-instance)))
  "Find the value/s of this parameter for this instance, unless the values are
  already known. Some parameters we ask first; others we use rules first."
  (or (get-db `(known ,parm ,inst))
      (put-db `(known ,parm ,inst)
              (if (parm-ask-first (get-parm parm))
                  (or (ask-vals parm inst) (use-rules parm))
                  (or (use-rules parm) (ask-vals parm inst))))))


(defun ask-vals (parm inst)
  "Ask the user for the value/s of inst's parm parameter,
  unless this has already been asked. Keep asking until the
  user types UNKNOWN (return nil) or a valid reply (return t)."
  (unless (get-db `(asked ,parm ,inst))
    (put-db `(asked ,parm ,inst) t)
    (loop
       (let ((ans (prompt-and-read-vals parm inst)))
         (case ans
           (help (format t help-string))
           (why (print-why (get-db 'current-rule) parm))
           (rule (princ (get-db 'current-rule)))
           ((unk unknown) (RETURN nil))
           (? (format t "~&A ~a must be of type ~a"
                      parm (parm-type parm)) nil)
           (t (if (check-reply ans parm inst)
                  (RETURN t)
                  (format t "~&Illegal reply. ~
                            Type ? to see legal ones."))))))))

(defun check-conditions (rule-num conditions kind)
  "Warn if any conditions are invalid."
  (when (null conditions)
    (warn "Rule ~a: Missing ~a" rule-num kind))
  (dolist (condition conditions)
    (when (not (consp condition))
      (warn "Rule ~a: Illegal ~a: ~a" rule-num kind condition))
    (multiple-value-bind (parm inst op val)
        (parse-condition condition)
      (declare (ignore inst))
      (when (and (eq kind 'conclusion) (not (eq op 'is)))
        (warn "Rule ~a: Illegal operator (~a) in conclusion: ~a"
              rule-num op condition))
      (when (not (typep val (parm-type parm)))
        (warn "Rule ~a: illegal value (~a) in ~a: ~a"
              rule-num val kind condition)))))

(defmacro defrule (number &body body)
  "Define a rule with conditions, a certainty factor, and conclustions."
  (assert (eq (first body) 'if))
  (let* ((then-part (member 'then body))
         (premises (ldiff (rest body) then-part))
         (conclusions (rest2 then-part))
         (cf (second then-part)))
    (check-conditions number premises 'premise)
    (check-conditions number conclusions 'conclusion)
    (when (not (cf-p cf))
      (warn "Rule ~a: Illegal certainty factor: ~a" number cf))
    `(put-rule
      (make-rule :number ',number :cf ,cf :premises ',premises
                 :conclusions ',conclusions))))

(defun report-findings (context inst)
  "Print findings on each goal for this instance."
  (when (context-goals context)
    (format t "~&Findings for ~a:" (inst-name inst))
    (dolist (goal (context-goals context))
      (let ((values (get-vals goal inst)))
        (if values
            (format t "~& ~a:~{~{ ~a (~,3f) ~}~}" goal
                    (sort (copy-list values) #'> :key #'second))
            (format t "~& ~a: unknown" goal))))))


(defun partition-if (pred list)
  "Return 2 values: elements of list that satisfy pred, and elements that don't."
  (let ((yes-list nil)
        (no-list nil))
    (dolist (item list)
      (if (funcall pred item)
          (push item yes-list)
          (push item no-list)))
    (values (nreverse yes-list) (nreverse no-list))))

(defun print-why (rule parm)
  "Tell why this rule is being used. Print what is known, what we are trying to
  find out, and what we can conclude."
  (format t "~&[Why is the value of ~a being asked for?]" parm)
  (if (member rule '(initial goal))
      (format t "~&~a is one of the ~a parameters."
              parm rule)
      (multiple-value-bind (knowns unknowns)
          (partition-if #'(lambda (premise)
                            (true-p (eval-condition premise nil)))
                        (rule-premises rule))
        (when knowns
          (format t "~&It is known that:")
          (print-conditions knowns)
          (format t "~&Therefore ,"))
        (let ((new-rule (copy-rule rule)))
          (setf (rule-premises new-rule) unknowns)
          (print new-rule)))))

(defun get-context-data (contexts)
  "For each context, create an instance and try to find out required data.
  Then go on to other contexts, depth first, and finally ask if there are other
  instances of this context."
  (unless (null contexts)
    (let* ((context (first contexts))
           (inst (new-instance context)))
      (put-db 'current-rule 'initial)
      (mapc #'find-out (context-initial-data context))
      (put-db 'current-rule 'goal)
      (mapc #'find-out (context-goals context))
      (report-findings context inst)
      (get-context-data (rest contexts))
      (when (y-or-n-p "Is there another ~a?"
                      (context-name context))
        (get-context-data contexts)))))

(defun emcyin (contexts)
  "An Expert-System shell. Accumulate data for instances of each context, and
  solve for goals. Then report findings."
  (clear-db)
  (get-context-data contexts))
