(defpackage scheme
  (:use :cl))
(in-package :paip)

(defun set-global-var! (var val)
  (setf (get var 'global-val) val))

(defun get-global-var (var)
  (let* ((default "unbound")
         (val (get var 'global-val default)))
    (if (eq val default)
        (error "Unbound Scheme variable: ~a" var)
        val)))

(defun set-var! (var val env)
  "Set a variable to a value, in the given or global environment."
  (if (assoc var env)
      (setf (second (assoc var env)) val)
      (set-global-var! var val))
  val)

(defun get-var (var env)
  "Get the value of a variable, from the given or global environment."
  (if (assoc var env)
      (second (assoc var env))
      (get-global-var var)))

(defun extend-env (vars vals env)
  "Add some variables and values to an environment."
  (nconc (mapcar #'list vars vals) env))

(defun length=1 (x)
  "Is x a list of length 1?"
  (and (consp x) (null (cdr x))))

(defun last1 (list)
  "Return the last element -- not cons cell -- of a list."
  (first (last list)))

(defun maybe-add (op exps &optional if-nil)
  "Return t if exps is nil; exps if there is only one, and and (and exp1 exp2 ...) if several."
  (cond ((null exps) if-nil)
        ((length=1 exps) (first exps))
        (t (cons op exps))))

(defparameter *scheme-procs*
  '(+ - * / = < > <= >= cons car cdr not append list read member
    (null? null) (eq? eq) (equal? equal) (eqv? eql)
    (write prin1) (display princ) (newline terpri)))

(defun init-scheme-proc (f)
  "Define a Scheme procedure as a corresponding CL function."
  (if (listp f)
      (set-global-var! (first f) (symbol-function (second f)))
      (set-global-var! f (symbol-function f))))

(defun init-scheme-interp ()
  "Initialize the Scheme interpreter with some global variables."
  (mapc #'init-scheme-proc *scheme-procs*)
  (set-global-var! t t)
  (set-global-var! nil nil))

(defun rest2 (x)
  "The rest of a list after the first two elements."
  (rest (rest x)))

(defun scheme-macro (symbol)
  (and (symbolp symbol) (get symbol 'scheme-macro)))

(defmacro def-scheme-macro (name parmlist &body body)
  "Define a Scheme macro."
  `(setf (get ',name 'scheme-macro)
         #'(lambda ,parmlist .,body)))

(defun scheme-macro-expand (x)
  "Macro-expand this Scheme expression."
  (if (and (listp x) (scheme-macro (first x)))
      (scheme-macro-expand
       (apply (scheme-macro (first x)) (rest x)))
      x))

(def-scheme-macro let (bindings &rest body)
  `((lambda ,(mapcar #'first bindings) . ,body)
    .,(mapcar #'second bindings)))

(def-scheme-macro let* (bindings &rest body)
  (if (null bindings)
      `(begin .,body)
      `(let (,(first bindings))
         (let* ,(rest bindings) . ,body))))

(def-scheme-macro and (&rest args)
  (cond ((null args) 'T)
        ((length=1 args) (first args))
        (t `(if ,(first args)
                (and . ,(rest args))))))

(def-scheme-macro or (&rest args)
  (cond ((null args) 'nil)
        ((length=1 args) (first args))
        (t (let ((var (gensym)))
             `(let ((,var ,(first args)))
                (if ,var ,var (or . ,(rest args))))))))

(defun starts-with (list x)
  "Is x a list whose first element is x?"
  (and (consp list) (eql (first list) x)))

(def-scheme-macro cond (&rest clauses)
  (cond ((null clauses) nil)
        ((length=1 (first clauses))
         `(or ,(first clauses) (cond .,(rest clauses))))
        ((starts-with (first clauses) 'else)
         `(begin .,(rest (first clauses))))
        (t `(if ,(first (first clauses))
                (begin .,(rest (first clauses)))
                (cond .,(rest clauses))))))

(def-scheme-macro define (name &rest body)
  (if (atom name)
      `(begin (set! ,name . ,body) ',name)
      `(define ,(first name)
           (lambda ,(rest name) . ,body))))

(def-scheme-macro delay (computation)
  `(lambda () ,computation))

(def-scheme-macro letrec (bindings &rest body)
  `(let ,(mapcar #'(lambda (v) (list (first v) nil)) bindings)
     ,@(mapcar #'(lambda (v) `(set! .,v)) bindings)
     .,body))

(defun print-proc (proc &optional (stream *standard-output*) depth)
  (declare (ignore depth))
  (format stream "{~a}" (or (proc-name proc) '??)))

(defstruct (proc (:print-function print-proc))
  "Represent a Scheme procedure."
  code (env nil) (name nil) (parms nil))

(defun interp (x &optional env)
  "Evaluate the expression x in the environment env."
  (prog ()
   :INTERP
   (return
     (cond
       ((symbolp x) (get-var x env))
       ((atom x) x)
       ((scheme-macro (first x))
        (setf x (scheme-macro-expand x)) (GO :INTERP))
       ((case (first x)
          (QUOTE (second x))
          (BEGIN (pop x)
                 (loop while (rest x) do (interp (pop x) env))
                 (setf x (first x))
                 (GO :INTERP)
          (SET! (set-var! (second x) (interp (third x) env) env))
          (IF (setf x (if (interp (second x) env)
                          (third x)
                          (fourth x)))
              (GO :INTERP))
          (LAMBDA (make-proc :env env :parms (second x)
                   :code (maybe-add 'begin (rest2 x))))
          (t (let ((proc (interp (first x) env))
                   (args (mapcar #'(lambda (v) (interp v env))
                                 (rest x))))
               (if (proc-p proc)
                   (progn
                     (setf x (proc-code proc))
                     (setf env (extend-env (proc-parms proc) args
                                           (proc-env proc)))
                     (GO :INTERP))
                   (apply proc args)))))))))))

(defun scheme ()
  "A Scheme REPL using interp."
  (init-scheme-interp)
  (loop (format t "~&==> ")
     (print (interp (read) nil))))

(scheme)

