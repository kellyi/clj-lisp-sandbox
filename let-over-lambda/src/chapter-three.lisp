(defpackage chapter-three
  (:use :cl)
  (:use :utils))

(defun sleep-units% (value unit)
  (sleep
   (* value
      (case unit
        ((s) 1)
        ((m) 60)
        ((h) 3600)
        ((d) 86400)
        ((ms) 1/1000)
        ((us) 1/1000000)))))

(defmacro sleep-units (value unit)
  `(sleep
    (* ,value
       ,(case unit
          ((s) 1)
          ((m) 60)
          ((h) 3600)
          ((d) 86400)
          ((ms) 1/1000)
          ((us) 1/1000000)))))

(defmacro unit-of-time (value unit)
  `(* ,value
      ,(case unit
         ((s) 1)
         ((m) 60)
         ((h) 3600)
         ((d) 86400)
         ((ms) 1/1000)
         ((us) 1/1000000))))

(defmacro nlet (n letargs &rest body)
  `(labels ((,n ,(mapcar #'car letargs)
              ,@body))
     (,n ,@(mapcar #'cadr letargs))))

(defun nlet-fact (n)
  (nlet fact ((n n))
        (if (zerop n)
            1
            (* n (fact (1- n))))))

(macroexpand
 '(nlet fact ((n n))
   (if (zerop n)
       1
       (* n (1- fact)))))

(let ((a)) a)

(let ((x 1))
  (1+ x))

(defmacro x-injector ()
  'x)

(defmacro nif-buggy (expr pos zero neg)
  `(let ((obscure-name ,expr))
     (cond ((plusp obscure-name) ,pos)
           ((zerop obscure-name) ,zero)
           (t ,neg))))

(defmacro nif (expr pos zero neg)
  (let ((g (gensym)))
    `(let ((,g ,expr))
       (cond
         ((plusp ,g) ,pos)
         ((zerop ,g) ,zero)
         (t ,neg)))))

(macroexpand '(nif x 'pos 'zero 'neg))

(let ((*print-circle* t))
  (print
   (macroexpand '(nif x 'pos 'zero 'neg)))
  t)

(defun g!-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string= (symbol-name s)
                "G!"
                :start1 0
                :end1 2)))

(defmacro defmacro/g! (name args &rest body)
  (let ((syms (remove-duplicates
               (remove-if-not #'g!-symbol-p
                              (flatten body)))))
    `(defmacro ,name ,args
       (let ,(mapcar
              (lambda (s)
                `(,s (gensym ,(subseq
                               (symbol-name s)
                               2))))
              syms)
         ,@body))))

(defmacro/g! nif (expr pos zero neg)
  `(let ((,g!result ,expr))
     (cond
       ((plusp ,g!result) ,pos)
       ((zerop ,g!result) ,zero)
       (t ,neg))))

(macroexpand-1
 '(defmacro/g! nif (expr pos zero neg)
   `(let ((,g!result ,expr))
      (cond ((plusp ,g!result) ,pos)
            ((zerop ,g!result) ,zero)
            (t ,neg)))))

(defmacro/g! junk-outer ()
  `(defmacro/g! junk-inner ()
     `(let ((,g!abc))
        ,g!abc)))

(defmacro square (x)
  `(* ,x ,x)) ;; x form is evaluated 2x

(defun o!-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string= (symbol-name s)
                "O!"
                :start1 0
                :end1 2)))

(defun o!-symbol-to-g!-symbol (s)
  (symb "G!"
        (subseq (symbol-name s) 2)))

(defmacro defmacro! (name args &rest body)
  (let* ((os (remove-if-not #'o!-symbol-p args))
         (gs (mapcar #'o!-symbol-to-g!-symbol os)))
    `(defmacro/g! ,name ,args
       `(let ,(mapcar #'list (list ,@gs) (list ,@os))
          ,(progn ,@body)))))

(defmacro! square-once-only (o!x)
  `(* ,g!x ,g!x))

(macroexpand '(square (incf x)))
(macroexpand '(square-once-only (incf x)))

(defmacro! square* (o!x)
  `(progn
     (format t "[~a gave ~a]~%"
             ',o!x ,g!x)
     (* ,g!x ,g!x)))

(defvar *x* 4)
(square* (incf *x*))

(defmacro! nif (o!expr pos zero neg)
  `(cond ((plusp ,g!expr) ,pos)
         ((zerop ,g!expr) ,zero)
         (t ,neg)))

(let ((temp-special 'whatever))
  (lambda () temp-special))
