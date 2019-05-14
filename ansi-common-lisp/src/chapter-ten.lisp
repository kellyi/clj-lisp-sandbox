(defpackage chapter-ten
  (:use :cl))

(defmacro nil! (x)
  `(setf ,x nil))

(defparameter *y* 1)

(defmacro while (test &rest body)
  `(do ()
       ((not ,test))
     ,@body))

(defmacro ntimes (n &rest body)
  (let ((g (gensym))
        (h (gensym)))
    `(let ((,h ,n))
       (do ((,g 0 (+ ,g 1)))
           ((>= ,g ,h))
         ,@body))))

(macroexpand #'ntimes)
(macroexpand-1 #'ntimes)

(defmacro for (var start stop &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
         ((> ,var ,gstop))
       ,@body)))

(macroexpand-1 #'for)

(defmacro in (obj &rest choices)
  (let ((insym (gensym)))
    `(let ((,insym ,obj))
       (or ,@(mapcar #'(lambda (c) `(eql ,insym ,c))
                     choices)))))

(in 1 2 3 4)
(in 1 1 2 3)

(defmacro random-choice (&rest exprs)
  `(case (random ,(length exprs))
     ,@(let ((key -1))
         (mapcar #'(lambda (expr)
                     `(,(incf key) ,expr))
                 exprs))))

(random-choice 1 2 3 4)

(random-choice '(1) '(2) '(3) '(4))

(defmacro avg (&rest args)
  `(/ (+ ,@args) ,(length args)))

(avg 1 2 3 4 5)

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 syms)
     ,@body))

(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))

(aif t "hello" "world")
(aif nil "hello" "world")

;; Exercise 2
(defmacro cond-if (test then &optional else)
  `(cond
     (,test ,then)
     (t ,else)))

(cond-if t 'hello 'world)
(cond-if nil 'hello 'world)

;; Exercise 8
(defmacro double* (n)
  (let ((local-n (gensym)))
    `(let ((,local-n ,n))
       (setf ,local-n (* 2 ,local-n)))))
