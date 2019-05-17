(defpackage chapter-seven
  (:use :cl))

(defmacro nil! (var)
  (list 'setq var nil))

(defmacro nil*! (var)
  `(setq ,var nil))

(defmacro nif (expr pos zero neg)
  `(case (truncate (signum ,expr))
     (1 ,pos)
     (0 ,zero)
     (-1 ,neg)))

(defmacro nif* (expr pos zero neg)
  (list 'case
        (list 'truncate (list 'signum expr))
        (list 1 pos)
        (list 0 zero)
        (list -1 neg)))

(mapcar #'(lambda (x)
            (nif x 'p 'z 'n))
        '(0 2.5 -8))

(setq b '(1 2 3))
`(a ,b c) ;; -> (A (1 2 3) C)
`(a ,@b c) ;; -> (A 1 2 3 c)

(defmacro our-when (test &body body)
  `(if ,test
       (progn
         ,@body)))

(defmacro memq (obj lst)
  `(member ,obj ,lst :test #'eq))

(memq 1 '(1 2 3))
(memq 1 '(2 3 4))

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(let ((x 1))
  (while (< x 5)
    (format t "~A~%" x)
    (incf x)))

(macroexpand '(or x y))

(macroexpand-1 '(memq 'a '(a b c)))

(macroexpand '(while (funny) (laugh)))

(defmacro mac (expr)
  `(pprint (macroexpand-1 ',expr)))

(destructuring-bind (x (y) . z) '(a (b) c d)
  (list x y z))

(defmacro our-dolist ((var list &optional result) &body body)
  `(progn
     (mapc #'(lambda (,var) ,@body)
           ,list)
     (let ((,var nil))
       ,result)))

(defmacro when-bind ((var expr) &body body)
  `(let ((,var ,expr))
     (when ,var
       ,@body)))

(defmacro our-and (&rest args)
  (case (length args)
    (0 t)
    (1 (car args))
    (t `(if ,(car args)
            (our-and ,@(cdr args))))))

(our-and (= 1 1) (= 2 2))
(our-and (= 1 1) nil)

(defmacro new-mac (x) `(1+ ,x))
(setq fn (compile nil '(lambda (y) (new-mac y))))
(defmacro new-mac (x) `(+ ,x 100))
(funcall fn 1) ;; -> 2

(defun fn-sum (&rest args)
  (apply #'+ args))

(defmacro mac-sum (&rest args)
  `(apply #'+ (list ,@args)))

(defmacro mac-sum* (&rest args)
  `(+ ,@args))

(defun foo-fn (x y z)
  (list x (let ((x y))
            (list x z))))

(defun foo-mac (x y z)
  `(list ,x (let ((x ,y))
              (list x ,z))))
