(defpackage chapter-six
  (:use :cl)
  (:use :utils)
  (:use :chapter-five))

(defmacro alambda (parms &body body)
  `(labels ((self ,parms ,@body))
     #'self))

(alambda (n)
  (if (> n 0)
      (cons
       n
       (self (1- n)))))

(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))

(defun |#`-reader| (stream sub-char numarg)
  (declare (ignore sub-char))
  (unless numarg (setq numarg 1))
  `(lambda ,(loop for i from 1 to numarg
               collect (symb 'a i))
     ,(funcall
       (get-macro-character #\`) stream nil)))

(set-dispatch-macro-character
 #\# #\` #'|#`-reader|)

'#`((,a1))

(mapcar (lambda (a)
          (list a ''empty))
        '(var-a var-b var-c))

(mapcar (lambda (a)
          `(,a 'empty))
        '(var-a var-b var-c))

(mapcar #`(,a1 'empty)
        '(var-a var-b var-c))

(let ((vars '(var-a var-b var-c)))
  (mapcar #2`(,a1 ,a2)
          vars
          (loop for v in vars
             collect (gensym
                      (symbol-name v)))))

(#3`(((,@a2)) ,a3 (,a1 ,a1))
   (gensym)
   '(a b c)
   'hello)

(defmacro alet* (letargs &rest body)
  `(let ((this) ,@letargs)
     (setq this ,@(last body))
     ,@(butlast body)
     this))

(alet* ((sum) (mul) (expt))
       (funcall this :reset)
       (dlambda
        (:reset ()
                (psetq sum 0
                       mul 1
                       expt 2))
        (t (n)
           (psetq sum (+ sum n)
                  mul (* mul n)
                  expt (expt expt n))
           (list sum mul expt))))

(loop for i from 1 to 5 collect (funcall * 2))

(funcall ** :reset)

(loop for i from 1 to 5 collect (funcall *** 0.5))

(defmacro alet (letargs &rest body)
  `(let ((this) ,@letargs)
     (setq this ,@(last body))
     ,@(butlast body)
     (lambda (&rest params)
       (apply this params))))

(alet ((acc 0))
      (alambda (n)
        (if (eq n 'invert)
            (setq this
                  (lambda (n)
                    (if (eq n 'invert)
                        (setq this #'self)
                        (decf acc n))))
            (incf acc n))))

(setf (symbol-function 'alet-test) *)

(alet-test 10)

(alet-test 'invert)

(alet-test 10)

(alet-test 'invert)

(alet-test 3)

(macroexpand
 '(alambda (n)
   (if (eq n 'invert)
       (setq this
             (lambda (n)
               (if (eq n 'invert)
                   (setq this #'self)
                   (decf acc n))))
       (incf acc n))))

(alet ((acc 0))
      (labels ((going-up (n)
                 (if (eq n 'invert)
                     (setq this #'going-down)
                     (incf acc n)))
               (going-down (n)
                 (if (eq n 'invert)
                     (setq this #'going-up)
                     (incf acc (- n)))))
        #'going-up))

(defmacro alet-fsm (&rest states)
  `(macrolet ((state (s)
                `(setq this #',s)))
     (labels (,@states) #',(caar states))))

(alet ((acc 0))
      (alet-fsm
       (going-up (n)
                 (if (eq n 'invert)
                     (state going-down)
                     (incf acc n)))
       (going-down (n)
                   (if (eq n 'invert)
                       (state going-up)
                       (decf acc n)))))

(defmacro! ichain-before (&rest body)
  `(let ((,g!indir-env this))
     (setq this
           (lambda (&rest ,g!temp-args)
             ,@body
             (apply ,g!indir-env
                    ,g!temp-args)))))

(alet ((acc 0))
      (ichain-before
       (format t "Changing from ~a~%" acc))
      (lambda (n)
        (incf acc n)))

(funcall * 2)
(funcall ** 2)

(alet ((acc 0))
      (ichain-before
       (format t "A~%"))
      (ichain-before
       (format t "B~%"))
      (ichain-before
       (format t "C~%"))
      (lambda (n)
        (incf acc n)))

(funcall * 2)

(alet ((acc 0))
      (lambda (n)
        (ichain-before
         (format t "Hello world~%"))
        (incf acc n)))

(loop for i from 1 to 4
   do
     (format t "~:r invocation:~%" i)
     (funcall * i))

(defmacro! ichain-after (&rest body)
  `(let ((,g!indir-env this))
     (setq this
           (lambda (&rest ,g!temp-args)
             (prog1
                 (apply ,g!indir-env
                        ,g!temp-args)
               ,@body)))))

(alet ((acc 0))
      (ichain-before
       (format t "Changing from ~a~%" acc))
      (ichain-after
       (format t "Changed to ~a~%" acc))
      (lambda (n)
        (incf acc n)))

(funcall * 7)

(defmacro! ichain-intercept* (&rest body)
  `(let ((,g!indir-env this))
     (setq this
           (lambda (&rest ,g!temp-args)
             (block intercept
               (prog1
                   (apply ,g!indir-env
                          ,g!temp-args)
                 ,@body))))))

(alet ((acc 0))
      (ichain-intercept*
       (when (< acc 0)
         (format t "Acc went negative~%")
         (setq acc 0)
         (return-from intercept acc)))
      (lambda (n)
        (incf acc n)))

(funcall * -8)
(funcall ** 3)

(defmacro! ichain-intercept (&rest body)
  `(let ((,g!indir-env this))
     (setq this
           (lambda (&rest ,g!temp-args)
             (block ,g!intercept
               (macrolet ((intercept (v)
                            `(return-from
                              ,',g!intercept
                               ,v)))
                 (prog1
                     (apply ,g!indir-env
                            ,g!temp-args)
                   ,@body)))))))

(alet ((acc 0))
      (ichain-intercept
       (when (< acc 0)
         (format t "Acc went negative~%")
         (setq acc 0)
         (intercept acc)))
      (lambda (n)
        (incf acc n)))

(funcall * -8)

(funcall ** 3)

(defmacro alet-hotpatch* (letargs &rest body)
  `(let ((this) ,@letargs)
     (setq this ,@(last body))
     ,@(butlast body)
     (lambda (&rest args)
       (if (eq (car args) ':hotpatch)
           (setq this (cadr args))
           (apply this args)))))

(setf (symbol-function 'hotpatch-test)
      (alet-hotpatch* ((acc 0))
                      (lambda (n)
                        (incf acc n))))

(hotpatch-test 3)

(hotpatch-test 4)

(hotpatch-test
 :hotpatch
 (let ((acc 0))
   (lambda (n)
     (incf acc (* 2 n)))))

(hotpatch-test 2)

(hotpatch-test 5)

(defmacro alet-hotpatch (letargs &rest body)
  `(let ((this) ,@letargs)
     (setq this ,@(last body))
     ,@(butlast body)
     (dlambda
      (:hotpatch (closure)
                 (setq this closure))
      (t (&rest args)
         (apply this args)))))

(defmacro! let-hotpatch (letargs &rest body)
  `(let ((,g!this) ,@letargs)
     (setq ,g!this ,@(last body))
     ,@(butlast body)
     (dlambda
      (:hotpatch (closure)
                 (setq ,g!this closure))
      (t (&rest args)
         (apply ,g!this args)))))

(defun let-binding-transform (bs)
  (if bs
      (cons
       (cond
         ((symbolp (car bs)) (list (car bs)))
         ((consp (car bs)) (car bs))
         (t (error "Bad let bindings")))
       (let-binding-transform (cdr bs)))))

(defmacro sublet (bindings% &rest body)
  (let ((bindings (let-binding-transform
                   bindings%)))
    (setq bindings
          (mapcar
           (lambda (x)
             (cons (gensym (symbol-name (car x))) x))
           bindings))
    `(let (,@(mapcar #'list
                     (mapcar #'car bindings)
                     (mapcar #'caddr bindings)))
       ,@(tree-leaves
          body
          #1=(member x bindings :key #'cadr)
          (caar #1#)))))

(macroexpand
 '(sublet ((a 0))
   (list a)))

(macroexpand
 '(sublet ((a 0))
   (list a)))

(defmacro sublet* (bindings &rest body)
  `(sublet ,bindings
           ,@(mapcar #'macroexpand-1 body)))

(macroexpand
 '(sublet* ((a 0))
   (list a)))

(defmacro injector-for-a ()
  'a)

(macroexpand-1
 '(sublet* ((a 0))
   (injector-for-a)))

(macroexpand-1 *)

(macroexpand-1
 '(sublet* ((a 0))
   (list (injector-for-a))))

(sb-cltl2:macroexpand-all *)

(defun pandoriclet-get (letargs)
  `(case sym
     ,@(mapcar #`((,(car a1)) ,(car a1))
               letargs)
     (t (error
         "Unknown pandoric get: ~a"
         sym))))

(defun pandoriclet-set (letargs)
  `(case sym
     ,@(mapcar #`((,(car a1))
                  (setq ,(car a1) val))
               letargs)
     (t (error
         "Unknown pandoric set: ~a ~a"
         sym val))))

(defmacro pandoriclet (letargs &rest body)
  (let ((letargs (cons
                  '(this)
                  (let-binding-transform
                   letargs))))
    `(let (,@letargs)
       (setq this ,@(last body))
       ,@(butlast body)
       (dlambda
        (:pandoric-get (sym)
                       ,(pandoriclet-get letargs))
        (:pandoric-set (sym val)
                       ,(pandoriclet-set letargs))
        (t (&rest args)
           (apply this args))))))

(setf (symbol-function 'pantest)
      (pandoriclet ((acc 0))
                   (lambda (n) (incf acc n))))

(pantest 3)

(pantest 5)

(pantest :pandoric-get 'acc)

(pantest :pandoric-set 'acc 100)

(pantest 3)

(pantest :pandoric-get 'this)

(declaim (inline get-pandoric))

(defun get-pandoric (box sym)
  (funcall box :pandoric-get sym))

(defsetf get-pandoric (box sym) (val)
  `(progn
     (funcall ,box :pandoric-set ,sym ,val)
     ,val))

(get-pandoric #'pantest 'acc)

(setf (get-pandoric #'pantest 'acc) -10)

(pantest 3)

(defmacro! with-pandoric (syms o!box &rest body)
  `(symbol-macrolet
       (,@(mapcar #`(,a1 (get-pandoric ,g!box ',a1))
                  syms))
     ,@body))

(with-pandoric (acc) #'pantest
               (format t "Value of acc: ~a~%" acc))

(with-pandoric (acc) #'pantest
               (setq acc 5))

(pantest 1)

(defun pandoric-hotpatch (box new)
  (with-pandoric (this) box
                 (setq this new)))

(pantest 0)

(pandoric-hotpatch #'pantest
                   (let ((acc 100))
                     (lambda (n) (decf acc n))))

(pantest 3)

(with-pandoric (acc) #'pantest
               acc)

(defmacro pandoric-recode (vars box new)
  `(with-pandoric (this ,@vars) ,box
                  (setq this ,new)))

(pandoric-recode (acc) #'pantest
                 (lambda (n)
                   (decf acc (/ n 2))))

(pantest 2)

(with-pandoric (acc) #'pantest
               acc)

(defmacro plambda (largs pargs &rest body)
  (let ((pargs (mapcar #'list pargs)))
    `(let (this self)
       (setq
        this (lambda ,largs ,@body)
        self (dlambda
              (:pandoric-get (sym)
                             ,(pandoriclet-get pargs))
              (:pandoric-set (sym val)
                             ,(pandoriclet-set pargs))
              (t (&rest args)
                 (apply this args)))))))

(setf (symbol-function 'pantest)
      (let ((a 0))
        (let ((b 1))
          (plambda (n) (a b)
                   (incf a n)
                   (setq b (* b n))))))

(defun pantest-peek ()
  (with-pandoric (a b) #'pantest
                 (format t "a=~a, b=~a~%" a b)))

(pantest-peek)

(defun make-stats-counter (&key
                             (count 0)
                             (sum 0)
                             (sum-of-squares 0))
  (plambda (n) (sum count sum-of-squares)
           (incf sum-of-squares (expt n 2))
           (incf sum n)
           (incf count)))

(defmacro defpan (name args &rest body)
  `(defun ,name (self)
     ,(if args
          `(with-pandoric ,args self
                          ,@body)
          `(progn ,@body))))

(defpan stats-counter-mean (sum count)
  (/ sum count))

(defpan stats-counter-variance (sum-of-squares sum count)
  (if (< count 2)
      0
      (/ (- sum-of-squares
            (* sum
               (stats-counter-mean self)))
         (1- count))))

(defpan stats-counter-stddev ()
  (sqrt (stats-counter-variance self)))

(defvar pandoric-eval-tunnel)

(defmacro pandoric-eval (vars expr)
  `(let ((pandoric-eval-tunnel
          (plambda () ,vars t)))
     (eval `(with-pandoric
                ,',vars pandoric-eval-tunnel
                ,,expr))))

(let ((x 1))
  (pandoric-eval (x)
                 '(1+ x)))

(let ((x 1))
  (pandoric-eval (x)
                 '(incf x)))
