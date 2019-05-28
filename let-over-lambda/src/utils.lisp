(defpackage utils
  (:use :cl))

(eval-when (:compile-toplevel :execute)
  (handler-case
      (progn
        (sb-ext:assert-version->= 1 2 2)
        (setq *features* (remove 'old-sbcl *features*)))
    (error ()
      (pushnew 'old-sbcl *features*))))

(defun mkstr (&rest args)
  "Make a string from args."
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  "Make symbols from args."
  (values (intern (apply #'mkstr args))))

(defun group (source n)
  "Group source into sets of n length."
  (if (zerop n)
      (error "zero length")
      (labels ((rec (source acc)
                 (let ((rest (nthcdr n source)))
                   (if (consp rest)
                       (rec rest (cons
                                  (subseq source 0 n)
                                  acc))
                       (nreverse
                        (cons source acc))))))
        (if source
            (rec source nil)
            nil))))

(defun flatten (x)
  "Flatten a nested list structure."
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   #+(and sbcl (not old-sbcl))
                   ((typep x 'sb-impl::comma) (rec (sb-impl::comma-expr x) acc))
                   ((atom x) (cons x acc))
                   (t (rec
                       (car x)
                       (rec (cdr x) acc))))))
    (rec x nil)))

(defun fact (x)
  "Return the xth factorial."
  (if (zerop x)
      1
      (* x (fact (1- x)))))

(defun choose (n r)
  "Given n and r, calculate the binomial coefficient."
  (/ (fact n)
     (fact (- n r))
     (fact r)))

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

(defmacro! nlet-tail (n letargs &rest body)
  (let ((gs (loop for i in letargs
               collect (gensym))))
    `(macrolet
         ((,n ,gs
            `(progn
               (psetq
                ,@(apply #'nconc
                         (mapcar
                          #'list
                          ',(mapcar #'car letargs)
                          (list ,@gs))))
               (go ,',g!n))))
       (block ,g!b
         (let ,letargs
           (tagbody
              ,g!n (return-from
                    ,g!b (progn ,@body))))))))

(defmacro! dlambda (&rest ds)
  `(lambda (&rest ,g!args)
     (case (car ,g!args)
       ,@(mapcar
          (lambda (d)
            `(,(if (eq t (car d))
                   t
                   (list (car d)))
               (apply (lambda ,@(cdr d))
                      ,(if (eq t (car d))
                           g!args
                           `(cdr ,g!args)))))
          ds))))

(defmacro alambda (parms &body body)
  `(labels ((self ,parms ,@body))
     #'self))

(defun |#`-reader| (stream sub-char numarg)
  (declare (ignore sub-char))
  (unless numarg (setq numarg 1))
  `(lambda ,(loop for i from 1 to numarg
               collect (symb 'a i))
     ,(funcall
       (get-macro-character #\`) stream nil)))

(set-dispatch-macro-character
 #\# #\` #'|#`-reader|)

(defmacro alet (letargs &rest body)
  `(let ((this) ,@letargs)
     (setq this ,@(last body))
     ,@(butlast body)
     (lambda (&rest params)
       (apply this params))))

(defmacro dis (args &rest body)
  `(disassemble
    (compile nil
             (lambda ,(mapcar (lambda (a)
                                (if (consp a)
                                    (cadr a)
                                    a))
                              args)
               (declare
                ,@(mapcar
                   #`(type ,(car a1) ,(cadr a1))
                   (remove-if-not #'consp args)))
               ,@body))))
