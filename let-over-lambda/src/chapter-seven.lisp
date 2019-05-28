(defpackage chapter-seven
  (:use :cl)
  (:use :utils))

(set-dispatch-macro-character #\# #\f
                              (lambda (stream sub-char numarg)
                                (declare (ignore stream sub-char))
                                (setq numarg (or numarg 3))
                                (unless (<= numarg 3)
                                  (error "Bad value for #f: ~a" numarg))
                                `(declare (optimize (speed ,numarg)
                                                    (safety ,(- 3 numarg))))))

'#f

'#0f

'(#1f #2f)

(defmacro fast-progn (&rest body)
  `(locally #f ,@body))

(defmacro safe-progn (&rest body)
  `(locally #0f ,@body))

(macroexpand
 '(fast-progn
   (+ 1 2)))

(macroexpand
 '(fast-progn
   (declare (type fixnum a)
    (the fixnum (+ a 1)))))

;; (defmacro error-checker ()
;;   `(safe-progn
;;     (declare (type integer var))
;;     do-whatever-other-error-checking))

;; (defun wrapped-operation ()
;;   (safe-progn
;;    do-whatever-other-error-checking
;;    (fast-progn
;;     but-this-needs-to-go-fast)))

(defun fast-keywords-strip (args)
  (if args
      (cond
        ((eq (car args) '&key)
         (fast-keywords-strip (cdr args)))
        ((consp (car args))
         (cons (caar args)
               #1=(fast-keywords-strip
                   (cdr args))))
        (t
         (cons (car args) #1#)))))

(defmacro! defun-with-fast-keywords (name args &rest body)
  `(progn
     (defun ,name ,args ,@body)
     (defun ,g!fast-fun
         ,(fast-keywords-strip args)
       ,@body)
     (compile ',g!fast-fun)
     (define-compiler-macro ,name (&rest ,g!rest)
       (destructuring-bind ,args ,g!rest
         (list ',g!fast-fun
               ,@(fast-keywords-strip args))))))

(defun slow-keywords-test (a b &key (c 0) (d 0))
  (+ a b c d))

(compile 'slow-keywords-test)

(defun-with-fast-keywords
    fast-keywords-test (a b &key (c 0) (d 0))
  (+ a b c d))

(defun keywords-benchmark (n)
  (format t "Slow keys:~%")
  (time
   (loop for i from 1 to n do
        (slow-keywords-test 1 2 :d 3 :c n)))
  (format t "Fast keys:~%")
  (time
   (loop for i from 1 to n do
        (fast-keywords-test 1 2 :d 3 :c n))))

(compile 'keywords-benchmark)

(keywords-benchmark 1000000000)

;; Slow keys:
;; Evaluation took:
;; 14.611 seconds of real time
;; 14.591124 seconds of total run time (14.574025 user, 0.017099 system)
;; 99.86% CPU
;; 33,664,014,520 processor cycles
;; 7,840 bytes consed

;; Fast keys:
;; Evaluation took:
;; 8.125 seconds of real time
;; 8.120587 seconds of total run time (8.113040 user, 0.007547 system)
;; 99.95% CPU
;; 18,718,352,054 processor cycles
;; 0 bytes consed

(macroexpand
 '(formatter "Hello ~A~%"))

(defun fformat (&rest all)
  (apply #'format all))

(compile 'fformat)

(define-compiler-macro fformat (&whole form stream fmt &rest args)
  (if (constantp fmt)
      (if stream
          `(funcall (formatter ,fmt)
                    ,stream ,@args)
          (let ((g!stream (gensym "stream")))
            `(with-output-to-string (,g!stream)
               (funcall (formatter ,fmt)
                        ,g!stream ,@args))))
      form))

(defun fformat-benchmark (n)
  (format t "Format :~%")
  (time
   (loop for i from 1 to n do
        (format nil "Hello ~a ~a~%" 'world n)))
  (format t "Fformat :~%")
  (time
   (loop for i from 1 to n do
        (fformat nil "Hello ~a ~a%" 'world n))))

(compile 'fformat-benchmark)

(fformat-benchmark 10000000)

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

(macroexpand
 '(dis (a b) (+ a b)))

(macroexpand
 '(dis ((fixnum a) (integer b))
   (+ a b)))

(dis (a b)
     (+ a b))

(dis (a b c)
     (+ a b c))

(dis ((fixnum a) b)
     (+ a b))

(dis ((fuxnum a) (fixnum b))
     (+ a b))

(dis ((fixnum a) (fixnum b))
     #f
     (+ a b))

(dis ((fixnum a) (fixnum b))
     #f
     (the fixnum (+ a b)))

(defmacro! pointer-& (obj)
  `(lambda (&optional (,g!set ',g!temp))
     (if (eq ,g!set ',g!temp)
         ,obj
         (setf ,obj ,g!set))))

(defun pointer-* (addr)
  (funcall addr))

(defsetf pointer-* (addr) (val)
  `(funcall ,addr ,val))

(defsetf pointer-& (addr) (val)
  `(setf (pointer-* ,addr) ,val))

(let ((x 0))
  (pointer-& (pointer-& x)))

(defvar temp-pointer *)

(pointer-* temp-pointer)

(pointer-* *)

(setf (pointer-* (pointer-* temp-pointer)) 5)

(pointer-* (pointer-* temp-pointer))

(pointer-& temp-pointer)

(pointer-* (pointer-* (pointer-* *)))

(setf (pointer-* (pointer-* (pointer-* **))) 9)

(pointer-* (pointer-* temp-pointer))

(dis (arr ind)
     (aref arr ind))

(dis (((simple-array fixum) arr)
      (fixnum ind))
     (aref arr ind))

(defmacro! with-fast-stack ((sym &key
                                 (type 'fixnum)
                                 (size 1000)
                                 (safe-zone 100))
                            &rest body)
  `(let ((,g!index ,safe-zone)
         (,g!mem (make-array ,(+ size (* 2 safe-zone))
                             :element-type ',type)))
     (declare (type (simple-array ,type) ,g!mem)
              (type fixnum ,g!index))
     (macrolet
         ((,(symb 'fast-push- sym) (val)
            `(locally #f
               (setf (aref ,',g!mem ,',g!index) ,val)
               (incf ,',g!index)))
          (,(symb 'fast-pop- sym) ()
            `(locally #f
               (decf ,',g!index)
               (aref ,',g!mem ,',g!index)))
          (,(symb 'check-stack- sym) ()
            `(progn
               (if (<= ,',g!index ,,safe-zone)
                   (error "Stack underflow: ~a"
                          ',',sym))
               (if (<= ,,(- size safe-zone)
                       ,',g!index)
                   (error "Stack overflow: ~a"
                          ',',sym)))))
       ,@body)))

(dis ((fixnum a))
     (with-fast-stack (input :size 2000)
       (loop for i from 1 to 1000000 do
            (fast-push-input a))))

(compile nil
         '(lambda (n)
           (declare (type fixnum a))
           (with-fast-stack (input :size 2000)
             (loop for i from 1 to 1000000 do
                  (fast-push-input a)))))

;; (funcall * 31337) -> will break slime

(declaim (inline make-tlist tlist-left
                 tlist-right tlist-empty-p))

(defun make-tlist () (cons nil nil))
(defun tlist-left (tl) (caar tl))
(defun tlist-right (tl) (cadr tl))
(defun tlist-empty-p (tl) (null (car tl)))

(declaim (inline tlist-add-left
                 tlist-add-right))

(defun tlist-add-left (tl it)
  (let ((x (cons it (car tl))))
    (if (tlist-empty-p tl)
        (setf (cdr tl) x))
    (setf (car tl) x)))

(defun tlist-add-right (tl it)
  (let ((x (cons it nil)))
    (if (tlist-empty-p tl)
        (setf (car tl) x)
        (Setf (cddr tl) x))
    (setf (cdr tl) x)))

(declaim (inline tlist-rem-left))

(defun tlist-rem-left (tl)
  (if (tlist-empty-p tl)
      (error "Remove from empty tlist")
      (let ((x (car tl)))
        (setf (car tl) (cdar tl))
        (if (tlist-empty-p tl)
            (setf (cdr tl) nil))
        (car x))))

(declaim (inline tlist-update))

(defun tlist-update (tl)
  (setf (cdr tl) (last (car tl))))

(defvar number-of-conses 0)

(declaim (inline counting-cons))

(defun counting-cons (a b)
  (incf number-of-conses)
  (cons a b))

(defmacro! with-conses-counted (&rest body)
  `(let ((,g!orig number-of-conses))
     ,@body
     (- number-of-conses ,g!orig)))

(defmacro counting-push (obj stack)
  `(setq ,stack (counting-cons ,obj ,stack)))

(let (stack)
  (with-conses-counted
      (loop for i from 1 to 100 do
           (counting-push nil stack)
           (pop stack))))

(defmacro with-cons-pool (&rest body)
  `(let ((cons-pool)
         (cons-pool-count 0)
         (cons-pool-limit 100))
     (declare (ignorable cons-pool
                         cons-pool-count
                         cons-pool-limit))
     ,@body))

(defmacro! cons-pool-cons (o!car o!cdr)
  `(if (= cons-pool-count 0)
       (counting-cons ,g!car ,g!cdr)
       (let ((,g!cell cons-pool))
         (decf cons-pool-count)
         (setf cons-pool (cdr cons-pool))
         (setf (car ,g!cell) ,g!car
               (cdr ,g!cell) ,g!cdr)
         ,g!cell)))

(defmacro! cons-pool-free (o!cell)
  `(when (<= cons-pool-count
             (- cons-pool-limit 1))
     (incf cons-pool-count)
     (setf (Car ,g!cell) nil)
     (push ,g!cell cons-pool)))

(defmacro make-cons-pool-stack ()
  `(let (stack)
     (dlambda
      (:push (elem)
             (setf stack
                   (cons-pool-cons elem stack)))
      (:pop ()
            (if (null stack)
                (error "Tried to pop an empty stack."))
            (let ((cell stack)
                  (elem (car stack)))
              (setf stack (cdr stack))
              (cons-pool-free cell)
              elem)))))

(with-cons-pool
    (let ((stack (make-cons-pool-stack)))
      (with-conses-counted
          (loop for i from 1 to 100 do
               (funcall stack :push nil)
               (funcall stack :pop)))))

(with-cons-pool
    (defun make-shared-cons-pool-stack ()
      (make-cons-pool-stack)))

(defmacro with-dynamic-cons-pools (&rest body)
  `(locally (declare (special cons-pool
                              cons-pool-count
                              cons-pool-limit))
     ,@body))

(defmacro fill-cons-pool ()
  `(let (tp)
     (loop for i from cons-pool-count
        to cons-pool-limit
        do (push
            (cons-pool-cons nil nil)
            tp))
     (loop while tp
        do (cons-pool-free (pop tp)))))
