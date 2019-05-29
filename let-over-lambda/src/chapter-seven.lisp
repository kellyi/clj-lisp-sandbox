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

(defvar bad-3-sn
  '((0 1) (0 2) (1 2)))

(defvar good-3-sn
  '((0 2) (0 1) (1 2)))

(defvar tracing-interpret-sn nil)

(defun interpret-sn (data sn)
  (let ((step 0) (swaps 0))
    (dolist (i sn)
      (if tracing-interpret-sn
          (format t "Step ~a: ~a~%" step data))
      (if (> #1=(nth (car i) data)
             #2=(nth (cadr i) data))
          (progn
            (rotatef #1# #2#)
            (incf swaps)))
      (incf step))
    (values swaps data)))

(let ((tracing-interpret-sn t))
  (interpret-sn '(1 2 3) bad-3-sn))

(let ((tracing-interpret-sn t))
  (interpret-sn '(1 2 3) good-3-sn))

(let ((tracing-interpret-sn t))
  (interpret-sn '(3 1 2) bad-3-sn))

(let ((tracing-interpret-sn t))
  (interpret-sn '(3 1 2) good-3-sn))

(let ((tracing-interpret-sn t))
  (interpret-sn '(3 2 1) bad-3-sn))

(let ((tracing-interpret-sn t))
  (interpret-sn '(3 2 1) good-3-sn))

(defun all-sn-perms (n)
  (let (perms curr)
    (funcall
     (alambda (left)
       (if left
           (loop for i from 0 to (1- (length left)) do
                (push (nth i left) curr)
                (self (append (subseq left 0 i)
                              (subseq left (1+ i))))
                (pop curr))
           (push curr perms)))
     (loop for i from 1 to n collect i))
    perms))

(all-sn-perms 3)

(defun average-swaps-calc (n sn)
  (/ (loop for i in (all-sn-perms n) sum
          (interpret-sn (copy-list i) sn))
     (fact n)))

(average-swaps-calc 3 bad-3-sn)

(average-swaps-calc 3 good-3-sn)

(defun build-batched-sn (n)
  (let* (network
         (tee (ceiling (log n 2)))
         (p (ash 1 (- tee 1))))
    (loop while (> p 0) do
         (let ((q (ash 1 (- tee 1)))
               (r 0)
               (d p))
           (loop while (> d 0) do
                (loop for i from 0 to (- n d 1) do
                     (if (= (logand i p) r)
                         (push (list i (+ i d))
                               network)))
                (setf d (- q p)
                      q (ash q -1)
                      r p)))
         (setf p (ash p -1)))
    (nreverse network)))

(build-batched-sn 3)

(build-batched-sn 7)

(defun prune-sn-for-median (elems network)
  (let ((mid (floor elems 2)))
    (nreverse
     (if (evenp elems)
         (prune-sn-for-median-aux
          (reverse network)
          (list (1- mid) mid))
         (prune-sn-for-median-aux
          (reverse network)
          (list mid))))))

(defun prune-sn-for-median-aux (network contam)
  (if network
      (if (intersection (car network) contam)
          (cons (car network)
                (prune-sn-for-median-aux
                 (cdr network)
                 (remove-duplicates
                  (append (car network) contam))))
          (prune-sn-for-median-aux
           (cdr network) contam))))

(defun prune-sn-for-median-calc (n)
  (loop for i from 2 to n collect
       (let* ((sn (build-batched-sn i))
               (snp (prune-sn-for-median i sn)))
            (list i
                  (length sn)
                  (length snp)))))

(interpret-sn
 '(4 2 3 7 6 1 5)
 (prune-sn-for-median
  7 (build-batched-sn 7)))

(prune-sn-for-median-calc 49)

(defun sn-to-lambda-form* (sn)
  `(lambda (arr)
     #f
     (declare (type (simple-array fixnum) arr))
     ,@(mapcar
        #`(if (> #1=(aref arr ,(car a1))
                 #2=(aref arr ,(cadr a1)))
              (rotatef #1# #2#))
        sn)
     arr))

(eval
 (sn-to-lambda-form*
  (build-batched-sn 3)))

(compile nil *)

(disassemble *)

(disassemble
 (compile nil
          (sn-to-lambda-form*
           (build-batched-sn 3))))

(defun sn-to-lambda-form (sn)
  `(lambda (arr)
     #f
     (declare (type (simple-array fixnum) arr))
     ,@(mapcar
        #`(let ((a #1=(aref arr ,(car a1)))
                (b #2=(aref arr ,(cadr a1))))
            (if (> a b)
                (setf #1# b
                      #2# a)))
        sn)
     arr))

(defmacro! sortf (comparator &rest places)
  (if places
      `(tagbody
          ,@(mapcar
             #`(let ((,g!a #1=,(nth (car a1) places))
                     (,g!b #2=,(nth (cadr a1) places)))
                 (if (,comparator ,g!b ,g!a)
                     (setf #1# ,g!b
                           #2# ,g!a)))
             (build-batched-sn (length places))))))

(macroexpand
 '(sortf < a b c))

(let ((a -3) (b 2))
  (sortf (lambda (a b) (< (abs a) (abs b)))
         a b)
  (list a b))

(let ((a 2) (b '(4)) (c #(3 1)))
  (sortf < a (car b) (aref c 0) (aref c 1))
  (format t "a=~a b=~a c=~a~%" a b c))

(defmacro sort-benchmark-time ()
  `(progn
     (setq sorter (compile nil sorter))
     (let ((arr (make-array
                 n :element-type 'fixnum)))
       (time
        (loop for i from 1 to iters do
             (loop for j from 0 to (1- n) do
                  (setf (aref arr j) (random n)))
             (funcall sorter arr))))))

(defun do-sort-benchmark (n iters)
  (let ((rs (make-random-state *random-state*)))
    (format t "CL sort:~%")
    (let ((sorter
           '(lambda (arr)
             #f
             (declare (type (simple-array fixnum)
                       arr))
             (sort arr #'<))))
      (sort-benchmark-time))
    (setf *random-state* rs)
    (format t "sortf:~%")
    (let ((sorter
           `(lambda (arr)
              #f
              (declare (type (simple-array fixnum)
                             arr))
              (sortf <
                     ,@(loop for i from 0 to (1- n)
                          collect `(aref arr ,i)))
              arr)))
      (sort-benchmark-time))))

(compile 'do-sort-benchmark)

(do-sort-benchmark 2 1000000)

(do-sort-benchmark 25 1000000)

(do-sort-benchmark 49 1000000)
