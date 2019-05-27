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
