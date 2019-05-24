(defpackage chapter-four
  (:use :cl)
  (:use :utils))

`(football-game
  (game-started-at
   #.(get-internal-real-time))
  (coin-flip
   #.(if (zerop (random 2))
         'heads
         'tails)))

(equal * (eval +))

(let ((s 'hello))
  `(,s world))

(let ((s '(b c d)))
  `(a . ,s)) ;; . , -> unquote and insert at cdr of list

(let ((s '(b c d)))
  `(a ,@s e)) ;; ,@list -> spread operator

(defvar to-splice '(b c d))

`(a .,to-splice)
to-splice

;; (defun dangerous-use-of-bq ()
;;   `(a ,.'(b c d) e))

;; (dangerous-use-of-bq) -> circular second time called

(defun safer-use-of-bq ()
  `(a
    ,.(mapcar #'identity '(b c d))
    e))

(let (*print-pretty*)
  (print
   '`(football-game
      (game-started-at
       ,(get-internal-real-time))
      (coin-flip
       ,(if (zerop (random 2))
            'heads
            'tails))))
  t)

(let ((let '`(let ((let ',let))
              ,let)))
  `(let ((let ',let)) ,let))

(defun |#"-reader| (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let (chars)
    (do ((prev (read-char stream) curr)
         (curr (read-char stream) (read-char stream)))
        ((and (char= prev #\") (char= curr #\#)))
      (push prev chars))
    (coerce (nreverse chars) 'string)))

(set-dispatch-macro-character
 #\# #\" #'|#"-reader|)

(defun |#>-reader| (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let (chars)
    (do ((curr (read-char stream)
               (read-char stream)))
        ((char= #\newline curr))
      (push curr chars))
    (let* ((pattern (nreverse chars))
           (pointer pattern)
           (output))
      (do ((curr (read-char stream)
                 (read-char stream)))
          ((null pointer))
        (push curr output)
        (setf pointer
              (if (char= (car pointer) curr)
                  (cdr pointer)
                  pattern))
        (if (null pointer)
            (return)))
      (coerce
       (nreverse
        (nthcdr (length pattern) output))
       'string))))

(set-dispatch-macro-character
 #\# #\> #'|#>-reader|)

(defun segment-reader (stream ch n)
  (if (> n 0)
      (let ((chars))
        (do ((curr (read-char stream)
                   (read-char stream)))
            ((char= ch curr))
          (push curr chars))
        (cons (coerce (nreverse chars) 'string)
              (segment-reader stream ch (1- n))))))

#+cl-pprce
(defmacro! match-mode-ppcre-lambda-form (o!args)
  ``(lambda (,',g!str)
      (cl-ppcre:scan
       ,(car ,g!args)
       ,',g!str)))

#+cl-ppcre
(defmacro! subst-mode-ppcre-lambda-form (o!args)
  ``(lambda (,',g!str)
      (cl-ppcre:regex-replace-all
       ,(car ,g!args)
       ,',g!str
       ,(cadr ,g!args))))

(defvar not-shared '((1) (1)))

(eq (car not-shared) (cadr not-shared))

(defvar shared '(#1=(1) #1#))

(eq (car shared) (cadr shared))

(list
 #1=(list 0)
 #1#
 #1#)

(list (list 0) (list 0) (list 0))

(let ((*print-circle* t))
  (print ++)
  t)

(list #1=(list 0) #1# #1#)

(print '#1=(hello . #1#))

(let ((*print-circle* t))
  (print '#1=(hello . #1#))
  nil)

(sb-ext:run-program
 "/bin/echo" '("hello"))
