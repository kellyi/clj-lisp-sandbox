(defpackage overview
  (:use :cl))
(in-package :paip)

(defstruct name
  first
  (middle nil)
  last)

(setf b (make-name :first 'Barney :last 'Rubble))
(name-first b)
(name-middle b)
(name-last b)
(name-p b)
(name-p '())
(setf (name-middle b) 'Q)
(princ b)

(let* ((x 6)
       (y (* x x)))
  (+ x y))

(setq n 0)
(incf n)
(decf n)

(defstruct player (score 0) (wins 0))

(defun determine-winner (players)
  "Incremene the wins for the player with the higest score"
  (incf (player-wins (first (sort players #'>
                                  :key #'player-score)))))

(defun determine-winner (players)
  "Increment the wins for the player with the highest score"
  (let ((temp (first (sort players #'> :key #'player-score))))
    (setf (player-wins temp) (+ (player-wins temp) 1))))

(defun length1 (list)
  (let ((len 0))
    (dolist (element list len)
      (incf len))))

(defun length2 (list)
  (let ((len 0))
    (mapc #'(lambda (element)
              (incf len))
          list)
    len))

(defun length3 (list)
  (do ((len 0 (+ len 1))
       (l list (rest l)))
      ((null l) len)))

(defun length4 (list)
  (loop for element in list
        count t))

(defun length5 (list)
  (loop for element in list
        summing 1))

(defun length6 (list)
  (loop with len = 0
        until (null list)
        for element = (pop list)
        do (incf len)
        finally (return len)))

((defun true (x) t)

(defun length7 (list)
  (count-if #'true list))

(defun length8 (list)
  (if (null list)
      0
      (+ 1 (position-if #'true list :from-end t))))

(mapcar #'- '(1 2 3))
(mapcar #'+ '(1 2) '(10 20))
(mapcar #'+ '(1 2) '(10 20) '(100 200))

(remove 1 '(1 2 3 2 1 0 -1))
(remove 1 '(1 2 3 2 1 0 -1) :key #'abs)
(remove 1 '(1 2 3 2 1 0 -1) :test #'<)
(remove 1 '(1 2 3 2 1 0 -1) :start 4)
(remove-if #'oddp '(1 2 3 2 1 0 -1))
(remove-if-not #'oddp '(1 2 3 2 1 0 -1))
(find-if #'evenp '(1 2 3 2 1 0 -1))

(setf x '(a b c))
(setf y '(1 2 3))

(every #'oddp y)
(some #'oddp y)
(mapcar #'- y)
(mapc #'print y)
(member 2 y)
(count 'b x)
(delete 1 y)
(find 2 y)
(position 'a x)
(reduce #'+ y)
(remove 2 y)
(substitute 4 2 y)

(defun length9 (list)
  (if (null list)
      0
      (+ 1 (length9 (rest list)))))

(defun length10-aux (sublist len-so-far)
  (if (null sublist)
      len-so-far
      (length10-aux (rest sublist) (+ 1 len-so-far))))

(defun length10 (list)
  (length10-aux list 0))

(defun length11 (list &optional (len-so-far 0))
  (if (null list)
      len-so-far
      (length11 (rest list) (+ 1 len-so-far))))

(defun length12 (the-list)
  (labels
      ((length13 (list len-so-far)
         (if (null list)
             len-so-far
             (length13 (rest list) (+ 1 len-so-far)))))
    (length13 the-list 0)))

(progn (setf x 0) (setf x (+ x 1)) x)

(trace length9)
(length9 '(a b c))

(defun product (numbers)
  "Multiply all the numbers together to compute their product"
  (let ((prod 1))
    (dolist (n numbers prod)
      (if (= n 0)
          (RETURN 0)
          (setf prod (* n prod))))))

(defmacro while (test &rest body)
  "Repeat body while test is true"
  (list* 'loop
         (list 'unless test '(return nil))
         body))

(macroexpand-1 '(while (< i 10)
                 (print (* i i))
                 (setf i (+ i 1))))

(setf i 7)
(while (< i 10)
       (print (* i i))
       (setf i (+ 1 i)))

(setf state-table
      '((AL . Alabama)
        (AK . Alaska)
        (AZ . Arizona)
        (AR . Arkansas)))

(assoc 'AK state-table)

(cdr (assoc 'AK state-table))

(rassoc 'Arizona state-table)

(setf tree '((a b) ((c)) (d e)))
(tree-equal tree (copy-tree tree))

(defun true (&rest ignore) t)

(defun same-shape-tree (a b)
  "Are two trees the same except for the leaves?"
  (tree-equal a b :test 'true))

(same-shape-tree tree '((1 2) ((3)) (4 5)))
(same-shape-tree tree '((1 2) (3) (4 5)))

(defun english->french (words)
  (sublis '((are . va) (book . libre) (friend . ami)
            (hello . bonjour) (how . comment) (my . mon)
            (red . rouge) (you . tu))
          words))

(english->french '(hello my friend - how are you today?))

(setf r '(a b c d))

(setf s '(c d e))

(intersection r s)

(union r s)

(set-difference r s)

(member 'd r)

(type-of 123)

(format t "hello world")

(format t "~&~a plus ~s is ~f" "two" "two" 4)

(step (+ 3 4 (* 5 6 (/ 7 8))))

(apropos 'string)

(describe 'make-string)

(describe 1234.56)

(documentation 'first 'function)

(documentation 'pi 'variable)

(defun average (numbers)
  (if (null numbers)
      (error "Average of the empty list is undefined")
      (/ (reduce #'+ numbers)
         (length numbers))))

(average '())

(average '(1 2 3))

(defun sqr (x)
  "Multiply x by itself"
  (check-type x number)
  (* x x))

(defun sqr2 (x)
  "Multiply x by itself"
  (assert (numberp x))
  (* x x))

(defun positive (x)
  (assert (> x -1))
  x)

(defun f (n)
  (dotimes (i n) nil))

(+ 1 2 3 4)
(funcall #'+ 1 2 3 4)
(apply #'+ '(1 2 3 4))
(apply #'+ 1 2 '(3 4))
(eval '(+ 1 2 3 4))

(defun foo (x y)
  (values x y (+ x y) (cons x y)))

(multiple-value-bind (a b sum pair) (foo 1 2)
  (list a b sum pair))

(defun find-all (item sequence &rest keyword-args
                      &key (test #'eql) test-not &allow-other-keys)
  "Find all the elements of a sequence that match item according to the keywords"
  (if test-not
      (apply #'remove item sequence
             :test-not (complement test-not) keyword-args)
    (apply #'remove item sequence
           :test (complement test) keyword-args)))

(find-all 1 '(1 2 3 2 1) :test #'=)
