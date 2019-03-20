(defpackage paip
  (:use :cl))
(in-package :paip)

(+ 2 2)
(+ 1 2 3 4 5 6 7 8 9 10)
(- (+ 9000 900 90 9) (+ 5000 500 50 5))

(append '(Pat Kim) '(Robin Sandy))

'John
'(John Q Public)

(append '(Pat Kim) (list '(John Q Public) 'Sandy))

(length (append '(Pat Kim) (list '(John Q Public) 'Sandy)))

(setf p '(John Q Public))
p

(setf x 10)
x
(+ x x)

(+ x (length p))

(first p)
(rest p)
(second p)
(third p)
(fourth p)
(length p)

(setf x '((1st element) 2 (element 3) ((4)) 5))
(length x)
(first (first (fourth x)))
(cons 'Mr p)

(setf town (list 'Anytown 'USA))
(list p 'of town 'may 'have 'already 'won!)
(append p '(of) town '(may have already won!))

(defun last-name (name)
  "Select the last name from a name represented as a list"
  (first (last name)))

(last-name p)
(last-name '(Rear Admiral Grace Murray Hopper))

(defun first-name (name)
  "Select the first name from a name represented as a list"
  (first name))

(first-name p)
(first-name '(Wilma Flintstone))

(setf names '((John Q Public) (Malcolm X)
              (Admiral Grace Murray Hopper) (Spot)
              (Aristotle) (A A Milne) (Z Z Top)
              (Sir Larry Olivier) (Miss Scarlet)))

(first-name (first names))

(mapcar #'last-name names)
(mapcar #'- '(1 2 3 4))
(mapcar #'+ '(1 2 3 4) '(10 20 30 40))
(mapcar #'first-name names)

(defparameter *titles*
  '(Mr Mrs Miss Ms Sir Madam Dr Admiral Major General)
  "A list of titles that can appear at the start of a name")

(defun first-name (name)
  "Select the first name from a name represented as a list"
  (if (member (first name) *titles*)
      (first-name (rest name))
      (first name)))

(defparameter *suffixes*
  '(Jr MD Esquire)
  "A list of suffixes that can appear at the end of a name")

(defun last-name (name)
  (if (member (first (last name)) *suffixes*)
      (last-name (butlast name))
      (first (last name))))

(last-name '(Morton Downey Jr))
(last-name '(Rex Morgan MD))

(mapcar #'first-name names)
(first-name '(Madam Major General Paula Jones))
;; (trace first-name)
(first-name '(John Q Public))
;; (untrace first-name)

(defun mappend (fn the-list)
  "Apply fn to each element of list and append the results"
  (apply #'append (mapcar fn the-list)))

(defun self-and-double (x)
  (list x (+ x x)))

(self-and-double 3)
(apply #'self-and-double '(3))

(mapcar #'self-and-double '(1 10 300))
(mappend #'self-and-double '(1 10 300))

(defun number-and-negation (x)
  "If x is a number, return a list of x and -x"
  (if (numberp x)
      (list x (- x))
      nil))

(defun numbers-and-negations (input)
  "Given a list, return only the numbers and their negations"
  (mappend #'number-and-negation input))

(numbers-and-negations '(testing 1 2 3 test))

(funcall #'+ 2 3)
(apply #'+ '(2 3))
(funcall #'(lambda (x) (+ x 2)) 4)

(mapcar #'(lambda (x) (+ x x)) '(1 2 3 4 5))

(mappend #'(lambda (l) (list l (reverse l)))
         '((1 2 3) (a b c)))

(defun my-exp (n pow)
  (if (= pow 1)
      n
      (my-exp (* n n) (- pow 1))))

(my-exp 3 2)

(defun count-anywhere (e l)
  (cond
    ((eq nil l) 0)
    ((eq e (first l)) (+ 1 (count-anywhere e (rest l))))
    ((listp (first l)) (+ (count-anywhere e (first l))
                          (count-anywhere e (rest l))))
    (t (count-anywhere e (rest l)))))

(count-anywhere 'e '(e l p e))
(count-anywhere 2 '(1 (2) 2 (1 ((1 (((2))))))))

(defun dot-product (one two)
  (apply #'+ (mapcar #'* one two)))

(dot-product '(10 20) '(3 4))
