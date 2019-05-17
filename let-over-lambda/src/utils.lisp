(defpackage utils
  (:use :cl))

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
             (cond
               ((null x) acc)
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
