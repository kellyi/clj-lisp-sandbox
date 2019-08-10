(defun digits (n &optional (digits-list '()))
  "Get digits in a number."
  (if (< n 10)
      (cons n digits-list)
      (digits
       (floor n 10)
       (cons (rem n 10)
             digits-list))))

(defun factorial (n)
  "Return the factorial of n."
  (cond
    ((zerop n) 1)
    ((zerop (1- n)) 1)
    (t (* n (factorial (1- n))))))

(defun sum-digits-factorials (n)
  "Sum the factorials of the digits of n."
  (apply #'+
         (mapcar #'factorial
                 (digits n))))

(defun equals-sum-digits-factorials-p (n)
  "Is n equal to the sum of its digits' factorials?"
  (eql n (sum-digits-factorials n)))

(defun solve ()
  "Sum numbers less than 10 million equal to the sums of their digits' factorials."
  (apply #'+
         (remove-if-not #'equals-sum-digits-factorials-p
                        (loop
                          for i
                          from 3
                            to 10000000
                          collect i))))
