(defun multiple-of-three-p (n)
  "True if n is a multiple of three."
  (zerop (mod n 3)))

(defun multiple-of-five-p (n)
  "True if n is a multiple of five."
  (zerop (mod n 5)))

(defun multiple-of-two-three-five-p (n)
  "True if n is a multiple of 2, 3, or 5."
  (or
   (evenp n)
   (multiple-of-three-p n)
   (multiple-of-five-p n)))

(defun sieve-of-e (n)
  "Sieve of Eratosthones."
  (labels ((sieve-helper (range primes)
             (if (null range)
                 primes
                 (sieve-helper (remove-if
                                #'(lambda (z)
                                    (zerop (mod (car range) z)))
                                (cdr range))
                               (cons (car range) primes)))))
    (sieve-helper (remove-if
                   #'multiple-of-two-three-five-p
                   (loop for x from 2 to n collect x))
                  '(5 3 2))))

(defun solve (n)
  "Sum all primes below n."
  (reduce #'+ (sieve-of-e n)))

(solve 2000000)
