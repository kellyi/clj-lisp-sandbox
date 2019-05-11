(defpackage chapter-four
  (:use :cl))

(make-array '(2 3) :initial-element 'a)

(defvar *arr* (make-array '(2 3) :initial-element nil))

(aref *arr* 0 0)

(setf (aref *arr* 0 0) 'b)

(aref *arr* 0 0)

(svref (vector 'a :b 3) 0)

(defun bin-search (obj vec)
  "Find an obj in a sorted vec."
  (labels ((finder (obj vec start end)
             (format t "~A~%" (subseq vec start (+ end 1)))
             (let ((range (- end start)))
               (if (zerop range)
                   (if (eql obj (aref vec start))
                       obj
                       nil)
                   (let ((mid (+ start (round (/ range 2)))))
                     (let ((obj2 (aref vec mid)))
                       (if (< obj obj2)
                           (finder obj vec start (- mid 1))
                           (if (> obj obj2)
                               (finder obj vec (+ 1 mid) end)
                               obj))))))))
    (let ((len (length vec)))
      (and (not (zerop len))
           (finder obj vec 0 (- len 1))))))

(bin-search 3 #(0 1 2 3 4 5 6 7 8 9))

(position '(a b) '((a b) (c d)))

(position '(a b) '((a b) (c d) :test #'equal))

(defun second-word (s)
  "Find the second word in a string."
  (let ((p1 (+ (position #\  s) 1)))
    (subseq s p1 (position #\  s :start p1))))

(position-if #'oddp '(2 3 4 5 6))

(defun tokens (str test start)
  "Extract tokens satisfying test from str starting at start."
  (let ((p1 (position-if test str :start start)))
    (if p1
        (let ((p2 (position-if #'(lambda (c)
                                   (not (funcall test c)))
                               str :start p1)))
          (cons (subseq str p1 p2)
                (if p2
                    (tokens str test p2)
                    nil)))
        nil)))

(defun constituent (c)
  "Tokenize on whitespace."
  (and (graphic-char-p c)
       (not (char= c #\  ))))

(tokens "ab12 3cde.f gh" #'constituent 0)

(defparameter *month-names*
    #("jan" "feb" "mar" "apr" "mar" "jun"
      "jul" "aug" "sep" "oct" "nov" "dec"))

(defun parse-month (str)
  (let ((p (position str *month-names*
                     :test #'string-equal)))
    (if p
        (1+ p)
        nil)))

(defun parse-date (str)
  (let ((toks (tokens str #'constituent 0)))
    (list (parse-integer (first toks))
          (parse-month (second toks))
          (parse-integer (third toks)))))

(parse-date "16 Aug 1980")

;; Exercise 2
(defun copy-list-prime (l)
  (reduce #'(lambda (next acc)
              (cons next acc))
              l
              :initial-value '()
              :from-end t))

(defun reverse-prime (l)
  (reverse (copy-list-prime l)))
