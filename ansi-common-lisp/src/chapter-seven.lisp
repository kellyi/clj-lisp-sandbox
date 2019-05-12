(defpackage chapter-seven
  (:use :cl))

(make-pathname :name "myfile")

(progn
  (format t "Please enter your name: ")
  (read-line))

(prin1 "Hello")
(princ "Hello")

(format nil "~10,2,0,'*,' F" 26.21875)

(car (read-from-string "'a"))
