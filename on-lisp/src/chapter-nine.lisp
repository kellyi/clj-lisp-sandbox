(defpackage chapter-nine
  (:use :cl))

(defmacro incorrect-for ((var start stop) &body body)
  `(do ((,var ,start (1+ ,var))
        (limit ,stop))
       ((> ,var limit))
     ,@body))

(incorrect-for (x 1 5)
  (princ x))

;; (incorrect-for (limit 1 5)
;;   (princ limit)) -> error

(defmacro closure-for ((var start stop) &body body)
  `(do ((b #'(lambda (,var) ,@body))
        (count ,start (1+ count))
        (limit ,stop))
       ((> count limit))
     (funcall b count)))

(closure-for (limit 1 5)
  (princ limit))

(defmacro gensym-for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
         ((> ,var ,gstop))
       ,@body)))

(closure-for (gstop 1 5)
  (princ gstop))
