(defpackage chapter-eleven
  (:use :cl))

(defclass rectangle ()
  (height width))

(defclass circle ()
  (radius))

(defmethod area ((x rectangle))
  (* (slot-value x 'height) (slot-value x 'width)))

(defmethod area ((x circle))
  (* pi (exp (slot-value x 'radius))))

(let ((r (make-instance 'rectangle)))
  (setf (slot-value r 'height) 2
        (slot-value r 'width) 3)
  (area r))

(let ((c (make-instance 'circle)))
  (setf (slot-value c 'radius) 5)
  (area c))

(defclass circle* ()
  ((radius :accessor circle-radius)
   (center :accessor circle-center)))

(let ((c* (make-instance 'circle*)))
  (setf (circle-radius c*) 1
        (circle-center c*) 1))

(defclass sculpture () (height width depth))
(defclass statue (sculpture) (subject))
(defclass metalwork () (metal-type))
(defclass casting (metalwork) ())
(defclass cast-statue (statue casting) ())

(defclass speaker () ())

(defmethod speak ((s speaker) string)
  (format t "~A" string))

(speak (make-instance 'speaker)
       "Hello world")

(defclass intellectual (speaker) ())

(defmethod speak :before ((i intellectual) string)
  (princ "Perhaps "))

(defmethod speak :after ((i intellectual) string)
  (princ " in some sense."))

(speak (make-instance 'intellectual)
       "I'm hungry")

(defmethod speak :before ((s speaker) string)
  (princ "I think "))

(speak (make-instance 'intellectual)
       "I'm hungry")
