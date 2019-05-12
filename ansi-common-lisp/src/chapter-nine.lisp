(defpackage chapter-nine
  (:use :cl))

(defun palindrome? (x)
  "Is x a palindrome?"
  (let ((mid (/ (length x) 2)))
    (equal (subseq x 0 (floor mid))
           (reverse (subseq x (ceiling mid))))))

(defun sq (x)
  "Square x."
  (* x x))

(defun mag (x y z)
  "Find the distance between two points in 3-D space."
  (sqrt (+ (sq x) (sq y) (sq z))))

(defun unit-vector (x y z)
  "Create a unit vector with the same direction as x y z."
  (let ((d (mag x y z)))
    (values (/ x d) (/ y d) (/ z d))))

(defstruct (point (:conc-name nil))
  x y z)

(defun distance (p1 p2)
  "Calculate the distance between p1 and p2."
  (mag (- (x p1) (x p2))
       (- (y p1) (y p2))
       (- (z p1) (z p2))))

(defun minroot (a b c)
  "Return the smallest real x for which ax^2 + bx + c = 0."
  (if (zerop a)
      (/ (- c) b)
      (let ((disc (- (sq b) (* 4 a c))))
        (unless (minusp disc)
          (let ((discrt (sqrt disc)))
            (min (/ (+ (- b) discrt) (* 2 a))
                 (/ (- (- b) discrt) (* 2 a))))))))

(defstruct surface  color)

(defparameter *world* nil)

(defconstant eye (make-point :x 0 :y 0 :z 200))

(defun lambert (s int xr yr zr)
  "Return the amount of light shining on surface s."
  (multiple-value-bind (xn yn zn) (normal s int)
    (max 0 (+ (* xr xn) (* yr yn) (* zr zn)))))

(defun first-hit (pt xr yr zr)
  "Examine all surfaces in *world* and return the surface the ray hits first
  or the default background color if no surface was hit."
  (let (surface hit dist)
    (dolist (s *world*)
      (let ((h (intersect s pt xr yr zr)))
        (when h
          (let ((d (distance h pt)))
            (when (or (null dist) (< d dist))
              (setf surface s hit h dist d))))))
    (values surface hit)))

(defun sendray (pt xr yr zr)
  (multiple-value-bind (s int) (first-hit pt xr yr zr)
    (if s
        (* (lambert s int xr yr zr) (surface-color s))
        0)))

(defun color-at (x y)
  "Compute the color at x/y."
  (multiple-value-bind (xr yr zr)
      (unit-vector (- x (x eye))
                   (- y (y eye))
                   (- 0 (z eye)))
    (round (* (sendray eye xr yr zr) 255))))

(defun tracer (pathname &optional (res 1))
  "Write a ray tracer image to pathname."
  (with-open-file (p pathname :direction :output)
    (format p "P2 ~A ~A 255" (* res 100) (* res 100))
    (let ((inc (/ res)))
      (do ((y -50 (+ y inc)))
          ((< (- 50 y) inc))
        (do ((x -50 (+ x inc)))
            ((< (- 50 x) inc))
          (print (color-at x y) p))))))

(defstruct (sphere (:include surface))
  radius center)

(defun defsphere (x y z r c)
  (let ((s (make-sphere
            :radius r
            :center (make-point :x x :y y :z z)
            :color c)))
    (push s *world*)
    s))

(defun intersect (s pt xr yr zr)
  (funcall (typecase s (sphere #'sphere-intersect))
           s pt xr yr zr))

(defun sphere-intersect (s pt xr yr zr)
  (let* ((c (sphere-center s))
         (n (minroot (+ (sq xr) (sq yr) (sq zr))
                     (* 2 (+ (* (- (x pt) (x c)) xr)
                             (* (- (y pt) (y c)) yr)
                             (* (- (z pt) (z c)) zr)))
                     (+ (sq (- (x pt) (x c)))
                        (sq (- (y pt) (y c)))
                        (sq (- (z pt) (z c)))
                        (- (sq (sphere-radius s)))))))
    (if n
        (make-point :x (+ (x pt) (* n xr))
                    :y (+ (y pt) (* n yr))
                    :z (+ (z pt) (* n zr))))))

(defun normal (s pt)
  (funcall (typecase s (sphere #'sphere-normal))
           s pt))

(defun sphere-normal (s pt)
  (let ((c (sphere-center s)))
    (unit-vector (- (x c) (x pt))
                 (- (y c) (y pt))
                 (- (z c) (z pt)))))

(defun ray-test (&optional (res 1))
  (setf *world* nil)
  (defsphere 0 -300 -1200 200 0.8)
  (defsphere -80 -150 -1200 200 .9)
  (do ((x -2 (1+ x)))
      ((> x 2))
    (do ((z 2 (1+ z)))
        ((> z 7))
      (defsphere (* x 200) 300 (* z -400) 40 0.75)))
  (tracer (make-pathname :name "spheres.pgm") res))
