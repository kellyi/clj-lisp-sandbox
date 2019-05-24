(defpackage chapter-five
  (:use :cl)
  (:use :utils))

(unit-of-time 1 d)

(defmacro! defunits* (quantity base-unit &rest units)
  `(defamacro ,(symb 'unit-of- quantity) (,g!val ,g!un)
     `(* ,.g!val
         ,(case ,g!un
            ((,base-unit) 1)
            ,@(mapcar (lambda (x)
                        `((,(car x)) ,(cadr x)))
                      (group units 2))))))

(defun defunits-chaining* (u units)
  (let ((spec (find u units :key #'car)))
    (if (null spec)
        (error "Unknown unit ~a" u)
        (let ((chain (cadr spec)))
          (if (listp chain)
              (* (car chain)
                 (defunits-chaining*
                     (cadr chain)
                     units))
              chain)))))

(defmacro! defunits** (quantity base-unit &rest units)
  `(defmacro ,(symb 'unit-of quantity) (,g!val ,g!un)
     `(* ,.g!val
         ,(case ,g!un
            ((,base-unit) 1)
            ,@(mapcar (lambda (x)
                        `((,(car x))
                          ,(defunits-chaining*
                               (car x)
                               (cons `(,base-unit 1)
                                     (group units 2)))))
                      (group units 2))))))

(defun defunits-chaining (u units prev)
  (if (member u prev)
      (error "~{ ~a~^ depends on ~}"
             (cons u prev)))
  (let ((spec (find u units :key #'car)))
    (if (null spec)
        (error "Unknown unit ~a" u)
        (let ((chain (cadr spec)))
          (if (listp chain)
              (* (car chain)
                 (defunits-chaining
                     (cadr chain)
                     units
                   (cons u prev)))
              chain)))))

(defmacro! defunits (quantity base-unit &rest units)
  `(defmacro ,(symb 'unit-of- quantity)
       (,g!val ,g!un)
     `(* ,,g!val
         ,(case ,g!un
            ((,base-unit) 1)
            ,@(mapcar (lambda (x)
                        `((,(car x))
                          ,(defunits-chaining
                               (car x)
                               (cons
                                `(,base-unit 1)
                                (group units 2))
                             nil)))
                      (group units 2))))))

;; (defunits time s
;;   m (1/60 h)
;;   h (60 m)) -> error

(defunits distance m
  km 1000
  cm 1/100
  mm (1/10 cm)
  nm (1/1000 mm)

  yard 9144/10000
  foot (1/3 yard)
  inch (1/12 foot)
  mile (1760 yard)
  furlong (1/8 mile)

  fathom (2 yard)
  nautical-mile 1852
  cable (1/10 nautical-mile)

  old-brit-nautical-mile (6080/3 yard)
  old-brit-cable (1/10 old-brit-nautical-mile)
  old-brit-fathom (1/100 old-brit-cable))

(/ (unit-of-distance 1 fathom)
   (unit-of-distance 1 old-brit-fathom))

(coerce
 (unit-of-distance 1/76 old-brit-fathom)
 'float)
