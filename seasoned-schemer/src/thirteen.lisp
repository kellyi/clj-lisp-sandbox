(defpackage thirteen
  (:use :cl))
(in-package :seasoned-schemer)

(defun intersect (set1 set2)
  "Find the intersection between two sets."
  (labels ((I (set)
             (cond
               ((null? set) '())
               ((member? (car set) set2)
                (cons (car set)
                      (I (cdr set))))
               (t (I (cdr set))))))
    (I set1)))

(intersect '(hello world) '(foo bar baz))
(intersect '(foo bar baz) '(hello world foo))

(defun intersectall (lset)
  "Find the intersection of a list of sets."
  (labels ((A (lset)
             (cond
               ((null? (cdr lset)) (car lset))
               (t (intersect (car lset)
                             (A (cdr lset)))))))
    (cond
      ((null? lset) '())
      (t (A lset)))))

(intersectall '((3 mangoes and)
                (3 kiwis and)
                (3 hamburgers)))

(defun intersectall-cc (lset)
  (catch 'hop
    (labels ((A (lset)
               (cond
                 ((null? (car lset)) (throw 'hop '()))
                 ((null? (cdr lset)) (car lset))
                 (t (intersect (car lset)
                               (A (cdr lset)))))))
      (cond
        ((null? lset) '())
        (t (A lset))))))

(intersectall-cc '((no mangoes and)
                   (10 kiwis and)
                   (3 hamburgers)))

(intersectall-cc '((3 mangoes and)
                   (3 kiwis and)
                   (3 hamburgers)))

(intersectall-cc '((3 steaks and)
                   (no food and)
                   (three baked potatoes)
                   (3 diet hamburgers)))

(defun intersectall-prime (lset)
  (catch 'hop
    (labels ((A (lset)
               (cond
                 ((null? (car lset)) (throw 'hop '()))
                 ((null? (cdr lset)) (car lset))
                 (t (I (car lset)
                       (A (cdr lset))))))
             (I (set1 set2)
               (labels ((J (set)
                          (cond
                            ((null? set) '())
                            ((member? (car set) set2)
                             (cons (car set)
                                   (J (cdr set))))
                            (t (J (cdr set))))))
                 (cond
                   ((null? set2) (throw 'hop '()))
                   (t (J set1))))))
      (cond
        ((null? lset) '())
        (t (A lset))))))

(intersectall-prime '((no mangoes and)
                      (10 kiwis and)
                      (3 hamburgers)))

(intersectall-prime '((3 mangoes and)
                      ()
                      (3 hamburgers)))

(intersectall-prime '((3 mangoes and)
                      (3 kiwis)
                      (3 hamburgers and)))

(defun rember-beyond-first (a lat)
  (labels ((R (lat)
             (cond
               ((null? lat) '())
               ((eq? (car lat) a) '())
               (t (cons (car lat)
                        (R (cdr lat)))))))
    (R lat)))

(rember-beyond-first 'desserts '(cookies
                                 chocolate mints
                                 caramel delight ginger snaps
                                 desserts
                                 chocolate mousse
                                 vanilla ice cream
                                 German chocolate cake
                                 more desserts
                                 gingerbreadman chocolate
                                 chip brownies))

(defun rember-upto-last (a lat)
  (catch 'skip
    (labels ((R (lat)
               (cond
                 ((null? lat) '())
                 ((eq? (car lat) a) (throw 'skip (R (cdr lat))))
                 (t (cons (car lat)
                          (R (cdr lat)))))))
      (R lat))))

(rember-upto-last 'roots '(noodles
                           spaghetti spatzle bean-thread
                           roots
                           potatoes yam
                           others
                           rice))

(rember-upto-last 'cookies '(cookies
                             chocolate mints
                             caramel delight ginger snaps
                             desserts
                             chocolate mousse
                             vanilla ice cream
                             German chocolate cake
                             more desserts
                             gingerbreadman chocolate
                             chip brownies))
