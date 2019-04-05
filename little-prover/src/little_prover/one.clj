(ns little-prover.one
  (:require [little-prover.jbob :refer :all]))

(equal 'ham (car (cons 'ham '(eggs))))

(equal 't (atom '()))
