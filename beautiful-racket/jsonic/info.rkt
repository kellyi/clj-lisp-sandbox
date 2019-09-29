#lang info
(define collection "jsonic")
(define version "0.1")
(define scribblings '(("scribblings/jsonic.scrbl")))
(define test-omit-paths '("jsonic-test.rkt"))
(define deps '("base"
               "beautiful-racket-lib"
               "brag"
               "draw-lib"
               "gui-lib"
               "br-parser-tools-lib"
               "rackunit-lib"
               "syntax-color-lib"))
(define build-deps '("scribble-lib"))
