#lang br

(syntax->datum #'foo)
(syntax->datum #'(+ 1 2 3))

(define stx #'foo)
(syntax-line stx)
(syntax-column stx)
(syntax-span stx)
(syntax-srcloc stx)

(define stx+prop
  (syntax-property stx 'hello "world"))

(syntax? stx+prop)
(syntax-property stx+prop 'hello)

(syntax (+ 1 2))
#'(+ 1 2)

(syntax->datum #'(+ 1 2))

(define-macro (make-z OUTSIDE-ID)
  (with-pattern ([Z-ID (datum->syntax #'OUTSIDE-ID 'z)])
    #'(define Z-ID 42)))

(make-z out-here)

(define-macro (rearrange XS)
  (with-pattern ([(1ST 2ND 3RD) #'XS])
    #'(list 3RD 2ND 1ST 3RD)))

(rearrange (10 20 30))

(syntax->list #'(10 20 30))

(define-macro (rev ARGS)
  (define arg-stxs (syntax->list #'ARGS))
  (with-pattern ([(REVERSED-ARG ...) (reverse arg-stxs)])
    #'(list REVERSED-ARG ...)))

(rev (10 20 30))

(define num 1)
(define nums '(2 3))
`(+ ,num ,@nums)
#`(+ #,num #,@nums)

[define [hello name] [println {string-append "hello " name}]]
{hello "kelly"}
