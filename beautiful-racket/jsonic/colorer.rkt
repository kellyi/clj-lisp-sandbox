#lang br
(require brag/support
         syntax-color/racket-lexer
         racket/contract)

(define jsonic-lexer
  (lexer
   [(eof) (values lexeme
                  'eof
                  #f
                  #f
                  #f)]
   [(:or "@$" "$@")
    (values lexeme
            'parenthesis
            (if (equal? lexeme "@$")
                '|(|
                '|)|)
            (pos lexeme-start)
            (pos lexeme-end))]
   [(from/to "//" "\n")
    (values lexeme
            'comment
            #f
            (pos lexeme-start)
            (pos lexeme-end))]
   [any-char
    (values lexeme
            'string
            #f
            (pos lexeme-start)
            (pos lexeme-end))]))

(define (color-jsonic port offset racket-coloring-mode?)
  (cond
    [(or (not racket-coloring-mode?)
         (equal? (peek-string 2 0 port) "$@"))
     (define-values (str cat paren start end)
       (jsonic-lexer port))
     (define switch-to-racket-mode (equal? str "@$"))
     (values str cat paren start end 0 switch-to-racket-mode)]
    [else
     (define-values (str cat paren start end)
       (racket-lexer port))
     (values str cat paren start end 0 #t)]))

(provide
 (contract-out
  [color-jsonic
   (input-port? exact-nonnegative-integer? boolean?
                . -> . (values
                        (or/c string? eof-object?)
                        symbol?
                        (or/c symbol? #f)
                        (or/c exact-positive-integer? #f)
                        (or/c exact-positive-integer? #f)
                        exact-nonnegative-integer?
                        boolean?))]))

(module+ test
  (require rackunit)
  (check-equal? (values->list
                 (color-jsonic (open-input-string "x") 0 #f))
                (list "x" 'string #f 1 2 0 #f)))
