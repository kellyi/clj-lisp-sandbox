#lang br/quicklang
(require brag/support)

(define (make-tokenizer port)
  (define (next-token)
    (define jsonic-lexer
      (lexer
       [(from/to "//" "\n") (next-token)]
       [(from/to "@$" "$@")
        (token 'S-EXP-TOK (trim-ends "@$"
                                     lexeme
                                     "$@"))]
       [any-char (token 'CHAR-TOK lexeme)]))
    (jsonic-lexer port))
  next-token)
(provide make-tokenizer)
