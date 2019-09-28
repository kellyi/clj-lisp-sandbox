#lang br/quicklang

(require json)

(define-macro (jsonic-mb PARSE-TREE)
  #'(#%module-begin
     (define result-string PARSE-TREE)
     (define validate-s-expr (string->jsexpr result-string))
     (display result-string)))
(provide (rename-out [jsonic-mb #%module-begin]))

(define-macro (jsonic-char CHAR-TOK-VALUE)
  #'CHAR-TOK-VALUE)
(provide jsonic-char)

(define-macro (jsonic-program S-EXP-OR-JSON-STR ...)
  #'(string-trim (string-append S-EXP-OR-JSON-STR ...)))
(provide jsonic-program)

(define-macro (jsonic-s-exp S-EXP-STR)
  (with-pattern ([S-EXP-DATUM (format-datum '~a #'S-EXP-STR)])
    #'(jsexpr->string S-EXP-DATUM)))
(provide jsonic-s-exp)
