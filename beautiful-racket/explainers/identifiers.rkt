#lang br

(define prometheus "I am bound, therefore I am a variable.")
prometheus

(module mod1 br
  (define foo "zim")
  (provide foo))
(require (submod "." mod1))

foo

(module mod2 br
  (define bar "zam")
  (provide bar))
(require (submod "." mod2))

(define bar "bar")
bar

(define-macro (define-as-42 ID)
  #'(define ID 42))

(define-as-42 baz)
baz

(define (get-bar)
  (define bar 42)
  (let ([bar 92])
    bar))

(get-bar)

(define top 40)
(define (f1)
  (let ()
    (let ()
      (let ()
        top))))

(f1)

(define (f2)
  (let ([top 41])
    (let ([top 42])
      (let ([top 43])
        top))))

(f2)

(define-macro (make-macro-scoped-foo)
  #'(begin
      (define foo 42)
      (* foo 2)))

(make-macro-scoped-foo)
foo

(define-macro (bind-existing-identifier ID)
  #'(begin
      (define ID 42)
      (* ID 2)))

(bind-existing-identifier 'foo)
'foo

(let ()
  (bind-existing-identifier bar)
  bar)
