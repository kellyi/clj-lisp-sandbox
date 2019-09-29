#lang scribble/manual
@(require (for-label json))

@title{jsonic: toy language from Beautiful Racket}
@author{Hello World}

@defmodulelang[jsonic]

@section{Introduction}

Domain Specific Language for writing Racket expressions in JSON.

Relies on the @racketmodname[json] library -- specifically, the @racket[jsexpr->string] function.

If we start with this:

@verbatim|{
#lang jsonic
[
  @$ 'null $@,
  @$ (* 6 7) $@,
  @$ (= 2 (+ 1 1)) $@
]
}|

We'll end up with this:

@verbatim{
[
  null,
  42,
  true
]
}
