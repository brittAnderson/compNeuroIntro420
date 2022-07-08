#lang scribble/book

@(require plot/pict
          scribble/manual
	  scribble/base
	  scribble-math/dollar
	  scribble/example
	  racket/math)

@(define plot-eval
  (let ([eval  (make-base-eval)])
    (eval '(begin
             (require racket/math
                      racket/match
                      racket/list
                      racket/draw
                      racket/class
                      plot/pict
                      plot/utils)))
    eval))


@title{Modeling Cognition}

@include-section{preamble.scrbl}
@include-section{nn.scrbl}
