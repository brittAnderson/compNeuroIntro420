#lang scribble/book

@(require plot/pict 
	  scribble/base
	  scribble-math/dollar
	  scribble/example)

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


@title{Topics for Final Projects}
@section{Agent Based Modeling}
@section{Genetic Algorithms}
@section{Quantum Probability}
@section{Vector Symbolic Architectures}

