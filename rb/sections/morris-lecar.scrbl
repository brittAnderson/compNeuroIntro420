#lang scribble/book

@(require plot/pict 
	  scribble/base
	  scribble-math/dollar
	  scribble/example
          scribble/manual
          symalg
          scriblib/figure
          scribble/core
          scribble/html-properties
          "./../code/refs.rkt")

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

@title{Morris Lecar Model}

@section{Work in Progress}
This section is a work in progress.

I want to discuss the benefits of simplifying models as well as show the use of direction plots and phase space diagrams.

At the moment I have working racket code, but I have not written up this section yet. I am adding this section now (Oct 2022) to have a way to make the @hyperlink["./../code/morris-lecar.rkt"]{code} discoverable. 


