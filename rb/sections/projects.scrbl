#lang scribble/book

@(require plot/pict 
	  scribble/base
	  scribble-math/dollar
	  scribble/example
          scriblib/autobib
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


@title[#:tag "Projects"]{Topics for Final Projects}
@section{Agent Based Modeling}
Provide an overview of the idea behind agent based models in psychology. Give a demonstration by implementing in racket a simple version (includeing graphics) of the classic work of Schelling @~cite[neighborhood-agent-model].
@section{Genetic Algorithms}
@section{Quantum Probability}
@section{Vector Symbolic Architectures}
Provide an overview of vector symbolic architectures and provide a short racket implementation of Kanerva's "what is Mexico's dollar?" example @~cite[vsa-dollar].
@section{Linear Ballistic Accumulators}
@section{Fitzhugh-Nagamo Neuron Model}



@generate-bibliography[#:sec-title "Project References"
                       #:tag "ref:projects"]
