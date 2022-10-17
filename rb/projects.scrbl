#lang scribble/book

@(require plot/pict 
	  scribble/base
	  scribble-math/dollar
	  scribble/example
          "./code/refs.rkt")

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
Provide an overview of the idea behind agent based models in psychology. Give a demonstration by implementing in racket a simple version (includeing graphics) of the classic work of Schelling @~cite{neighborhood-agent-model}.
@section{Genetic Algorithms}
@section{Quantum Probability}
@section{Vector Symbolic Architectures}
@section{Linear Ballistic Accumulators}
@section{Fitzhugh-Nagamo Neuron Model}

@generate-bibliography[#:tag "ref:projects" #:sec-title "Project References"]
