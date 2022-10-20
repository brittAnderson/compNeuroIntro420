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

@title{Perceptrons}
@section{Goals}
The goal for this file is to share the idea of a perceptron, the mathematical formula for updating one, and iniate the process of coding a simple implementation that we will adapt to the delta rule.
@section{Perceptron History and Implementation}
The perceptron was the invention of a psychologist, @hyperlink["http://dspace.library.cornell.edu/bitstream/1813/18965/2/Rosenblatt_Frank_1971.pdf"]{Frank Rosenblatt}.  He was not a computer scientist. Though he obviously had a bit of the mathematician in him.

@figure[
        "fig:markI"
        @elem{The Perceptron Mark I}
        @elem{@image[]{./images/Mark_I_perceptron.jpeg}}]

Details to be found on the @hyperlink["https://en.wikipedia.org/wiki/Perceptron"]{wikipedia page}.

Those interested in some interesting background reading could consult his over 600 page book entitled @hyperlink["https://babel.hathitrust.org/cgi/pt?id=mdp.39015039846566&view=1up&seq=9"]{Principles of Neurodynamics} or this @hyperlink["https://link.springer.com/book/10.1007/978-3-642-70911-1"]{historical review}.

From the foreward of that book we have the following quote:

"For this writer, the perceptron program is not primarily concerned with the invention of devices for "artificial intelligence", but rather with investigating the physical structures and neurodynamic principles which under lie "natural intelligence". A perceptron is first and fore most a brain model, not an invention for pattern recognition. As a brain model, its utility is in enabling us to determine the physical conditions for the emergence of various psychological properties."

@section{The Perceptron Rules}
The perceptron rules are the equations that characterize what a perceptron is, and what it does in contact with experience, so that it can learn and revise its behavior. A lot can be done with these simple equations.

@($ "I = i\\sum_{i=1}^{n} w_i~x_i")

If @($ "I \\ge T") then @($ "y = +1") else if @($ "I < T") then @($ "y = -1")

If the answer was correct, then @($ "\\beta = +1"), else if the
answer was incorrect then @($ "\\beta = -1").

Updating is done by @($ "\\mathbf{w_{new}} =
\\mathbf{w_{old}} + \\beta y \\mathbf{x}")

@section{You Are The Perceptron}

This is a pencil and paper exercise. Before coding it is often a good idea to try and work the basics out by hand. This may be a flow chart or a simple hand worked example. This both gives you a simple test case to compare your code against, but more importantly makes sure that you understand what you are trying to code. Let's make sure you understand how to compute the perceptron learning rule, but doing a simple case by hand.

Beginning with an input of @($ "\\begin{bmatrix}0.3 \\\\ 0.7 \\end{bmatrix}"), an initial set of weights of @($ "\\begin{bmatrix}-0.6 \\\\ 0.8 \\end{bmatrix}"), and a @bold{class} of 1. Compute the value of the new weight vector with pen and paper.
                                                                                                              

@generate-bibliography[#:sec-title "Perceptron Bibliography"
                       #:tag "ref:perceptron"]
