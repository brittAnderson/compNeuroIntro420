#lang scribble/book

@(require plot/pict 
	  scribble/base
	  scribble-math/dollar
	  scribble/example
          scribble/manual
          symalg
          scriblib/figure
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

@(define (my-exp x) (exp (/ 1.0 x)))

@(use-mathjax)

@(define-cite ~cite citet-id generate-bibliography #:style author+date-style)

@title[#:tag "forgetting"]{What Is Cognition?}

The technical challenges for modeling in psychology are mastering the mathematics and programming necessary to formally express an idea and to implement a simulation to examine the consequences. The conceptual challenge is to decide what it is you are modeling. @margin-note*{Is a mathematical formula that reproduces an observed behavioral pattern a model?} In the late 1800s Ebbinghaus, using creative, and for their time, @hyperlink["https://youtu.be/TGGr5Uc8_Bw"]{innovative, Herculean methodologies}, demonstrated that the proportion of items retained in memory declines predictably over time. The form of the forgetting curve is exponential. 

@figure[
  "fig:forgetting-curve"
  @elem{@hyperlink["https://en.wikipedia.org/wiki/Forgetting_curve"]{Forgetting Curve - Wikipedia}}
  @elem{@image[#:scale 0.75]{./images/Forgetting_curve_decline.svg}}]

@figure[
        "fig:exponential-curve"
        @elem{Plot of @($ "\\exp ( \\frac{1}{x} )")}
        @elem{@(plot-eval (plot (function my-exp 1 4)))}]

@section{Cognition is ...}

As revealed in this recent article@~cite[what-is-cognition] there are a wide array of views on what is cognition. @margin-note{Which of these opinions, if any, do you agree with and why?}

@generate-bibliography[#:sec-title "Cognition Section Bibliography"]
