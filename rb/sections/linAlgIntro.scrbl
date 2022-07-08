#lang scribble/book

@(require plot/pict
          scribble/manual
	  scribble/base
	  scribble-math/dollar
	  scribble/example
          scriblib/figure
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


@title[#:tag "linear-algebra-introduction"]{Linear Algebra}

@section{Introduction}
@subsection{Goals}
Our goal for the next few lessons is to come to understand 
@itemlist[
          @item{What is a neural network?}
@item{What mathematics are needed to build a neural network?}
@item{How can neural networks help us understand cognition?}]

As a first illustration of some of the key ideas we will execute a simple cellular automata rule. 
@subsection{Be the Neuron}

For this exercise you will need a sheet of graph paper and a @bold{rule}.

Think of this @italic{rule} as a @italic{mathematical function}: a matching of each possible input to one possible output. You are going to consider a particular grid cell of the graph papers as a metaphorical neuron. That "neuron" decides whether or not to "fire" (in this case firing is to be colored in black and not firing to be left white).  Your rule specifies what the output is for that neuron/grid-cell as a function of it's three inputs: the neighbor to the left, itself, and the neighbor to the right. We view the passage of time as flowing down, so you color in the neuron's state below it. 

@figure["automata-grid" "Automata as a 1D grid with neighbors. For this example the neuron is grid cell . You decide whether to color the grid cell below visualized as the neuron one moment in time later based on the value of the neighbor to your left, itself, and the neigbhbor to the right. Implement your assigned rule on the piece of graph paper to see what pattern emerges."]{@image{/home/britt/gitRepos/compNeuroIntro420/notebooks/linAlg/grid.png}}

** What are the lessons learned?
1. Repetitive actions are hard. We (humans) make mistakes following even simple rules for a large number of repeated steps. Better to let the computer do it since that is where its strengths lie.
2. Complex global patterns can emerge from local actions. Each neuron is only responding to its immediate right and left yet global structures emerge.
3. These characteristics seem similar to brain activity. Each neuron in the brain is just one of many. Whether a neuron spikes or not is a consequence of its own state and its inputs (like the neighbors in the grid example).
4. From each neuron making a local computation, global patterns of complex activity can emerge.
5. Maybe by programming something similar to this system we can get insights into brain activity.

** Programmatic implementation
To demonstrate how one might take the ideas that we practiced when implementing a cellular automata phycially programmatically, I have coded some of the steps for a visualization in common lisp and show the code here. Because things don't always run so cleanly when breaking the code into snippets inside an orgmode file and using emacs, if you decide to adapt or test this code you should use the [[file:cellular-automata.lisp]].
