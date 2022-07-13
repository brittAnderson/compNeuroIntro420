#lang scribble/book

@(require plot/pict 
	  scribble/base
	  scribble-math/dollar
	  scribble/example
          symalg)

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


@title{Models of Spiking Neurons and Differential Equations}
@section{An Introduction to Differential Equations}



Why are Differential Equations an important technique for computational modelling in psychology and neuroscience?

@centered{@bold{The Action Potential}}

@itemlist[@item{10 minutes to brush up on what an action potential is}
               @item{Then be able to draw one and explain,}
               @item{What are the axes?}
               @item{What ion causes the upward deflection?}
               @item{What causes the repolarization?}
               @item{Who discovered the action potential?}
               @item{Who won the Nobel Prize for characterizing the ionic events of the action potential experimentally and building a mathematical model?}]

Did you draw     @hyperlink["https://commons.wikimedia.org/w/index.php?curid=44114666"]{this}?

@margin-note{Why is the action potential relevant for a discussion of DE's in modelling?}

@;     The computational model of the action potential is a partial differential equation and action potentials, "spikes", are often taken to be the information processing unit of the nervous system.

@;     DE's are the way we capture dynamics, how things change over time. If you think some neural or cognitive process that you are interested in is changing over time, then you are interested in dynamics and the first approach you should think of for modelling it is a differential equation.

@bold{Notation}
Concise summaries of things that would take too long to write out in detail. Mathematical notation is just a technical emoji. You probably know the "math" they represent; you just don't know the abbreviation that is being used.

@; Write out in long hand (or type on your computer) what is meant by the
@; following:
@(use-mathjax)

@($$ "\\sum_{i=0}^n x_i^3")

@($$ "\\sum_{\\forall x \\in \\left\\{ 1 , 2 , 3 \\right \\}} x ~=~ 6")
@subsection{Multiple Ways to Say the Same Thing}

@($ "\\frac{dx}{dt}")

@($ "\\dot{x}")

@($ "x'")

@($ "f'(x)")


@section{Derivatives are Slopes}
@itemlist[@item{What is a slope?}
               @item{When in doubt return to definition.}
               @item{Deriving the definition of a derivative.}
               @item{What is the definition of a derivative?}]

Digression: Use your computer as a tool for exploration

@examples[#:eval plot-eval
          (begin
            (define xs (list 1 2 3 4 5))
            (define ys (list 2 4 6 8 10))
            (plot (lines (map vector xs ys))))]




@(plot (list (function (lambda (x) (expt x 3)) (- 3) 3)
             (function (lambda (x) (- (* 12 x) 16)) 1 3)))

@(plot (list (function (lambda (x) (expt x 3)) 1.5 2.5)
             (function (lambda (x) (- (* 12 x) 16)) 1.5 2.5)))



@margin-note{Derivatives are Instantaneous Slopes}

You pick two points that are "close enough" and you get an answer that
is "close enough." If your answer isn't "close enough" then you move
your points closer, until /in the limit/ there is an infinitesimal
distance between them.

@centered{@bold{Definition of the Derivative}@(linebreak)@($ "\\frac{df}{dx} = \\lim_{h \\to 0}\\frac{f(x + h) - f(x)}{(x + h) - x}")}


@section[#:tag "use-deriv-to-solve"]{Using Derivatives to Solve Problems With a Computer}

@subsection{What is the square root of 53?}

We want to know the value of @($ "x") that makes @($ "53 =x^2") true?

@margin-note*{Always use the computer for the busy work when you can. Your computer can solve many mathematical problems for you. For example, requiring @tt{symalg} we can programatically find that the derivative of @($ "x^2") is 
@($ (latex (simplify (differentiate (parse-infix "x^2")))))}

@itemlist[@item{Come up with a guess.}
               @item{Calculate the error.}
               @item{Adjust your guess based on the error.}
               @item{This adjustment will use the derivative.}]


@subsubsection{Working Through an Example}

Let's say we want to solve for @($ "x") when @($ "x^2 = 128"). How might we start? When in doubt, guess! 

How much is your guess off?

@($$ "\\mbox{Error} = \\mbox{(my guess)}^2 - \\mbox{128}")

What we want to do now is adjust our guess. Since we know how much our function changes its output for each adjustment in the input, @margin-note*{How do we know this? Our derivative is a @italic{rate of change}.} we can revise our guess based on this necessary adjustment. If we are still wrong, we just repeat the process. 

To get there let us consider representing the ratio of how our function's output changes for changes in input. We can just make things concrete. 

@($$ "\\frac{\\Delta~\\mbox{output}}{\\Delta~\\mbox{input}} = \\frac{\\mbox{function(input_1)} - \\mbox{function(input_0)}}{\\mbox{input_1} - \\mbox{input_0}}")

If you take a look at the definition of the derivative above you will see the resemblance, except for the absence of the limit. When trying to solve this problem we don't initially know both inputs, but we do know that when we put in the solution to our problem we will get 128. And we also know that we can computer the derivative. A bit of rearranging and renaming give us. 

@($$ "\\Delta(guess) = \\frac{\\mbox{f(guess)} - 128}{\\frac{\\mbox{df}}{\\mbox{dg}}|_{guess}}")

What is square root of 128?

@examples[(define (df g) (* 2.0 g))
          (define (update-guess g target)
            (/ (- target (expt g 2.0) ) (df g)))
          (define (my-sqrt [target 128.0] [guess 7.0] [tol 0.000001])
            (let* ([udg (update-guess guess target)]
                   [current-guess (+ guess udg)])
              (if (< udg tol)
                  current-guess
                  (my-sqrt target current-guess))))]


@examples[(require "my-square-root.rkt")
          (my-sqrt 55.0 4.0)]


@itemlist[@item{What is a @tt{cube root}?}
               @item{What is the derivative of @($ "x^3")?}
               @item{Write a Racket program to computer the cube root of a give number.}]

@subsection{Practice Simulating With DEs}

@subsubsection{Frictionless Springs}

We want to code neurons, but to get there we should feel comfortable with the underlying tool or we won't be able to adapt it or re-use it for some new purpose. I don't want to give you a fish. I want to teach you how to fish.

By working with an example simpler than a neuron, and one for which you might have more intuition, such as a simple spring or "slinky" I hope you will get a better /feel/ for how the numbers, equations, and code all relate. Then we can move on to the neuronal application.

The equation of a frictionless spring is:

@($$ "\\frac{d^2 s}{dt^2} = -P~s")
where 's' refers to space, 't' refers to time, and 'P' is a constant, often called the spring constant, that indicates how stiff or springy the spring is. 


Imagine that we knew this derivative. It would tell us how much space the spring head would move for a given, very small, increment of time. We could then just add this to our current position to get the new position and repeat. This method of using a derivative to iterate forward is sometimes called the Euler method.

Returning to our definition of the derivative:

@($$"\\frac{s(t + \\Delta t) - s(t)}{\\Delta t} = velocity \\approx \\frac{d s}{d t}")

But our spring equation is not given in terms of the velocity it is given in terms of the acceleration which is the second derivative. Therefore, to find our new position we need the velocity, but we only have the acceleration. However, if we knew the acceleration and the velocity we could use that to calculate the new velocity. Unfortunately we don't know the velocity, unless ... , maybe we could just assume something. Let's say it is zero because we have started our process where we have stretched the spring, and are holding it, just before letting it go. 

How will our velocity change with time?

@($$ "\\frac{v(t + \\Delta t) - v(t)}{\\Delta t} = acceleration \\approx \\frac{d v}{d t} = \\frac{d^2 s}{d t^2}")

 And we have a formula for this. We can now bootstrap our simulation.

Note the similiarity of the two functions. You could write a helper function that was generic to this pattern of old value + rate of change times the  time step, and just used the pertinent values. 

How do we know the formula for acceleration? We were given it \ref{eq:1}. 

@examples[#:eval plot-eval
          (require "spring.rkt")
          (begin
            (define spring-results (release-spring))
            (plot (lines (map vector (third spring-results) (fourth spring-results)))))]

                              NB: The code above creates a series of steps that happen one after the other. This is *not* a nested loop, though it will look similar to the nesting of loops you can see in python and R. This is just a common lisp idiosyncrasy. You will again need to use a loop. This time I am using what kind of loop? The ~collect~ statement is a short hand for gathering all my values up and returing them. You will probably need something like a =return= statement or a variable that you use to catch the updated values and append them on to a growning list. 

@; #+Name: putting it together: functions and plotting
@; #+begin_src lisp :exports both :results replace graphics file :file "./images/spring.png" 
@;   (defun spring-plot (output)
@;     (let ((data (mapcar (lambda (a) (cons (fourth a) (third a))) (release-spring 1000)))
@; 	  (point-type 7)
@; 	  (point-color "red")
@; 	  (step-size 1)
@; 	  (slope 0.4))
@;       (with-plots (*standard-output* :debug nil)
@; 	(gp-setup :output output :terminal '(:pngcairo) :title
@; 	      "Frictionless Spring" :xlabel "Time (sec.)"
@; 	      :ylabel "Location" :key '(box lt -1 lw 2 opaque))
@;   	(plot
@; 	 (lambda ()
@; 	   (loop for p in data
@; 		   do (format t "~&~a ~a" (car p) (cdr p))))
@; 	 :with `(:lines :lc :rgb ,point-color :title "Location")))))
@;   (spring-plot "./images/spring.png")
@; #+end_src

@; **** Do you understand the logic of the code? :class_exercise:
@; You may see examples of _pseudo-code_ in the future. This is when an algorithm is described as if it were a computer program. You are told the steps that need to happen and the order, but often it is not in any particular language, but just a general sort-of kind-of like code code. It is good to practice reading code like that so that you can more easily translate the procedures and steps into the language you like to use.

@; The "mapcar" you see is a lisp version of =map=. This is a common /functional programming/ construct that is very powerful and lets you write concise code that achieves loop like behavior in a simpler fashion. Less writing means less chances for bugs in your code, and more concise code makes it easier for people to read and understand your code. See if you can find the =map= equivalent for the language you are programming in and see if you can get it to work here. The usual procedure is to get something working first, and then try to amend it to use the new tool or technique. 

@; #+RESULTS: putting it together: functions and plotting
@; [[file:./images/spring.png]]



@; ** Damped Oscillators :homework:

@; Provide the code for the damped oscillator. It has the formula of

@; $$ \frac{d^2 s}{dt^2} = -P~s(t) - k~v(t) $$

@; This should really only require changing one line of your code for the
@; simple harmonic oscillator.

