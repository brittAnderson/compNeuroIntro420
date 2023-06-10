#lang scribble/book

@(require pict
          plot/pict 
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
             (require pict
                      racket/math
                      racket/match
                      racket/list
                      racket/draw
                      racket/class
                      plot/pict
                      plot/utils)))
    eval))


@(define-cite ~cite citet-id generate-bibliography #:style author+date-style)

@title{A Digression Into Dynamics}

@section{Introduction}

In the material on differential equations and their use in spiking neuron models we have been relying on the use of a differential equation that specifies the evolution of voltage (or some important parameter) as a function of time. So far, we have focused on using such models to create a simulation of the graph of an action potential, but those functions of time are statements about @bold{dynamics}. And the study of neural dynamics in its own right can give us important insights into brain and neuronal activity normally and as a consequence of disease. An interest in dynamical systems has been a part of computational neuroscience since the days of Hodgkin and Huxley, but it has become much more popular now. With the development of cheaper and more powerful computing capacities it is not feasible to simulate more complex models and models in higher dimensions. The latter is apt as there has been a contemporaneous development in multiple electrode recordings that yield high dimensional data. We can study the dynamics of one neuron, but we can also study the dynamics of a population of neurons. We can look at the time evolution of a vector of voltages that move across a high dimensional space.

In order to take advantage of our recent experiences with the nature and use of simple differential equations for producing simple, single neuron simulations of the integrate and fire variety I will introduce some of the basic terminology and ideas at play in neuronal dynamics. An excellent and concise summary of these ideas, one that I drew upon heavily for this treatment, is a set of notes by @~cite[dynamics-tutorial]. 

@section{Beginning to Think Dynamically}

Assume you have a derivative that is equal to @$["f(x) = x - x^3"].@margin-note{This is a derivative by declaration, but not by the usual notation.}

@examples[
          (define (dx/dt=x-x^3 x y)
  (vector (- x (expt x 3.0)) 0))]

What you want is some function of the variable @italic{x} that will equal what you get when you take its derivative. For this function you could find the analytical solution by first separating the variables (@italic{x}'s on one side and @italic{t}'s on the other) and then using partial fractions before integrating. Should you do this by hand (or using a computer algebra system like @hyperlink["https://www.wolframalpha.com/input?i=integral+of+1%2Fx"]{Wolfram Alpha}) you will find a complicated formula where it is not easy to intuit how the value of the function changes as you evolve @italic{t}.

The dynamical systems approach is the same idea as we used to implement our spiking neuron models. We think of  @$["x"] as itself a function: @$["$x(t)"]. Then as @italic{t} changes we will also change $x$.

@section{Fixed Points}

@quote{Give me a place to stand, and I shall move the world. ---Archimedes}

Fixed points are the points in a function where it no longer changes. It becomes @italic{fixed}. We have already used fixed points, at least informally, to determine certain starting parameters for our Hodgkin & Huxley model. We assumed that the system evolved to some point in time where the derivative of a function with time was zero. Then we could infer or compute the form that our parameter (e.g. the ɑ or β) took. This is the determinant of a fixed point: a point where the derivative is zero. If the derivative is describing how your function changes over time, then when the derivative is zero the function does not change and so that point is fixed.

You can learn a lot about a system by looking at its @bold{fixed points}.

@subsection{Class Exercise}

For the function above what are the fixed points and are they @bold{stable} or @bold{unstable}? Solve your equation for all the values of @$["x"] that make the derivative zero. That will give you the fixed points. What do you think is meant by the term @italic{stability}?


@;Here they are @$["\{0, 1, -1\}"].

Informally, you can assess stability by looking nearby the fixed points to see if they are pushed toward or away from the fixed point. Which of these three are stable or unstable?

@examples[#:no-prompt
          #:label "Stability From Vector Fields:"
#:eval plot-eval
          (begin
            (define (dx/dt=x-x^3 x y) (vector (- x (expt x 3.0)) 0))
            (plot (vector-field dx/dt=x-x^3   -0.2 0.2 -0.2 0.2)))]

@examples[#:no-prompt
          #:label "A more realistic 2D example:"
          #:eval plot-eval
          (plot (vector-field (λ (x y) (vector (+ x y) (- x y))) -2 2 -2 2))]

@subsection{Extended Class Activity}

Another example taken from @~cite[dynamics-tutorial] is @$$["x' = \\lambda + x^2."] We can use this equation to demonstrate the idea of a @bold{@italic{bifurcation}}.

A bifurcation is the description of a point in the plot of a function where some aspect of function behavior changes in a way we regard as important. The trick here is to change one's perspective. We have been considering our functions as functions of time, which they are, but in the case of this function we also have a parameter expressed by the λ. This too is free to change, just like our time variable. Consider your integrate and fire neuron. For some injections of currents nothing happens. It just reaches and holds a steady value. But as we increase the level we reach a point where we get repetitive firing. It /oscillates/. Or recall how the behavior of your model changed when you adjusted the membrane time constant: τ. You could now think about how the entire @$["v(t)"] function changes as you change τ. If there was some abrupt shift in function behavior you would have a bifurcation. To make this concrete consider the fixed points and stability of this equation. 

@italic{What are the fixed points of this equation?}

@italic{Are they stable?}

If you have had some calculus you may remember that you could find the maxima or minima of a function by the location of the points where the derivative was zero. You were either on top of a mountain or in the depths of a valley.

@examples[#:no-prompt
          #:eval plot-eval
          (hc-append
           (plot-pict (function sqr -2 2))
           (plot-pict (function (lambda (x) (- (sqr x) ))-2 2)))]

Imagine that these two plots are plots of the derivative. To see how the derivative is changing, you can visually take the derivative of this derivative by imagining the tangent lines, that is the approximations to the slope that we used before. Trace these imaginary lines around the graph from left to right and observe that in one case they go from minus to positive and in the other from positive to minus. Which is which? 

More precisely you can calculate the derivative of your derivative and find its value at the fixed point. If it is greater than zero you are unstable. Less than zero and you are stable. If it exactly equals zero the behavior is not clear.

For this classroom activity find the fixed points for this equation as a function of lambda. Note the values for lambda might equal 0 or lambda could be greater or less than zero. Often it is more helpful to consider regions of values than just some odd assortment of numbers you pull from a hat. Consider your fixed points as functions of lambda. 

Plot the values of x for all its fixed points as a function of lambda. What does  the look like. What goes on the x axis? Y axis?







@examples[#:no-prompt
          #:eval plot-eval
          (begin 
            (define (hopf-fixed-example l) (list (list l (- (sqrt (* -1 l)))) (list l (sqrt (* -1 l)))))
            (define (hopf-fixed-example-deriv x) (* 2 x))
            (define ls-to-plot (range -5 -0.05 0.05))
            (define fixed-points (append* (map hopf-fixed-example ls-to-plot)))
            (define-values (r g) (partition (lambda (x) (positive? (hopf-fixed-example-deriv (second x)))) fixed-points))
            (plot (list
                   (point-label (vector 0 0) "bifurcation point" #:anchor 'right)(points r #:color "red" #:label "unstable")
                   (points g #:color "green" #:label "stable")) #:x-label "lambda" #:y-label "fixed-points"))]

This is a so-called @bold{@italic{saddle node}}.

Now try to do the same for the function: @$$["x' = \\lambda~x - x^2."]
Find the fixed points for the @bold{@italic{transcritical}} bifurcation and plot the stability. It is probably easier if just use pen and paper (or ipad). Use your calculus to find the fixed points and the derivatives for stability.

@margin-note{It is important to recognize that the λ here is not the same as for the λ calculus. There are only so many symbols and the mathematicians tend to recycle and re-use.}

Then are many more types of bifurcations that are classified based on these types of graphs.
Some, such as the "Hopf" can only be observed in higher dimensions where the ideas of a @bold{@italic{phase}} space and plot become common terms. 

IAMHERE

The extrapolation we want to make from the 1D case is that this geometry of a "particle" moving in a space is sufficient or all ordinary (no partial derivatives)  differential equations.

For the two dimensional examples you need two variables (say x and y) and two functions of those two variables. We need to consider the derivatives of each. How x changes (x') is a function of both x and y. Same for how y changes (y'). For this two dimensional case the phase *plane* is the x - y plane.

Think of your "particle". At the initial time (t_0) it is somewhere (x(t_0), y(t_0)). As t grows the position of the particle will move in the x - y plane. How do we know where it goes next? We have the derivatives that describe how it changes over time.

One approach:
1. Determine the fixed points.
2. Draw the /nullclines/.
   What is a nullcline? It is the line for when one of the derivatives is zero. For example if you had a system of equations like:

   \begin{align*}
   x' &= y - x^2 + x \\
   y' &= x - y
   \end{align*}

   Then you would consider when $x'$ is zero. That would mean when $y = x^2 - x$. What is the y nullcline for this system of equations? Fixed points are where both derivatives are zero. How would that show up in such a graph as this?
   
Locally everything is linear. To find out the stability of complex systems where the graphical depiction doesn't give you all the answers you take your non-linear system and linearize in the neighborhood of the fixed points.

Terms: before we used the  first and second derivatives. Same here, but things get more complicated with more equations and more terms. To "linearize" you can use Taylor's expansion. And find the eigenvalues of the Jacobian. We will not be explaining this further.

A reduced model of the Hodgkin-Huxley is the Morris Lecar. Potassium/Calcium and a leak current. And an applied constant current. 



@examples[#:eval plot-eval
          (require "./code/morris-lecar.rkt")
          ;;(require plot)@; needs commenting?
          (list (try-a-new-phi 0.03)
                (poor-mans-phase-plot 0.03))]


@generate-bibliography[#:sec-title "Dynamics Bibliography"
                       #:tag "ref:dyn"]
