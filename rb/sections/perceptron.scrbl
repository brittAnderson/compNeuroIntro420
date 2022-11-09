#lang scribble/book

@(require scribble/base
	  scribble-math/dollar
	  scribble/example
          scribble/manual
          symalg
          scriblib/figure
          scribble/core
          scribble/html-properties
          scriblib/autobib
          "./../code/refs.rkt")



@(define percep-eval
   (let ([eval (make-base-eval)])
     (eval '(begin
              (require math/matrix
                       math/array)))
     eval))

@(define percep-plot-eval
   (let ([eval (make-base-eval)])
     (eval '(begin
              (require math/matrix
                       math/array
                       "./code/perceptron-rule.rkt")))
     eval))

@(define ca-eval
   (let ([eval (make-base-eval)])
     (eval '(begin (require "./code/ca.rkt")))
     eval))
              

@(define-cite ~cite citet-id generate-bibliography #:style author+date-style)


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

@($ "I = \\sum_{i=1}^{n} w_i~x_i")

If @($ "I \\ge T") then @($ "y = +1") else if @($ "I < T") then @($ "y = -1")

If the answer was correct, then @($ "\\beta = +1"), else if the
answer was incorrect then @($ "\\beta = -1").

The "T" in the above equation refers to the threshold. This is a user defined value that is conveniently, and often made, to be zero. 

Updating is done by @($ "\\mathbf{w_{new}} =
\\mathbf{w_{old}} + \\beta y \\mathbf{x}")

@section{You Are The Perceptron}

This is a pencil and paper exercise. Before coding it is often a good idea to try and work the basics out by hand. This may be a flow chart or a simple hand worked example. This both gives you a simple test case to compare your code against, but more importantly makes sure that you understand what you are trying to code. Let's make sure you understand how to compute the perceptron learning rule, but doing a simple case by hand.

Beginning with an input of @($ "\\begin{bmatrix}0.3 \\\\ 0.7 \\end{bmatrix}"), an initial set of weights of @($ "\\begin{bmatrix}-0.6 \\\\ 0.8 \\end{bmatrix}"), and a @bold{class} of 1. Compute the value of the new weight vector with pen and paper.

@subsection{A simple data set}

For these data there are two dimensions or features (the first and second columns) and the third colum represents their @italic{class}. 

@(racketblock (matrix [[ 0.3 0.7 1.0]
                      [-0.5 0.3 -1.0]
                      [0.7 0.3 1.0]
                      [-0.2 -0.8 -1.0]]))

Using the starting weight above write code to iteratively compute a new weight from each input and it's class and using the current weight. If you can, save each updated weight so you can see how they change, but if you can't still try to use a for construct to iterate through these data and see how the weights change.

In broad outlines you will need to decide on a data structure. You can use a matrix as I have here, but it may be easier to just use a list to start. For example @(racket (list (list 0.3 0.7) 1.0)). The first element of the list would be the input data and the last item the desired class. You could create a list of list of such elements to capture the matrix I have displayed above. 

This progressive updating of the weight vector is the @italic{learning}. Note that sometimes our initial weight vector classifies incorrectly. How does it do after one complete cycle through all the training examples?

@examples[#:label "Checking our Learned Weight For One Input"
         #:eval percep-eval
         (let ([in-class -1.0])
           (if (= (if (>= (matrix-ref
                           (matrix* (row-matrix [-0.6 0.3])
                                    (col-matrix [1.2 2.3])) 0 0) 0.0)
                      1 -1) in-class) "correct" "incorrect"))]

@subsection{What does it all mean? How is the Perceptron Learning?}

@examples[#:label "Changing Weights as Vectors"
          #:eval percep-plot-eval
          #:no-prompt
          (wt-plot (one-loop-through-data my-data (col-matrix [-0.6 0.8])))]

These functions and the @tt{my-data} are in the file @hyperlink["./../code/perceptron-rule.rkt"]{perceptron-rule.rkt}. Each time through the perceptron rule I compute the new weights and use the first position as the 'x' value and the second position as the 'y' value to plot vectors on an 'x-y' plane. You can imagine that as we iterate through the data we are rotating the vectors around an origin. The decision plane is perpendicular to the vectors and anchored at the bottom of the arrows. If you compare this to the location of the data points (which you can add to the plot by editing the functions in the linked file) you will see that the rule is learning to find the decision plane that puts all of one class on one side of the line and all of the other class on the other side. That is why it is limited to problems that are linearly separable!

@subsection{Bias}

These data were selected such that the base of the vector could separate them while anchored at zero. However, for many data sets you not only need to learn what direction to point the vector, but you also need to learn where to anchor the vector. This is done by including a @bold{bias weight}. Add an extra dimension to your weight vector and your inputs. For the inputs it will just be a constant value of 1.0, but this extra, bias weight, will also be learned and allows you to achieve, effectively, a translation away from the origin to be able to separate points that are more heterogeneously scattered. 

@subsubsection{Geometrical Thinking}
@itemlist[@item{What is the relation between the inner product of two vectors and the cosine of the angle between them?}
@item{What is the *sign* for the cosine of angles less than 90 degrees and those greater than 90 degrees?}
@item{ How do these facts help us to answer the question above?}
@item{ Why does this reinforce the advice to think /geometrically/ when thinking about networks and weight vectors?}]

@section{The Delta Rule - Homework}

The @bold{@italic{Delta Rule}} is another simple learning rule that is a minimal variation on the perceptron rule. It is used more frequently, and it has the spirit of Hebbian learning, which we will learn more about soon. The homework asks you to write code to test and train an artificial neuron using the delta learning rule. 

@itemlist[#:style 'ordered
          @item{For an easy start create some pseudo random linearly separable points on a a sheet of paper. Label one population as @bold{1} and the other population as @bold{-1}.}
          @item{For a more challenging set-up create the data programatically using random numbers and some method that allows you to vary how close or distant the points are to the line of separation, and how many points there are to train on.}
          @item{The Delta Learning rule is: @($$ "\\Delta~w_i = x_i~\\eta(desired - observed)")}
          @item{Submit your code that has your test data in it. Start with an initial random weight and use the delta rule to learn the correct weighting to solve all your training examples. Then test on a new set of points that you did @bold{not} test on but that are classified according to the same rule. Your code should assess how well the @italic{trained} rule classifies the @italic{test} data.}]

I have @hyperlink["./../code/perceptron-rule.rkt"]{some code} for the perceptron that might give you some code you could adapt if you have trouble getting started. 

@generate-bibliography[#:sec-title "Perceptron Bibliography"
                       #:tag "ref:perceptron"]
