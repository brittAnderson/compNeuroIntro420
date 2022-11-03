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
          "./../code/bool-figs.rkt")


@title{The Math That Underlies Neural Networks?}
@section{Linear Algebra}

The math at the heart of neural networks and their computer implementation is @italic{@bold{linear algebra}}. For us, the section of linear algebra we are going to need is mostly limited to vectors, matrices and how to add and multiply them.

@subsection{Important Objects and Operations}
@itemlist[#:style 'ordered
          @item{Vectors}
          @item{Matrices}
          @item{Scalars}
          @item{Addition}
          @item{Multiplication (scalar and matrix)}
          @item{Transposition}
          @item{Inverse}]

@subsubsection{Adding Matrices}                                          
To gain some hands on familiarity with the manipulation of matrices and vectors we will try to do some hand and programming exercises for some of the fundamental operations of addition and multiplication. We will also thereby learn that some of the rules we learned for numbers (such as a * b = b * a) do not always apply in other mathematical realms.

There are in fact many ways to think about what a vector is. 

It can be thought of as a column (or row of numbers).
More abstractly it is an object (arrow) with magnitude and direction.
Most abstractly it is anything that obeys the requirements of a vector space. 

For particular circumstances one or another of the different definitions may serve our purposes better. In application to neural networks we often just use the first definition, a column of numbers, but the second can be more helpful for developing our geometric intuitions about what various learning rules are doing and how they do it.  

Similarly, we often just consider a matrix as a collection of vectors or as a rectangular (2-D) collection of numbers.

@subsubsection{Activity}
Look up how racket handles @hyperlink["https://docs.racket-lang.org/math/matrices.html"]{matrices and vectors}. Here is a very simple @hyperlink["./../code/la-demo.rkt"]{file} to try and get started. 

@bold{Important}: vectors are a special datatype in Racket, and the vector type is probably not what you want to be using. Look for matrices and linear algebra.

Make two arrays and make them the same size@margin-note*{What is the @italic{size} of a matrix?}.

Add them together in both orders (A + B and B + A). How does one add an array that itself has numerous different numbers?

Then do the same for multiplication. Note that there are particular requirements for the sizes of matrices in order that it is possible to multiply them in both directions. What is that rule?

What is the name for the property of having A*B = B*A?


@subsection{Common Notational Conventions for Vectors and Matrices}

Vectors tend to be notated as @italic{lower case} letters, often in bold, such
as @($ "\\mathbf{a}"). They are also occasionally represented with little
arrows on top such as @($ "\\overrightarrow{\\textbf{a}}").

Matrices tend to be notated as @italic{upper case} letters, typically in bold,
such as @($ "\\mathbf{M}").

Good things to know: what is an @italic{inner product}? How do you compute it in racket?

@section{What is a Neural Network?}

What is a Neural Network? It is a brain inspired computational approach
in which "neurons" compute functions of their inputs and pass on a
@italic{weighted} proportion to the next neuron in the chain.

@figure["fig-nn" @; figure tag
        @elem{simple schematic of the basics of a neural network. This is an image for a single neuron. The input has three elements and each of these connects to the same neuron ("node 1"). The activity at those nodes is filtered by the weights, which are specific for each of the inputs. These three processed inputs are combined to generate the output from this neuron. For multiple layers this output becomes an input for the next neuron along the chain.}
        @elem{@image{./images/nn.png}}]


@subsection{Non-linearities}
The spiking of a biological neuron is non-linear. You saw this in both the integrate and fire and Hodgkin and Huxley models you programmed. The lines on those plots you created are not, for the most part, straight. Perhaps the simplest way to incorporate a non-linearity into our artificial neuron is to give it a threshold, like we did for the integrate and fire model. When activity exceeds the threshold (which we will usually designate with a capital Greek Theta @($ "\\Theta") then the neuron is set to 1 and if it is not firing it is set to 0 (like the "w" → 0; "b" → 1 mapping we used for the cellular automata).
   
@($$ "\\begin{equation}
\\mbox{if } I_1 \\times w_{1,1} + I_2 \\times w_{2,1} + I_3 \\times w_{3,1} > \\Theta \\mbox{ then } Output = 1
\\end{equation}")

What this equation shows is that Inputs (the @($"I")s) are passed to a neuron. Those inputs have something like a synapse. That is designated by the w's. Those weights are how tightly the input and internal activity of our artificial neuron is coupled. The reason for all the subscripts is to try and help you see the similarity between this equation and the inner product and matrix multiplication rules you just worked on programming. The activity of the neuron is a sort of internal state, and then, based on the comparison of that activity to the threshold, you can envision the neuron spiking or not, meaning it has value 1 or 0. Mathematically, the weighted sum is fed into a threshold function that compares the value to a threshold @($"\\Theta"), and passes on the value 1 if it is greater than the threshold and 0 (sometimes @($"-1") rather than zero is chosen for the inactive state because there are certain computational conveniences in doing so).

To prepare you for the next steps in writing a simple percetron (the earliest form of artificial neural network), you should try to answer the followign questons. 

Questions:
@itemlist[#:style 'ordered
          @item{What, geometrically speaking, is a plane?}
          @item{What is a hyperplane?}
          @item{What is linearly separability and how does that relate to planes and
   hyperplanes?}]

One of our first efforts will be to code a @italic{perceptron} to solve the XOR problem. In order for this to happen you need to know a bit about @italic{Boolean} functions and what an XOR problem actually is. 

@bold{Examples of Boolean Functions and How They Map onto our Neural Network Intuitions}

The "AND" Operation/Function

@figure["fig:and"
        @elem{The @italic{and} operation is true when both its inputs are true.}
        @(and-fig)]

        
@figure["fig:or"
        @elem{The @italic{or} operation is true if either or both of its inputs are true.}
        @(or-fig)]

@figure["fig:xor"
        @elem{The @italic{xor} is true when one or the other, but not both of the inputs are true. It is exclusively an or function.}
        @(xor-fig)]

This short @hyperlink["https://media.nature.com/m685/nature-assets/nbt/journal/v26/n2/images/nbt1386-F1.gif"]{article} provides a nice example of linear separability and some basics of what a neural network is. 

@subsubsection{Exercise XOR}

Using only @italic{not}, @italic{and}, and @italic{or} operations draw the diagram that allows you to compute in two steps the @italic{xor} operation. You will need this to code it up as a perceptron.

@subsection{Connections}
  Can neural networks encode logic? Is the processing zeros and ones enough to capture the richness of human intellectual activity?

There is a long tradition of representing human thought as the consequence of some sort of calculation of two values (true or false). If you have two values you can swap out 1's and 0's for the true and false in your calculation. They even seem to obey similar laws. If you the conjunction (AND) of two true things it is only true when both are true. If you take T = 1, then T ∧ T is the same as @($"1~\\times~1").

We will next build up a simple threshold neural unit and try to calculate some of these truth functions with our neuron. We will build simple neurons for truth tables (like those that follow), and string them together into an argument. Then we can feed values of T and F into our network and let it calculate the XOR problem.


@subsection{Boolean Logic}
George Boole, Author of the @italic{Laws of Thought}

@itemlist[@item{Read the @hyperlink["https://archive.org/details/investigationofl00boolrich"]{book} on Archive.org}
               @item{Read about @hyperlink["https://plato.stanford.edu/entries/boole/#LifWor"]{George Boole}}]

@subsection{ First Order Logic - Truth Tables}
@bold{Or}

@tabular[#:sep @hspace[1]
         (list (list @bold{Pr A} @bold{Pr B} @bold{Or})
               (list "0" "0" "0")
               (list "0" "1" "1")
               (list "1" "0" "1")
               (list "1" "1" "1"))]

@bold{And}

@tabular[#:sep @hspace[1]
         (list (list @bold{Pr A} @bold{Pr B} @bold{Or})
               (list "0" "0" "0")
               (list "0" "1" "0")
               (list "1" "0" "0")
               (list "1" "1" "1"))]

@bold{Nand}

@tabular[#:sep @hspace[1]
         (list (list @bold{Pr A} @bold{Pr B} @bold{Or})
               (list "0" "0" "1")
               (list "0" "1" "1")
               (list "1" "0" "1")
               (list "1" "1" "0"))]
