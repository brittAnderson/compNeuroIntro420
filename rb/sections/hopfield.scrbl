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
          "./../code/hopfield-draw.rkt"
          "./../code/refs.rkt")
              
@(define-cite ~cite citet-id generate-bibliography #:style author+date-style)

@title{Hopfield Networks}

@section{Not all Networks are the Same}
@itemlist[@item{Feedforward}
                @item{Recurrent}
                @item{Convolutional}
                @item{Multilevel}
                @item{Supervised}
                @item{Unsupervised}]

The Hopfield network@~cite[hopfield-orig] has taught many lessons, both practical and conceptual. Hopfield showed physicists a new realm for their skills and added recurrent (i.e. feedback) connections to network design (output becomes input). He changed the focus from network architecture to that of a dynamical system. Hopfield showed that the network could remember and it could do some error correction, it could reconstruct the "right" answer from faulty input.

@figure["fig:hopfield-net"
        @elem{Hopfield Recurrent Connections}
        @elem{@(hopfield-net)}]

@subsection{How does a network like this work?}
@itemlist[@item{Each node has a value.}
               @item{Each of those arrowheads has an associated weight.}
               @item{The line with the "x" indicates that there are no self connections.}
               @item{All other connections for all other units are present and go in both directions.}]

@subsection{Test your understanding:}
@itemlist[#:style 'ordered @item{Tell me what the input for a network like this with four nodes should look like it terms of the linear algebra constructs we have talked about.}
          @item{A weight is a number associated to each connection. Tell me what the weights should look like in terms of the linear algebra constructs.}
          @item{How might we conceive of "running" the network for one cycle in terms
   of the above.}]

@subsection{A Worked Example}
Inputs can be thought of as vectors. Although I have drawn the network like a square that shape is really independent of the structure of data flow. Each node needs an input and each node will need a weighted contact to all the other nodes. Consider the following two input patterns and the following weight matrix.  
  @($$ "A = \\{1,0,1,0\\}^T")

  @($$ "B = \\{0,1,0,1\\}^T")

@($$ "weights =  \\begin{bmatrix}
  0 & -3 & 3 & -3\\\\
  -3 & 0 & -3 & 3\\\\
  3 & -3 & 0 & -3\\\\
  -3 & 3 & -3 & 0\\\\
  \\end{bmatrix}")

@margin-note{Ask yourself, how do I compute the output? Which comes first: the matrix or the input vector and why?}

Hopfield networks use a threshold rule. This non-linearity is, at least metaphorically, like the threshold that says whether a neuron in the brain or in our integrate and fire model fires. For the Hopfield network our threshold rule says:

@($$ "output(t)=\\{\\begin{array}{c} 1\\; \\mbox{if } t \\geq \\Theta\\\\ 0\\; \\mbox{if } t < \\Theta \\end{array}")

@($ "\\Theta") will represent the value of our threshold and for now let's set @($ "\\Theta = 0").

To make sure you understand the mechanics of this type of network you should first calculate the output to each of the two input patterns. 

Then, to test your intuition, you should guess what output you would get for an input of @($ "\\{1,0,0,0\\}^T"). Calculate it.

To understand why this is the case, ask yourself whether A or B is @bold{closer} to this test input? This will hopefully lead you to reflect on what it means, in this context, for one vector to be "closer" to another. 

@subsubsection{Distance Metrics}
Metrics relate to measurement. For some operation to be a distance metric it should meet three intuitive requirements and one that is maybe not as obvious. To measure the distance between two things we need an operation that is binary. That is, it takes two inputs. In this case that would be our two vectors. It's result should always be @bold{Non-negative}. A negative distance would clearly be meaningless. Our output should be @bold{symmetric}. Meaning that @($"d(A,B)~d(B,A)"). The distance from Waterloo to Toronto ought to come out as the same as going from Toronto to Waterloo. Our metric should be @bold{reflexive}. The distance from anything to itself ought to be zero. Lastly, to be a distance metric, our operation must obey the @hyperlink["https://en.wikipedia.org/wiki/Triangle_inequality"]{@bold{triangle inequality}}

Now, to understand what the network did, consider your distance measure to be the number of mismatched bits. This metric is called the Hamming distance.

@italic{Reminder}: Don't forget to think about geometry and dynamics.

For perceptrons we talked about how the weight vector moved the direction it pointed. Here we don't have the weight vector moving, but you can visualize what is happening as updating a point in space. When we first input our four element vector we have a location in 4-D space. We multiply the first row of our weight matrix against our column of the input vector and we see, in effect, what is the effect on our first element (node) of all the other weighted inputs coming in to it. We then "update" that location. Maybe we flip it from a 1 to a zero (or vice versa). Then we try the next row of the weight matrix to see what happens to the second element. As we change the values of our nodes we are creating new points. The sequence of points is a trajectory that we are tracing in the input space. In this simple situation here we only require one pass to reach the final location, but in other settings we might not. In that case we just keep repeating the process until we do. One of the wonderful insights that Hopfield had was that by conceptualizing this process as an "energy" he could mathematically prove that the process would always reach a resting place.

@subsection{Hebb's @hyperlink["https://en.wikipedia.org/wiki/Outer_product"]{Outer Product} Rule}
@margin-note{Why is this learning rule called "Hebb's"? And if you don't know who Hebb is let's take a moment to figure that out.}

The strength of a change in a connection is proportionate to the product of the input and outputs, i.e. @($$ "\\Delta A[i,j] = \\eta f[j]g[i]") and @($$ "g[i] = \\sum_j~A[i,j]~f[j]") therefore, @($$ "\\vec{g} = \\mathbf{Af}"). @margin-note{Does it matter that the (\mathbf{W}) comes first?}

@margin-note{What is an outer product? Can you compute one with racket?}

@section{Hopfield Homework Description: Robustness to Noise}
Overview of the steps to take:
@itemlist[#:style 'ordered @item{Create a small set of random data for input patterns.}
          @item{Generate the weights necessary to properly decode the inputs.}
          @item{Conceive of a way to randomly corrupt the inputs. Perhaps by flipping some bits and show that your network does correctly decode the uncorrupted inputs.}
          @item{Report the accuracy of the output. Explore how the length of the input vector and the number of bits your "flip" impact performance.}]

Detailed instructions:
@itemlist[#:style 'ordered
          @item{Make the input patterns 2-d, square and of size "n".}
          @item{Use a bipolar system and have, roughly, equal numbers of +1s and -1s in your patterns.}
          @item{Make a few of them and store them in some sort of data structure.}
          @item{Using those patterns, compute the weight matrix with the following equation:
   @($$ "w_{ij} =\\frac{1}{N} \\sum_{\\mu} value^\\mu_i \\times value^\\mu_j")
   Where N is the size of the patterns, that is how many "neurons". @($ "\\mu") is an index for each of the patterns, and @($"i") and @($"j") refer to the neurons in the pattern @($ "\\mu"). Do this @bold{in code}. The computer is good   for this manual, repetitive sort of stuff.}
          @item{Program an @bold{asynchronous} updating rule, run your network until it stabilizes, and then show that you get back what you put in.}
          @item{Then do the same for at least one disrupted pattern (where you   flipped a couple of bits around.)}]

  
@generate-bibliography[#:sec-title "Hopfield Bibliography"
                       #:tag "ref:hopfield"]
