#lang scribble/book

@(require scribble/base
          scribble-math/dollar
	  scribble/example
          scribble/manual
          symalg
          scriblib/figure
          scribble/core
          scribble/html-properties)


@(define ca-eval
   (let ([eval (make-base-eval)])
     (eval '(begin (require "./code/ca.rkt")))
     eval))

@title{Introduction to Linear Algebra and Neural Networks}
@section{Linear Algebra Goals}
Our goal for the next few lessons is to come to understand
@itemlist[@item{What is a neural network?}
               @item{What mathematics are needed to build a neural network?}
               @item{How can neural networks help us understand cognition?}]

As a first illustration of some of the key ideas we will execute a simple cellular automata rule. What I hope to emphasize through this exercise is that whenever you can get the computer to do a repetitive task do so. It will do it much better than you. And even if it takes you days to get the program right for many task you will quickly save the time in the long run. Second, we are using a simple rule (as you will shortly see). But even though the rule is local it yields impressive global structure. And very slight tweaks in this local rule can lead to large macroscopic changes. While the variation in our rule is very limited the array of behaviors we can observe is vast. @margin-note*{Match these features to facts about neurons. Extend them to what you believe will be their application in neural networks.}

@section{Drawing Cellular Automata}

This activity has several stages. For the first stage make sure you can load the file @hyperlink{"./../code/ca.rkt"} into Dr Racket and that it runs.

Next, pick a number between 0 and 255 inclusive. In your interactive window use the function @tt{rule-tester} to generate the input-output pairing for your rule like so.


@examples[#:label "Testing Rule 22"
          #:eval ca-eval (rule-tester 22 test-set)]

Use your rule and a piece of graph paper to implement your rule.

Color a single black square in the middle of the top row. Then moving down one row and working left to right implement your rule by coloring in the appropriate square.

@figure[
  "fig:grid-automata"
  @elem{Nearest Neighbors in the Grid}
  @elem{@image[#:scale 1.0]{./images/grid.png}}]

For example, if the boxes 1, 2, and 3 were 'w, 'w, and 'b I could color the square with the question mark black. Then I would move one to the right and square 2 would become my new number 1 and so on.

Complete several rows following your rule.

What you have probably noticed is that this is tedious and mistake prone, but your rule is a good example of a function. A function can be conceived as a set of pairs. The first element of the pair is the input, and the second element of the pair is the output. Functions require that each input element be unique. Implementing your rule  makes you the metaphorical neuron deciding whether or not to fire (color the square black) based on the input you receive from neighboring neurons. 

Having learned how tedious and error prone this process explore some of the other rules using the functions in @tt{ca.rkt}. The simplest method is to use the function @tt{d-r-a <some-rule-number>}. You can adjust the size and scale with various optional arguments and even print it to a file if you find one you like. Here is one of my favorites as a demonstration.

@examples[#:label "Rule 110"
          #:eval ca-eval
          (d-r-a 110 #:num-rows 100 #:num-cols 100 #:scale 3)]

What are the lessons learned from this exercise?
@itemlist[#:style 'ordered
          @item{Repetitive actions are hard. We (humans) make mistakes following even simple rules for a large number of repeated steps. Better to let the computer do it since that is where its strengths lie.}
          @item{Complex global patterns can emerge from local actions. Each neuron is only responding to its immediate right and left yet global structures emerge.}
          @item{These characteristics seem similar to brain activity. Each neuron in the brain is just one of many. Whether a neuron spikes or not is a consequence of its own state and its inputs (like the neighbors in the grid example).}
          @item{From each neuron making a local computation, global patterns of complex activity can emerge.}
          @item{Maybe by programming something similar to this system we can get insights into brain activity.}]

@subsection{Comments on the programmatic implementation}

The code in @tt{ca.rkt} involves a lot of looping. I used @italic{for} loops extensively, though sometimes these were @tt{for/fold} variants. We need to inch along the columns and down the rows. The plotting used the built in functionality of @bold{racket} for generating pictures as output.

The potentially more tricky part was going from a number (in decimal) to a binary representation that had the right number of places occupied. I ended going back and forth between strings and lists to get what I wanted. This was undoubtedly a kludge, but there is a slogan to first get it working, and then make it better. Trying to be too perfect and too elegant can cost you time in the long run. It is often easier to revise a functioning program then write one from the start. 

Initially I did not have all the testing code, because I was adapting code I had written in the past. However, when things did not work it turned out I went faster by slowing down and writing code that allowed me to inspect the state of my various variables, and individually try out the small functions on test input.

@section{More Lessons from Cellular Automata}

Cellular automata demonstrate some basic lessons that we will make use of when thinking about neural networks. One of these points is that there may be simple representations for complex entities. If we can find the right language for representation we may get concision and repeatability as by-products. This is demonstrated by the @hyperlink["https://plato.stanford.edu/entries/cellular-automata/supplement.html"]{naming convention for the rules of cellular automata}.

In emphasizing that local decisions can produce interesting global effects it may be interesting to examine other similar uses of the cellular automata idea. One famous and visually pleasing one is the @hyperlink["https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life"]{Game of Life}.

The analogy of automata to simple neurons may be deeper than at first it appears. Some very famous thinkers connected the two. One of the most brilliant people of all time, John von Neumann, was working on a book about automata and the brain at the time of his death. I have linked to a commentary in case you are interested in reading further see @hyperlink["http://www.ams.org/bull/1958-64-03/S0002-9904-1958-10214-1/S0002-9904-1958-10214-1.pdf"]{Claude Shannon (pdf)} as well as to a pdf @hyperlink["https://complexityexplorer.s3.amazonaws.com/supplemental_materials/5.6+Artificial+Life/The+Computer+and+The+Brain_text.pdf"]{copy} of the book: @hyperlink["https://ocul-wtl.primo.exlibrisgroup.com/permalink/01OCUL_WTL/vk29fk/alma994863683505162"]{The Computer and the Brain}).

A contemporary mathematician and the inventor of the Mathematica software system also believes that cellular automata may be a theory of everything. See what Stephen Wolfram @hyperlink["http://www.wolframscience.com"]{thinks}.

