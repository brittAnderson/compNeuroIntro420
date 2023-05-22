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
          "./../code/refs.rkt"
          "./../code/bp-net-figs.rkt")
              
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


@title{Backpropagation}

@section{Warm up questions}

@itemlist[#:style 'ordered @item{What is a neural network?}
          @item{What is the difference between supervised and unsupervised learning? Give an example of each?}
          @item{What is the @italic{activation function} we have used for the perceptron and delta rule networks?}
          @item{What role does "error" play in the perceptron and delta learning rules?}
          @item{For a multilayer network how do you know how much of the "error" to pass back into the deeper layers of the network?}]

@;Some Answers
@;
@;1. nodes and edges and a whole bunch of other options.
@;2. Supervised we are given the desired outputs to learn. Perceptron and Hopfield.
@;3. Threshold function.
@;4. It is part of the weight updating equation.
@;5. That is what we will be working on today.



@section{Sigmoid Functions}
Our prior networks have been forms of threshold units. We check to see if our activation cleared a certain hurdle, and if so, set its value to 1 or -1.

While this step-function approach was used originally, it is more common now to scale the output continuously between a lower and upper bound. One of the intuitions is that this is like a probability that the neuron might fire.

@examples[#:eval plot-eval
          (begin
            (define (sig x) (/ 1.0 (+ 1 (exp (* -1.0 x)))))
            (plot (function sig (- 5) 5)
                #:title "A sigmoid function."))]


@($ "\\frac{1}{1+e^{-z}}")


@subsection{A few questions about sigmoid functions}
@itemlist[#:style 'ordered 
          @item{Why is it called "sigmoid?"}
          @item{What advantage does it offer over a threshold function?}
          @item{Is it the only "sigmoid" function? Does it have other names?}
          @item{Can you guess an an advantage to this particular form of the equation?}
          @item{How do use this with a neural network, i.e. what is @($"z")?}]


@;Some Answers
@;
@;1. S shaped
@;2. Differentiable
@;3. No,
@;![Sigmoid Functions](https://upload.wikimedia.org/wikipedia/commons/thumb/6/6f/Gjl-t%28x%29.svg/1000px-Gjl-t%28x%29.svg.png "Sigmoid Functions Illustrated")
@;4. The exponential gives it some nice differentiation properties. Can you differentiate it? Go ahead. Why would you care about knowing the derivative?
@;
@; What do you need to know?
@;
@; a. Differentiate a quotient.
@;
@; b. Chain rule.
@;
@; c. Derivatives of sums and sums of derivatives
@;
@; d. Fancy rearrangements.
@;
@; e. The derivative of $e^z$.
@;5. The weighted input.
@;
@;How to use it?
@;
@;Have to input the activation of the neuron into the $z$ of the sigmoid function. That is we need, @;$\frac{1}{1+e^{-(\sum_i xw)}}$
@;

Getting ready to put things together. Can you write a small snippet of racket code that takes a vector of inputs, appends a bias input, combines this with a suitable weight vector using the scalar product and pipes the result though the sigmoid function?
Think about equations qualitatively.

Remember, one of the goals of computational modeling is to get an insight into the implications of our ideas and theories. Sometimes this means running a model to see what comes out of it. But it can also mean that we look at the equations that go into the model and think about their "behavior" to get some sense of how things will behave that have particular functional forms. 

How might you do that here? Think about how it the process just described is the same as, and different from, the threshold based rules we have been using up until now. Think about extreme values: what happens at the extremes? How is that like (or different from) our older threshold rules?

Remember that derivatives are rates of change. If we want to know how the error changes as we change something else we will need a derivative. What problem does this approach run into when using a threshold unit?

In the sigmoid illustrated above where is the derivative maximal? What happens if the dot product of a weight vector and input vector are large? Or very small (and what does small mean here)? What about negative extremes and positive extremes. 

Can you think of a function that would give us an even simpler derivative and why might we want (or not want) to use it?

Why are we starting this discussion of the backpropagation algorithm with all this discussion of activation functions?

In summary, we want to understand ...
1. What is being backpropagated?
2. What is it we want our network to do?
3. How do we guide it?

@;; Answers
@;; 1. Cost
@;; 2. Move to the point of minimal cost
@;; 3. We use the derivative (just like we have all course long).

Many networks have a cost function. We may want to know more than just whether you were right or wrong, but how wrong? In a continuous case being "right" might not even really be possible - what is the value of @($"\\pi") ? Our computers cannot be precise. There is not a single "right" cost function either, but what might you suggest that we use, and why?

What would you suggest as the cost function?


@subsubsection{Mean Squared Error}

It's always a good guess and a reasonable starting point

@($$ "C(\\mathbf{w}) = \\frac{1}{2\\mathrm{n}}\\sum_\\mathbf{x} \\lVert \\mathbf{y}(\\mathbf{x}) - \\mathbf{a}\\rVert^2")



Some Questions:
Why isn't this a function of @($ "\\mathbf{x}") and @($"\\mathbf{y}") too?

What is the @italic{dimensionality} of the part of the equation inside the double lines? 

What do you call the operation characterized by the double lines? 

Why is adjusting weights for a multilayer network hard?


@;Some Answers:
@;1. It is, but we can't vary those, so we treat them as constants. 
@;
@;2. the dimensionality is that of the output vector
@;
@;3. the norm (how big or how far) - think of Hamming distance from the Hopfield network.
@;
@;4. Because we don't know how to distribute the errors for the intermediate connections. How much /blame/ do we give to each one? 


@section{Backpropagation 1}

We learned in implementing the XOR function that we can solve complex (i.e. non-linearly separable ones) problems if we use a @italic{multi-layer} network. However we have a problem. In a single layer network it is clear how our output error depends on the weights, but how do we apportion out the error to earlier layers when we are in a multi-layer situation?

If you think about it the only thing we are really free to change are the weights. Sure, our error will change if we change the output to make it closer to the input, but in the common scenarios for which we use such neural networks we want to achieve a particular input-output mapping. For that reason as well, we can't alter our inputs. They are our data. We have to accept them as given.

If weights are the only thing we can change we have to discover, if one exists, an algorithm for apportioning out the error to early weights. This is the achievement of @hyperlink["http://www.nature.com/nature/journal/v323/n6088/pdf/323533a0.pdf"]{backpropagation algorithm}. If you look you will find that you can read this article. It does not use any mathematical concepts that we have not already covered. You have all the notation, language, and concepts. Note that the abstract makes sense to you. 

@italic{@bold{Class Question?}}
Is backpropagation biologically plausible?

Some intuition can help to understand the ideas behind the backpropagation algorithm even if the math gets too intricate for you (and it is more an issues of intricacy than concepts). If we get a wrong answer we might want to change the contributions from a node that is very active. This is because that even if we have a node that is badly weighted if its total activation is small it can't be contributing much to the error. We want to concentrate on nodes and weights were the activity is large and thus small changes will have big effects on errors. This should suggest the idea of a derivative. We want to put most of our change at locations where the ratio of improved output to small changes of weights is high. It is there we get the best return from our adjustment. 

@subsection{Some Details}

The mathematics behind the backpropagation algorithm involves derivatives. These derivatives are usually "partial". We study the rate of change of our dependent variable as a function of one of many possible independent variables. If we want to study how the error changes as we change one specific weight in our network we are looking at the partial derivative. This is typically notated with a sort of curly d like @($ "\\partial"). This means that we could write our rate of change of the error as a function of the change in a particular weight in layer l connecting the kth neuron in the l-1 layer to the jth neuron in the l layer as
@($ "\\frac{\\partial E}{\\partial w_{jk}^l}"). Note this ordering maybe backwards from your intuition. 

We do not have an equation that directly specifies the change of error in terms of a specific weight, but we can tell how the error changes if we change the output of the last layer. That output is determined by our activation function which is determined in part by the input. By looking how this chain of relationships change we can track our way back to a dependency on the weights. In calculus there is a rule for navigating such chains. It is called the @hyperlink["https://en.wikipedia.org/wiki/Chain_rule"]{chain rule}.

The details of all this back tracking is tedious, but collapses into two different classes. One is for the output layer where we have a direct comparison to the error. The second is all the earlier layers, the so-called "hidden" layers, where we have to say our a current weights change depends on what went before. Thus to know what to do at layer l-1 we need to know facts about layer l. But we only need the immediately preceding layer. So, if we start at the top and work our way back layer by layer we can backpropagate the error. Doing the same thing over and over again is what computers are good at and people are bad, so if we can we want to write a program do this repetitive computation for us.

Today, there are many nice libraries that have been written to scale nicely, and to run efficiently. We do not have to write this algorithm ourselves. One of the most popular is the python library @hyperlink["https://pytorch.org/"]{pyTorch}. If you plan to use backpropagation for any real application you should probably not write your own implementation. It is an error prone and frustrating process that will probably not run as fast or reliably as the use of an external library. Check your language for a suitable implementation.

@subsection{Learning About Backpropagation}

While the above is true for a professional use case, it is not true from a learning perspective. There are many benefits from working through some of the math yourself, and trying to write your own simple implementation. The following are intended as bread crumbs if you decide to follow that route.

@subsubsection{Bread Crumbs}

If you decide to try and follow the chain rule chain to see how weights in early layers can be updated based on backpropagated errors start with a single linear line of nodes that each one connects to the next with a single weight. This is not a useful network for computing anything, but it is a nice simple system for exploring the mathematical relationships without worrying too much about subscripts. 

@figure["fig:linear-net"
        @elem{A simple linear network that can be useful for tracking the chain rule derivations.}
        @(linear-net-fig)]

I found this @hyperlink["https://mattmazur.com/2015/03/17/a-step-by-step-backpropagation-example/"]{this page} to give a very nice overview of the derivatives and how they relate as you expand them via the chain rule. There are also some simple numerical examples that you can work by hand to check your understanding. This site does not include code, which can be a nice way to focus on the logic before worrying about how to implement it. 

Often you will see the "sigma" character in on line discussion. This sigma is generally whatever sigmoid, roughly s-shaped, function is being used. As such, the specific derivative will depend on that choice. Don't assume that it is always going to be the logistic function, though this is a common choice. 

How would you write as an equation with the "sigma" (@($ "\\sigma") summation sign the value activation of a single @italic{arbitrary} neuron in an @italic{arbitrary} layer of a multi-layer network?

@bold{The activation}
   
To help you check your understanding try to describe in words what is happening here: 
@($$ "a^l_j = \\sigma \\left ( \\sum_k w_{jk}^l~a^{l-1}_k \\right )")

One of the reasons for this type of equation with all its formatting as subscripts and superscripts is that the coding of the backpropagation algorithm often uses multi-dimensional arrays. All the inputs are treated as vectors and loaded into a matrix where each row (or column) is one pattern, and the collection is a matrix. The weights between one layer and the next are going to be a matrix as well with one dimension the number of nodes in the first layer and the other dimension the number of nodes in the next layer. Each row/column intersection holds the value of one weight. To collect all the weight matrices into a single structure we need to aggregate them into some sort of three dimensional structure where each matrix can be thought to be stacked on the one that came before. If this sounds complicated to think about imagine trying to code it. It is a project, and it does not map easily on to the logic of the neural network that we learn about as layers and nodes serially connected.

In addition, there are other arrays that are needed. We must keep track of the errors that we backpropagate and the inputs that are going forward. Depending on your implementation you may need an array for inputs, one for weights, one for activations, one for "deltas", and then you will need to progressively loop over all the layers from beginning to end to get the feedforward output, and then backwards to apply the weight adjustments from end to beginning. This requires careful book-keeping and making sure you orient the various matrices correctly. 

If you are looking for a step by step approach to coding this algorithm, one that uses an object oriented orientation, this @hyperlink["https://machinelearningmastery.com/implement-backpropagation-algorithm-scratch-python/"]{version} in python is accessible.  

Here is a pseudo-code summary:
@itemlist[#:style 'ordered
          @item{Fix the inputs of the first layer to the input pattern @($"x")}
          @item{Compute the weighted input to each neuron of the next layer using the input, weights and biases.}
          @item{Compute the weighted cost function error vector for the last layer.}
          @item{Backpropagate the error}
          @item{Use the backpropagated error to update the weights}]

I wrote a version in racket that seems to work for simple cases. As I only tested it in a few limited cases you are encouraged to probe it for bugs and logic errors and suggest corrections.

@(define backprop-eval
   (let ([eval (make-base-eval)])
     (eval '(begin (require
                    racket/list
                    "./code/backpropagation.rkt")))
     eval))

@examples[#:label "Illustrating Backpropagation Code"
          #:eval backprop-eval
          (begin
            (displayln "Before Training")
            (for ([i (map first data-xor)])
              (displayln (test-learning i test-net)))
            (define many-loops-bp (bp-loop data-xor test-net #:loop-no 1000))
            (displayln "After Training 1000 loops")
            (for ([i (map first data-xor)])
              (displayln (test-learning i many-loops-bp))))]


@section{Homework}
The homework will only require you to use the library provided in order to explore some of the features of the backpropagation algorithm. It will acquaint you with some of the terminology and some of the practical considerations. 
@itemlist[#:style 'ordered
          @item{Does a backpropagation network always get the same answer? Create at least three random networks. Train them for the same number of trials and compare their accuracy at the end and inspect the weights of the last layer. Are they the same?}
          @item{Does the number of neurons matter or the number of layers? Should you need more than one layer? Compare a 2 - 5 - 5 - 1 to a 2 - 10 - 1 network and report your observations.}
          @item{What is a global minimum and how does it differ from a local minimum. Which are you guaranteed to get with backprop?}
          @item{Test your network for catastrophic forgetting. In my code I train on each of the four XOR inputs one after the other over and over. Test just one pattern for the same number of loops. Then, using those weights as your ending verify you are getting the correct answer. Then train on the second pattern starting from that network. Now go back and test on the original input pattern. Report on your observations.}]

 
@subsection{Additional Readings}
In years past some student have recommend other sources they like.
@itemlist[@item{@hyperlink["http://neuralnetworksanddeeplearning.com/chap1.html"]{On line deep learning textbook}}
               @item{@hyperlink["https://youtu.be/bxe2T-V8XRs?list=PLiaHhY2iBX9hdHaRr6b7XevZtgZRa1PoU"]{Youtube video on backpropagation coding}}]

@generate-bibliography[#:sec-title "Backpropagation Bibliography"
                       #:tag "ref:backprop"]
