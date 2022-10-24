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

The Hopfield network@~cite[hopfield-orig] has taught many lessons, both practical and conceptual. He showed physicists a new realm for their skills and added recurrent (i.e. feedback) connections to network design (output becomes input). He changed the focus from network architecture to that of a dynamical system. Hopfield showed that the network could remember and it could do some error correction, it could reconstruct the "right" answer from faulty input.

@figure["fig:hopfield-net"
        @elem{Hopfield Recurrent Connections}
        @elem{@(hopfield-net)}]

@; *** How does a network like this work?
@; 
@; 1. Each node has a value.
@; 2. Each of those arrowheads has an associated weight.
@;    The dotted lines indicate that those connections are missing. There are no self connections. Later you will tell me why this /must/ be the case.
@; 
@; *** Test your understanding:
@; 1. Tell me what the input for a network like this in terms of the linear
@;    algebra constructs we have talked about.
@; 2. Tell me what the weights look like in terms of the linear algebra
@;    objects we know about.
@; 3. How might we conceive of "running" the network for one cycle in terms
@;    of the above. Althought we will do it that way for a bit, that is not
@;    actually how it is done typically.
@; 
@; *** Worked Example
@;   $$A = \(\{1,0,1,0\}^T\)$$
@;   $$B = \(\{0,1,0,1\}^T\)$$
@; 
@; $$  \begin{bmatrix}
@;   0 & -3 & 3 & -3\\
@;   -3 & 0 & -3 & 3\\
@;   3 & -3 & 0 & -3\\
@;   -3 & 3 & -3 & 0\\
@;   \end{bmatrix}$$
@; 
@; Threshold rule:
@; 
@; $output(t)=\{\begin{array}{c} 1\; \mbox{if } t \geq \Theta\\ 0\; \mbox{if } t < \Theta \end{array}$
@; 
@; Let $\Theta = 0$.
@; 
@; What are the outputs for each input pattern?
@; 
@; What would you guess you would get for an input of $\{1,0,0,0\}^{T}$ and why?
@; 
@; *** More Questions to Think About
@; 1. Is A or B *closer*?
@; 2. What does it mean for something to be *closer*?
@; 
@; **** Distance Metrics
@; 1. Non-negative
@; 2. symmetric
@; 3. identity
@; 4. [[https://en.wikipedia.org/wiki/Triangle_inequality][triangle-inequality]].
@; 
@; **** [[https://en.wikipedia.org/wiki/Hamming_distance][Hamming Distance]] Example
@; Count the differences in bits.
@; 
@; *** Digression: Don't forget to think about geometry and dynamics.
@; 
@; In the Perceptron lecture, we talked about how the weight vector moved around. But here we don't have the weight vector moving, but we are changing the points on the input. You can think of this as a point moving around in a four dimensional space. At each point in time, there is a new coordinate position. This is a dynamical system. What Hopfield did was, in part, use the language of physics to describe and evaluate what a network was doing. In his case, he used the idea of "energy" to compute the state of the network and to show that it would, under his conditions, achieve a minimum.
@; 
@; *** Hebb [[https://en.wikipedia.org/wiki/Outer_product][Outer Product]] Rule
@; The strength of a change in a connection is equal to the product of the input and outputs, i.e. $$\Delta A[i,j] = \eta f[j]g[i]$$ and $$g[i] = \sum_jA[i,j]f[j]$$ therefore, $$\vec{g} = \mathbf{Af}$$ Does it matter that the (\mathbf{W}) comes first?
@; 
@; Why is this named after Hebb? And if you don't know who Hebb is let's take a moment to figure that out. 
@; 
@; ** Class Exercise
@; 1. Assume that $\vec{g}$ and $\vec{f}$ are each three elements in
@;    length.
@; 2. Assume that the $\mathbf{W}$ starts at zero, and that one pattern is
@;    learned.
@; 3. Write the matrix $\mathbf{W}$ in a symbolic form, that is with $f's$
@;    and $g's$ and $A's$; not numbers.
@; 4. Then, when you have done that, and see if you can find a more compact
@;    way to represent the matrix $\mathbf{W}$ using the vectors $\vec{f}$
@;    and $\vec{g}$.
@; 
@; $$
@; \(\mathbf{W}\) = $\eta \times \begin{bmatrix} f[1]g[1] & f[2]g[1] & f[3]g[1]\\ f[1]g[2] & f[2]g[2] & f[3]g[2]\\ f[1]g[3] & f[2]g[3] & f[3]g[3] \end{bmatrix} = \eta \, \vec{g} \, \vec{f}^T$ $$
@; 
@; 
@; ** Hopfield Homework Description: Robustness to Noise
@; 1. Create a small set of random data set of input patterns or find something on line.
@; 2. Generate the weights necessary to properly decode the inputs.
@; 3. Show that your network does correctly decode the uncorrupted inputs.
@; 4. Add noise to the network by randomly flipping one bit in each input vector. Report the accuracy of the output. Explore how the length of the input vector and the number of bits your "flip" impact performance.
@; 
@; IAMHERE
@; 
@; 
@; More detailed:
@; 
@; 1. Make the input patterns 2-d, square and of size "n".
@; 2. Use a bipolar system and have, roughly, equal numbers of +1s and -1s
@;    in your patterns
@; 3. Make a few of them and store them in some sort of data structure.
@; 4. Using those patterns compute the weight matrix with the following
@;    equation:
@;    $$ w_{ij} =\frac{1}{N} \sum_{\mu} value^\mu_i \times value^\mu_j$$
@;    Where N is the size of the patters, that is how many "neurons". $\mu$
@;    is an index for each of the patterns, and $i$ and $j$ refer to the
@;    neurons in the pattern $\mu$. Do this *in code*. The computer is good
@;    for this manual, repetitive sort of stuff.
@; 5. Program an asynchronous updating rule, run your network until it
@;    stablelizes, and then show that you get back what you put in.
@; 6. Then do the same for at least one disrupted pattern (where you
@;    flipped a couple of bits around.)
@; 
@; ****** Imports
@;        :PROPERTIES:
@;        :CUSTOM_ID: imports
@;        :END:
@; 
@; #+BEGIN_SRC python :session *hop* :tangle yes
@;   import matplotlib
@;   import matplotlib.pyplot as p
@;   import random as r
@;   import numpy as np
@;   from copy import copy,deepcopy
@; #+END_SRC
@; 
@; #+RESULTS:
@; 
@; ****** Generate Random Size and Number of Patterns
@;        :PROPERTIES:
@;        :CUSTOM_ID: generate-random-size-and-number-of-patterns
@;        :END:
@; 
@; #+BEGIN_SRC python :session *hop* :tangle yes
@;   def hopGenSzN ():
@;       pn = r.randint(4,8)
@;       psz = np.repeat(r.randint(6,10),2)
@;       return((pn,psz))
@; #+END_SRC
@; 
@; #+RESULTS:
@; 
@; ****** Function to make random patterns
@;        :PROPERTIES:
@;        :CUSTOM_ID: function-to-make-random-patterns
@;        :END:
@; 
@; #+BEGIN_SRC python :session *hop* :tangle yes
@;   def hopMkPatts(numPs2Make,psize):
@;       ps = []
@;       for n in range(numPs2Make):
@;           tobepatt = np.array([[(np.round(r.random()) *2) - 1 for i in range(psize[0])] for j in range(psize[1])])
@;           ps.append(tobepatt)
@;       return(ps)
@; #+END_SRC
@; 
@; #+RESULTS:
@; 
@; ****** Function to Make Weight Matrix for a set of Generated Patterns
@;        :PROPERTIES:
@;        :CUSTOM_ID: function-to-make-weight-matrix-for-a-set-of-generated-patterns
@;        :END:
@; 
@; #+BEGIN_SRC python :session *hop* :tangle yes
@;   def hopMkWts(patterns):
@;       w = np.zeros(list(map((lambda x: x**2),patterns[0].shape)))
@;       for p in patterns:
@;           w = w + np.outer(p,p)
@;       w = 1.0/len(patterns) * w
@;       np.fill_diagonal(w,0)
@;       return(w)
@; #+END_SRC
@; 
@; #+RESULTS:
@; 
@; #+BEGIN_SRC python :session *hop* :tangle yes
@;   def hopLoop(patt,wts):
@;       workingpatt = deepcopy(patt)
@;       testpatt    = deepcopy(patt)
@;       while True:
@;           rws = list(range(patt.shape[0]))
@;           cls = list(range(patt.shape[1]))
@;           r.shuffle(rws)
@;           r.shuffle(cls)
@;           linpatt = np.reshape(workingpatt,(1,workingpatt.size))
@;           for rw in rws:
@;               for cl in cls:
@;                   workingpatt[rw][cl] = 1.0 if np.dot(linpatt,wts[rw*len(rws) + cl]) > 0 else -1.0
@;           break
@;                         #          if (np.all(testpatt == inpatt)): break
@;       return(workingpatt)
@; #+END_SRC
@; 
@; #+RESULTS:
@; 
@; ****** Function to Loop Asynchronously Through all the Elements of a
@; Pattern Until Output is Stable
@;        :PROPERTIES:
@;        :CUSTOM_ID: function-to-loop-asynchronously-through-all-the-elements-of-a-pattern-until-output-is-stable
@;        :END:
@; 
@; ****** A Function to Visualize the Output
@;        :PROPERTIES:
@;        :CUSTOM_ID: a-function-to-visualize-the-output
@;        :END:
@; 
@; #+BEGIN_SRC python :session *hop* :tangle yes
@;   def hopPlot(ins,outs): 
@;       r = len(ins)
@;       c = 3
@;       pltcntr = 1
@;       for i in range(r):
@;           p.subplot(r,3,pltcntr)
@;           p.imshow(ins[i])
@;           p.subplot(r,3,(pltcntr+1))
@;           p.imshow(outs[i])
@;           p.subplot(r,3,(pltcntr+2))
@;           p.imshow(ins[i]-outs[i])
@;           pltcntr = pltcntr+3
@;       return(p)
@; #+END_SRC
@; 
@; #+RESULTS:
@; 
@; ****** Example of How to Use
@;        :PROPERTIES:
@;        :CUSTOM_ID: example-of-how-to-use
@;        :END:
@; 
@; #+BEGIN_SRC python :session *hop* :tangle yes
@;   myn,mysz = hopGenSzN()
@;   myps = hopMkPatts(myn,mysz)
@;   w = hopMkWts(myps)
@;   outps = []
@;   for inp in myps:
@;       op = hopLoop(inp,w)
@;       outps.append(op)
@; #+END_SRC
@; 
@; #+RESULTS:
@; 
@; #+BEGIN_SRC python :session *hop* :tangle yes :results graphics file :file "hop.png"
@;   myp = hopPlot(myps,outps)
@; #+END_SRC
@; 
@; #+RESULTS:
@; 
@; file:8a1284b97d78ce1c4330d596fa0ccc6873678a1d.png]]
@; 

  
@generate-bibliography[#:sec-title "Hopfield Bibliography"
                       #:tag "ref:hopfield"]
