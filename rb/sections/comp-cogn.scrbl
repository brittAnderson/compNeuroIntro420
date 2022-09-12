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

@(use-mathjax)

@(define-cite ~cite citet-id generate-bibliography #:style author+date-style)

@title{Is Cognition Computational?}

@section{Motivating questions:}
@itemlist[#:style 'ordered
          @item{Is there anything a human can think that a computer cannot compute?}
          @item{What does it mean for a function to be "computable"?}
          @item{What does it mean to say that cognition is computational?}
          @item{What implications does the answer have for modeling cognition?}]

@section{Introduction}
Is there a difference between computational cognitive neuroscience and cognitive computational neuroscience? The former seems to suggest that we are interested in explaining cognition directly from the actions of neurons and then using computational tools as adjuncts to aid this attempt. On the other hand, when you reverse the order it seems as if you are trying to explain thinking via the computational accounts of neuronal function. The latter seems to require a clear link between notions of computation and models of thinking. To use Marr's three levels as a scaffold, we are taking neurons as our implementation, positing algorithms for them, and assuming that computational principles populate the level of our cognitive abstractions. Measures of success depend on the meanings of terms. @margin-note{What do people who call themselves @hyperlink["https://2022.ccneuro.org/program.php"]{Cognitive Computational Neuroscientists} study?@~cite[cog-comp-neurosci-is]}

In all of this is the basic idea that if you want to understand @emph{mind} you need to develop a @emph{theory} (and understand what that word means), express your theory clearly for which the best language is @emph{math}, and then explore the implications of your construction via simulation, which requires the writing of @emph{code}. Mind → Theory → Math  → Code. Let's explore a little more what is implied by this pathway. What it means to use computing as a model for mind.

@section{Computing in Minds and Computers}

One definition of whether something is computable is whether there exists a Turing Machine that computes it. That Turing machine must halt, and since one cannot decide in advance for all machines whether they will halt whether something is computable is, in general, undecidable.

Although most of us learned of Turing machines in the context of whether a computer has human intelligence, the so-called @emph{Turing test}, the model of Turing computability emerged from thinking about humans computing @~cite[turing-machine]. @margin-note*{A nice short version of this history can be found in this @hyperlink["https://link.springer.com/content/pdf/10.1007/978-3-642-31933-4.pdf"]{pdf}.} 

@subsection{Classical Computational Theory of Mind}

The classical computational theory of mind says that in all important ways the mind is like a Turing machine. If we want to  model (or emulate) functions of mind one way would be to build a Turing machine. However this can be a practical challenge, and one can question the insight gained from this approach. Still given the theoretical and practical prominence given to Turing computatbility it behooves us to know what a Turing machine truly is.

@subsubsection{What is a Turing machine? Some Background and Details}
@hyperlink["http://www.turingarchive.org/browse.php/B/12"]{Turing machines} are quite simple implements. While one can build a physical Turning machine, the more usual sense of the term is for a hypothetical computer that is comprised of
@itemlist[@item{a finite alphabet,}
               @item{a finite set of states,}
               @item{the capacity to read and write to a single location in memory, and the ability to adjust to the memory location immediately left or right or to make no move at all, and}
               @item{a set of instructions (or "machine table") that translates the combination of the current state and the current symbol to a new state and one of the acceptable actions.}]

Other models of computation are the @emph{lambda calculus}  and the @emph{theory of recursive functions}. Those alternative accounts of computability are interesting, and may offer more insight or be more practical in some situations, but it appears to be the case that they are equivalent. Anything designated "computable" by one these formal accounts is computable by the others as well. Does this mean that if you accept the computational mind hypothesis you must accept that the mind is able to be simulated by a Turing Machine? As a consequence does this mean that minds are @emph{@hyperlink["https://plato.stanford.edu/entries/multiple-realizability"]{multiply realizable}}?

@margin-note{@itemlist[@item{Are Turing machines @emph{digital}?}
                            @item{Is this an important distinction?}
                            @item{Does this mean that analog computations are omitted?}
                            @item{Would an analog paradigm be a better match to modeling mental activity?}]}

@section{Programming a Turing Machine: the Busy Beaver}

The following is a slightly formatted version of the Wikipedia description of a Turing machine.

@itemlist[#:style 'ordered
          @item{A Turing machine has n "operational" states plus a Halt state, where n is a positive integer, and one of the n states is distinguished as the starting state.}
          @item{The machine uses a single two-way infinite (or unbounded) tape.}
          @item{The tape alphabet is {0, 1}, with 0 serving as the blank symbol.}
          @item{The machine's transition function takes two inputs:
                    @itemlist[@item{the current non-Halt state,}
                                   @item{the symbol in the current tape cell,}]}
          @item{and produces three outputs:
                    @itemlist[@item{a symbol to write over the symbol in the current tape cell (it may be the same symbol as the symbol overwritten),}
                                   @item{a direction to move (left or right@";" that is, shift to the tape cell one place to the left or right of the current cell), and}
                                   @item{a state to transition into (which may be the Halt state).}]}]

We will use it to guide us to write a simple instance that computes a solution to the Busy Beaver problem. A n-state Turing machine has @($ "(4n + 4)2n") states. The formula is (symbols × directions × (states + 1))(symbols × states). The transition function (how to figure how where to go next may be seen as a finite look-up table. Each row of the table is a 5-tuple: (current state, current symbol, symbol to write, direction of shift, next state). Our goal with the Busy Beaver problem is to run our machine to produce as long a series of uninterrupted ones as we can @emph{and} @bold{halt}.

What it means to "run" a Turing machine is to start in the starting state with the current tape cell being any cell of a blank (all-0) "tape", and then iterate the transition function. If the Halt state is entered then the number of 1s remaining on the tape is called the machine's score. Different transition rules will give us different outputs, so we can score our machine based on its performance. 

To restate in a more general way, the n-state busy beaver (BB-n) game is a contest to find an n-state Turing machine having the largest possible score — the largest number of 1s on its tape after halting. A machine that attains the largest possible score among all n-state Turing machines is called an n-state busy beaver, and a machine whose score is merely the highest so far attained (perhaps not the largest possible) is called a champion n-state machine (This ends the lightly edited Wikipedia quote).

@subsection{Why Use the Busy Beaver Problem As an Example?}

ADD DISCUSSION OF BUSY BEAVER and computability

@subsection{A Busy Beaver Warm-Up}

A simple version of the Busy Beaver problem, and one you can do by hand with pencil and paper, is the n=2 version. Create a Turing Machine with the following transition rules:

@itemlist[@item{a0 → b1r}
               @item{a1  → b1l}
               @item{b0  → a1l}
               @item{b1  → h1r}]

IAMHERE -- need to port this code from LISP  → racket

***** My Code (see "[[https://en.wikipedia.org/wiki/Eating_your_own_dog_food][dogfooding]]")
#+Caption: Importing Necessary Library "Trivia"
#+begin_src lisp :results silent :exports code
(ql:quickload "trivia")
#+end_src

#+Caption: Creating a Structure to hold the features of a Turing machine
#+begin_src lisp :results silent :exports code
  (defstruct turing-machine state tape head-location)  
#+end_src

My idea was to see if making the code explicitly reflect the written description would be easy in lisp and whether that would help me write the code to implement it since I might be more easily able to follow the textual description directly.


#+Caption: Moving the tape under the head
#+begin_src lisp :exports code :results silent
  (defun move-left (tm)
    (let ((loc (turing-machine-head-location tm))
	  (lst (turing-machine-tape tm)))
      (if (= loc 0)
	  (setf (turing-machine-tape tm) (cons 0 lst))
	  (setf (turing-machine-head-location tm) (- loc 1)))))
  (defun move-right (tm)
    (let ((loc (turing-machine-head-location tm))
	  (lst (turing-machine-tape tm)))
      (when (= (+ loc 1) (length lst))
	(setf (turing-machine-tape tm) (append lst (list 0))))
      (setf (turing-machine-head-location tm) (+ loc 1))))
  
  (defun move (tm dir)
    (cond 
      ((eq dir 'left)
       (move-left tm))
      ((eq dir 'right)
       (move-right tm))))
#+end_src

Used separate functions for moving tape right or left, and then a generic ~move~ function that simply picks which one to call. Trying to encapsulate my logic in small functions. 

#+Caption: Testing Equality of Turing Machines
#+begin_src lisp :results silent :exports code
  (defun equal-sv (tm s v)
    (and (eql (turing-machine-state tm) s)
	 (= (elt (turing-machine-tape tm) (turing-machine-head-location tm)) v)))
#+end_src

I had to define a function for comparing states and values. Some computing languages have a way to overload functions or to invoke particular methods based on the type of values being compared. That might have made the expression of this comparison more natural than what I had to do here. Lisp has a notion of types and generics, but I am not currently advanced enough to program those quickly so I did it this way first, with the idea that I can refine this later on if it is deemed worth the time and effort. 

#+Caption: Defining the Rules
#+begin_src lisp :results silent :exports code
  (defun rule (tm) ;;state value
    (cond
      ((equal-sv tm 'a 0)
       (setf (turing-machine-state tm) 'b)
       (setf (elt (turing-machine-tape tm) (turing-machine-head-location tm)) 1)
       (move tm 'right))
      ((equal-sv tm 'a 1)
       (setf (turing-machine-state tm) 'b)
       (setf (elt (turing-machine-tape tm) (turing-machine-head-location tm)) 1)
       (move tm 'left))
      ((equal-sv tm 'b 0)
       (setf (turing-machine-state tm) 'a)
       (setf (elt (turing-machine-tape tm) (turing-machine-head-location tm)) 1)
       (move tm 'left))
      ((equal-sv tm 'b 1)
       (setf (turing-machine-state tm) 'h)
       (setf (elt (turing-machine-tape tm) (turing-machine-head-location tm)) 1)
       (move tm 'right))))
#+end_src

Each Turing machine is a /particular/ choice of rules (and some other things). This function ~rule~ is named a bit generically for what it is, which is a specific choice of algorithm. It compares the current state of the machine and value on the tape to each of the rules and then implements the update based on which rule matches. This way of writing the rules would not scale very well, but the similar structure of all the cases suggests that this could be made more concise, but at the expense of obscuring what is happening.

#+Caption: Running our Turing machine
#+begin_src lisp :results silent :exports both
  (defparameter *initial-state* (make-turing-machine :state 'a :tape (list 0) :head-location 0))
  (defun pp-tm (tm)
    (format t "~&state:~a, tape: ~a, head: ~a" (turing-machine-state tm) (turing-machine-tape tm) (turing-machine-head-location tm)))
  
  (defun bb (tm)
   (progn
     (pp-tm tm)
     (do ()
      ((eql (turing-machine-state tm) 'h))
       (rule tm)
       (pp-tm tm))))
    #+end_src

We declare an initial state. All Turing machines start in a state. The ~pp-tm~ function is a minimal /pretty printer/. Pretty printers are functions that take a complicated structure and render it in a human readable form that makes it easier for the human user to see what is going on or get essential information displayed.

The ~do~ clause of the program is essentially a loop. It has a termination condition that it checks for (which is when the machine is in the halting state). Unless that is met it just keeps following its rules, updating states, and letting us see the results via its pretty printer. 
    
#+begin_src lisp :exports code :results output
  (bb *initial-state*)
#+end_src

#+RESULTS:
: state:A, tape: (0), head: 0
: state:B, tape: (1 0), head: 1
: state:A, tape: (1 1), head: 0
: state:B, tape: (0 1 1), head: 0
: state:A, tape: (0 1 1 1), head: 0
: state:B, tape: (1 1 1 1), head: 1
: state:H, tape: (1 1 1 1), head: 2

*A run of my Turing Machine*
#+begin_example
state:A, tape: (0), head: 0
state:B, tape: (1 0), head: 1
state:A, tape: (1 1), head: 0
state:B, tape: (0 1 1), head: 0
state:A, tape: (0 1 1 1), head: 0
state:B, tape: (1 1 1 1), head: 1
state:H, tape: (1 1 1 1), head: 2
#+end_example


_Busy Beaver Competition_

Try to come up with a version or rules for n=5 and we will run your programs against each other in class. The current chanmpion produces 4098 ones over about 50 million steps. Don't try and break the record. We are learning about Turing machines and how to write code the implements mathematical and theoretical ideas for the elucidation of cognition. Spending too much time perfecting your Busy Beaver implementation misses the point.  


***** A tutorial article with examples and a nice visualization
If you are having a little trouble getting started then this [[https://catonmat.net/busy-beaver][article]] might help.
**** Why the [[https://en.wikipedia.org/wiki/Busy_beaver][Busy Beaver]]?
     Because the solution to this problem is [[#sec:noncomputable][*uncomputable*]]. What does it mean that we are solving this with our computers and our own reasoning, but that the problem itself is impossible to solve? Does that present any hurdle at all for using the Turing Machine as a model of mind?

One objection to the computer metaphor of the brain is that it is obviously wrong. Computers are programmable and the mind/brain is not. Of course, that is not what the proponents of a computational theory of mind mean, so it is perhaps best to avoide the mind as computer metaphor, and stick with the more clumsy name.

/What are the implications of the mind as Turing machine for implementation?/ Doesn't this mean that if the mind is a Turing machine (not /like/ a Turing machine, but actually a Turing machine - and if you hold that the mind is a computational machine then given that all that can be effectively computed is computable by a suitably specificed Turing machine; that is what logic requires you accept) then any functionally equivalent computational system, regardless of its hardware (i.e. it could be vacumn tubes or the population of China) would be a mind.

** Functionalism [[cite:&levin21_funct]]
You don't need to think about mental states in terms of what they are as "things". Think about them in terms of what they "do". You might compare this ideas of subjects and verbs or to objects and arrows in a [[file:category-day.org][category]]. Mental states serve a functional role in a cognitive system. They relate to the sensory input, motor output and to /each other./

There is an appearl to the mathematical notion of a function, but the term function here is more inspired by the idea of asking what the function of something is? What is the function of raising interest rates? Raising interest rates leads to less borrowing and less spending. Having the mental state of X leads to Y is a functionalist account.

There is more than one kind of functionalism. The one closest to our Turing machine is probably /machine state/ functionalism. 

** And there are others
I could greatly expand here, but I risk making this a philosophy course instead of an emphasis on mathematical tools and their programmatic translation. I will pause for now.

** A couple of troubling questions for the computational account of mind

1. It is a trivial account. Any sufficiently complex physical system (such as the molecules comprising the wall behind me or the brain) can be shown to be /isomorphic/ to the formal structure of /any/ program. If you view the mind as a program than you might as well say that you and the wall behind you share the same thoughts. 

2. There is no room for the time scale to matter and there is an intuition that it should. We could implement a Turing machine with water wheels, levers & pulleys, vacumn tubes, or transistors. The speed with which the resulting machine computes will be very different, but they will all perform the same computation. Do we think that a model of mind that is blind to time scale can possibly be right?
   
3. Discrete or continuous. Turing machines are *discrete*, *finite* state machines. Time and thought operate in continuous time (don't they)?. Are discrete models that move forward in time in discontinuous steps capable of modeling us who live in and think in the world of continuous time?

4. Computations might model something without explaining it. Weather simulators predict rain, but they don't themselves actually rain. Flight simulators do not fly. Even if a computer program simulated a mind it does not mean it would be thinking. Does the simulations, explanation or demonstration distinction bother you?

** Some Non-computational theories of mind
*** Logical Behaviorism
    Mental states are predispositions to behave. There is no internal state corresponding to belief that is mental. Belief is only a predisposition to behave in a certain way in a certain context.  We characterize people by what they are likely to do without ascribing to them associated mental states. The person who first developed this idea, Gilbert Ryle, asserts that being a mentalist is incompatible with being a realist (that is it makes you a dualist). Logical behaviorism  does not seem to be much in vogue now, but it is another take on the important issues [[cite:&schuette_behav_logic]].

*** Type-Identity Theory
Mental states are just brain states.

Since our brains are different from time to time (synaptic weights change; cells die (and a few born)) does that mean we never have the same mental state twice?

Since no two people have the same brain does that mean no two people ever have the same mental states?

If you feel this is true, but that the differences are trivial, how do you decide where to draw the line?

If you program a computational neuroscience model what are you modeling since each mental state is going to be distinct of each brain state?

Does this bind you to a particular take on the question of /multiple realizability/? 


* Alternatives to the Turing Machine

** Models of Computation


** Lambda Calculus
The lambda calculus was developed as a theory of functions, but it has turned into a favorite tool of functional programmers and theoretical computer scientists to describe their languges, and even as a basis for developing their programming languages. John McCarthy invented Lisp as a theoretical exercise for working on the theory of computable functions. He felt the Turing machine to be too mechanical and too awkward for this work, and wanted a better tool, a better metaphor. He adopted the lambda of the lambda calculus and the ~eval~ function to take in lisp programs and execute them. Late one of his [[https://en.wikipedia.org/wiki/Steve_Russell_(computer_scientist)][collaborators]] observed that it was relatively straightforward to implement this as a real programming language. A bit more of the history is [[https://lwn.net/Articles/778550/][here]]. But in order to try and learn a bit of the lambda calculus I will make reference to a concise summary [[cite:see Chap 2 in &michaelson89_lambd]] and then give us some exercises we can play with.

The untyped lambda calculus was followed by the typed lambda calculus, which is directly related to programming language theory for all the contemporary statically typed languages such as Haskell and OCAML.

*** If you feel that ...                                   :class_discussion:
human brains (and not just human beings) are computational then these abstract formal descriptions might provide the right language for expresssing your truths about thinking. But is it necessary that you know about them or care about them to actually do cognitive or neuroscience modeling?

*** Some Lambda Calculus details from cite:stark90_mathem_found_lisp

**** Why a "lambda" (λ)?
The λ of lambda calculus doesn't really mean anything. It just signals that you have a lambda expression. It creation as the symbol was an accident of notation and the limits of older typewriters.

**** There is not just one lambda calculus
To have the lambda calculus you need to specify your algebra. What are the rules for reducing things (i.e. your computations), what are the allowed symbols, and what do things mean? A "lambda calculus" is a way of handling "lambda expressions."

**** Some of the details

*Terms* are either simple variables ($x$ or $y$) or composite terms ($\lambda~v~t_1$). Having two terms next to each other ($(~t_1~t_2)$) means "apply" $t_1$ to $t_2$. The meaning of a term like $\lambda~v~t_1$ is value returned by lambda abstraction. The meaning part is described by formulas such as $t_1~\rightarrow~t_2$.

That is the tl;dr version, but there are essential details for actually using it.

Some "axioms" you supply (or that Alonzo Church did).

- Beta reduction

- Beta abstraction

- Alpha conversion

- Eta reduction
  
- Normal Form

The equivalent of the halting problem for the Turing machine is the reaching of a /normal form/ in the  lambda calculus.

Functions have the form λ <name> . <body>

Note the "dot". This separates the name from the body of expressions that it names.

<body> is also an expression (note the recursion that is built in).

I am worried about this being too dry, so I figure it may be better to give you the online texts [[cite:&stark90_mathem_found_lisp;&michaelson89_lambd]] and then we can tackle some simple examples as exercises.

***** Classroom Exercises and Discussion
   :answer:
   Identity function: $$\lambda~x.x$$

   
:END:

1. Write the lambda expression for the identity function? What is the identity function?
2. Apply the identity function to itself.
3. What is the identity function in the programming language of your group?
   #+Caption: An example in Common Lisp
   #+begin_src lisp :results value :exports both
     (identity 10)
   #+end_src
   
   #+RESULTS:
   : 10
   
   #+Caption: Writing it as a lambda
   #+begin_src lisp :exports code :results value
     (funcall (lambda (x) x) 10)
   #+end_src
   
   #+RESULTS:
   : 10
4. An interesting lambda expression is the so-called /self-application/ expression: $\lambda~s . (s~s)$. Just do these with pencil and paper (or on the Zoom whiteboard). 
   a. apply this to the identity.
   b. apply the identity to the self-application
   c. apply the self-application to itself. What is its termination status?


***** Write your own homework
The idea here is to figure out a good, concise, tractable practical project we can do to use the different computing languages to solidify our knowledge of how formal computing theory informs (or maybe doesn't inform) practical attempts to implement cognitive models that implicitly adhere to a computation theory of mind.

Maybe this exercise on conditionals would be good? Use the lambdas to compute some simple truth tables?
https://www.diderot.one/courses/56/books/275/chapter/3210

#+begin_src lisp
  (defparameter myt (lambda (x) (lambda (y) x)))
  (defparameter myf (lambda (x) (lambda (y) y)))
  
  (defun mytfun (x1 y1) ((lambda (x) (lambda (y) x) x1) y1))
  (defun myffun (x1 y1) ((lambda (x) (lambda (y) x) y1) x1))
  
  (and (mytfun 1 0) (myffun 1 0))
  (or  (mytfun 1 0) (myffun 1 0))
  
  ; write xor with these primitive functions?
#+end_src   

#+RESULTS:
: 1



   
** Recursive Function Theory (Kleene)
Something for another day. 


** Non-computable
:PROPERTIES:
:CUSTOM_ID: sec:noncomputable
:END:
 
*** The Busy Beaver Problem is non-computable
    The busy beaver problem is to compute the maximum number of 1's that a Turing machine can write before halting with the number of states equal to n. This [[https://jeremykun.com/2012/02/08/busy-beavers-and-the-quest-for-big-numbers/][webpage]] includes the proof of the non-computability of the busy beaver problem. It uses contradiction, and like most proof relying on contradiction I find it head warping, but there it is. 

* Companion and Optional Readings
  1. /An Introduction to Functional Programming Through the Lambda Calculus/ [[cite:&michaelson89_lambd]]
  2. /Functionalism/ in SEP [[cite:&levin21_funct]]
  3. The original beaver paper as a proof of a simple non-computable function [[https://pdos.csail.mit.edu/~rsc/rado62beaver.pdf][(pdf)]].
  4. Comprehensive [[https://www.cs.virginia.edu/~njb2b/cstheory/s2020/files/slides/church-turing-thesis.pdf][on line slides]] (pdf) from a course on the theory of computing.
  5. A detailed and more traditionally written up [[http://www.people.cs.uchicago.edu/~soare/History/handbook.pdf][history of the concept of computability (pdf)]]. It is over 30 pages long, but readable. It does deal with technical material, but is mostly prose, and tries to tie it to history, usage, and contemporary interpretations. A really good read if you have the time and interest. 
  6. An accessible [[http://bach.ai/lambda-calculus-for-absolute-dummies/][blog]] article called _Lambda Calculus for Absolute Dummies_
  7. Another nice online [[http://cs.rpi.edu/academics/courses/spring10/proglang/handouts/LambdaCalculus.pdf][resource]] (pdf)
* References
[[bibliography:/home/britt/gitRepos/masterBib/bayatt.bib]]

* A bit of detail about lambda calculus notation

λ expressions are either a "name" | "function" | "application"

Names name expressions, functions introduce an abstraction, applications specialize abstractions.

Names are sequences of characters.

Functions have the form λ <name> . <body>

Note the "dot". This separates the name from the body of expressions that it names.

<body> is also an expression (note the recursion that is built in).

/application/ has the form of <function expression> <argument expression>

Note that both are expressions (everything is an expression). They are simply placed in proximity. An example $\lambda~x.x\hspace{1.5em}\lambda~a.\lambda~b.b$ . Interesting to note that functions can be arguments too. The intent here is that application provides an expression for the name.


@(generate-bibliography)
