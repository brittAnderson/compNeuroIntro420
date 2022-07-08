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


@section{Using Derivatives to Solve Problems With a Computer}

@subsection{What is the square root of 53?}

We want to know the value of @($ "x") that makes @($ "53 =x^2") true?

@margin-note*{Always use the computer for the busy work when you can. Your computer can solve many mathematical problems for you. For example, the derivative of @($ "x^2") is 
@($ (latex (simplify (differentiate (parse-infix "x^2")))))}

@itemlist[@item{Come up with a guess.}
               @item{Calculate the error.}
               @item{Adjust your guess based on the error.}
               @item{This adjustment will use the derivative.}]
IAMHERE



@; **** Analytical Solutions
@;      Let software be your guide.
@;      1. [[https://www.sympy.org/en/index.html][Sympy]]
@;      2. [[https://www.cfm.brown.edu/people/dobrush/am33/SymPy/index.html][Sympy for applied DEs]]
@;      3. [[https://cran.r-project.org/web/packages/Deriv/Deriv.pdf][R for Derivatives]]

@; #+Name: Derivatives with a Computer Algebra System: Maxima
@; #+Caption: What is the derivative of x squared?
@; #+BEGIN_SRC maxima
@; f(x) := x^2;
@; tex(diff (f(x), x));
@; #+END_SRC

@; #+RESULTS: Derivatives with a Computer Algebra System: Maxima
@; | $$2\ | x$$ |

@; NB: As I am trying to learn a new computer language I tried a new tool in that language to help with learning. [[https://maxima.sourceforge.io/][Maxima]] is a powerful computer algebra system. You do *not* have to know or use Lisp to use maxima, but it is written in Lisp and can be called from Lisp. 

@; #+Caption: Derivative of $x^2$
@; $$2\,x$$


@; **** Working Through an Example

@; ***** When in doubt, guess
@; - How much is your guess off?

@;  $\mbox{Error} = \mbox{(my guess)}^2 - \mbox{128}$

@; - How much does the error change when the guess changes?
@;   This is a derivative. A *rate of change*.

@;   $\frac{\Delta~\mbox{Error}}{\Delta~\mbox{Guess}} = \frac{\mbox{Error(Guess1) - Error(Guess0)}}{\mbox{Guess1} - \mbox{Guess0}}$

@;   As the guess gets very small this will approach the definition of the derivative.

@; We have a function for how to compute the result of our guess and we can get the derivative of that either by hand or by using a computer algebra system.

@; $$\frac{\mbox{dError}}{\mbox{dGuess}} = \frac{\mbox{f(Guess1)} - \mbox{goal} - \mbox{f(Guess0)} + \mbox{goal}}{\mbox{Guess1} - \mbox{Guess0}}$$

  
@; - Using these relations can we come up with a formula for how much we need to adjust our guess based on how big the error was?
   
  
@; #+BEGIN_SRC python :session: *de-intro* :results file graphics replace :exports both :file  "./images/sqrt55.png"
@; #Error Plot
@; #What is square root of 55?
@; plotData = [(x**2-55,x) for x in np.arange(5.0,8.5,0.01)]
@; plt.clf()
@; plt.plot([y[1] for y in plotData],[x[0] for x in plotData],)
@; plt.plot([5.0,8.5],[0,0],'r-')
@; plt.plot([np.sqrt(55),np.sqrt(55)],[12,-30],'r-')
@; plt.plot([5.0,8.0],[-30,0],'k--')
@; plt.plot([8.0,8.0],[-5,15],'g--')
@; plt.plot([8.0,8.0-(9.0/16)] , [9.0,0.0],'b')
@; plt.savefig("./images/sqrt55.png")
@; #+END_SRC

@; #+Caption: Slopes for Curves
@; #+RESULTS:
@; [[file:./images/sqrt55.png]]


@; #+BEGIN_SRC python :session: *de-intro* :results file graphics replace :exports both :file "./images/sqrt-more.png"

@; plotData = [(x**2-55,x) for x in np.arange(7.3,7.6,0.01)]
@; plt.clf()
@; plt.plot([y[1] for y in plotData],[x[0] for x in plotData],)
@; plt.plot([7.0,7.5],[0,0],'r-')
@; plt.plot([np.sqrt(55),np.sqrt(55)],[3,-3],'r-')
@; xs = [np.sqrt(55)-1/4,np.sqrt(55)+1/4]
@; plt.plot(xs , [16*x-119 for x in xs],'b')
@; plt.savefig("./images/sqrt-more.png")
@; #+END_SRC

@; #+RESULTS:
@; [[file:./images/sqrt-more.png]]

@; **** Finding Cube Roots :class_exercise:
@;      :PROPERTIES:
@;      :CUSTOM_ID: finding-cube-roots
@;      :END:

@; 1. What is a /cube root/?

@; 2. What is the derivative of $x^3$?

@; 3. Find it with a computer algebra system *even if you know how to do it by hand.*
@;    A simple way to make sure you are on the right track with a new programming tool is to do something in code that you can do in your head or on paper to make sure that you get the correct answer. It does not substitute for [[https://realpython.com/python-testing/][proper testing]], but it is an easy to do start. You don't want to waste time trying to perfect a flawed approach.

@; 4. Write two Python Functions: one to =return= the cube of a number, and
@;    one to =return= the derivative when evaluated at a particular value
@;    of =x=.

@; #+BEGIN_SRC maxima
@; f(x) := x^3;
@; df:diff(f(x), x);
@; tex(df);
@; newline();
@; print("The derivative of x^3 at 3 is: ",ev(df,x=3));
@; #+END_SRC

@; #+RESULTS:
@; | $$3\,x^2$$ |            |    |     |    |   |     |    |
@; | The        | derivative | of | x^3 | at | 3 | is: | 27 |

@;   $$3\,x^2$$
@;   The derivative of x^3 at 3 is:  27 



@; #+BEGIN_SRC lisp
@; (setq *read-default-float-format* 'DOUBLE-FLOAT)
@; (defvar *goal* 128.0)
@; (defvar *initial-guess* 5.0)
@; (defun x-cubed (x) (* x x x))
@; (defun diff-x-cubed (x) (* 3 x x))
@; #+END_SRC

@; #+RESULTS:
@; : DIFF-X-CUBED

@; #+BEGIN_SRC lisp
@; (defun get-step (guess &optional (goal *goal*)) (/ (- goal (x-cubed guess)) (diff-x-cubed guess)))
@; #+END_SRC

@; #+RESULTS:
@; : GET-STEP

@; NB: I am using optional arguments so that when I use my functions I don't have to keep typing in common things that rarely change. You can do this in [[https://realpython.com/python-optional-arguments/][python]] and [[https://www.oreilly.com/library/view/the-r-book/9780470510247/ch002-sec060.html][R]] as well.

@; #+BEGIN_SRC lisp
@; (defun get-cube-root (goal initial-guess &optional (tolerance 0.001))
@;   (loop
@;     for error = (get-step initial-guess goal) then (get-step new-guess goal)
@;     for new-guess = (+ initial-guess error) then (+ error new-guess)
@;     while ( > (abs ( - (x-cubed new-guess) goal )) tolerance )
@;     do (format t "new-guess is ~,15f~%" new-guess)
@;     finally (return new-guess)))
@; #+END_SRC

@; #+RESULTS:
@; : GET-CUBE-ROOT

@; NB: My solution uses a *loop*. To emulate this approach you will need to discover how to write loops in your chosen language. The two main kinds of loops are =for= loops and =while= loops. Which one have I used above?

@; #+BEGIN_SRC lisp
@; @; let's try it out
@; (get-cube-root 128 5.0)
@; #+END_SRC

@; #+RESULTS:
@; : 5.039684219366759



@; ** Homework :homework:
@;    Submit your program for computing the fourth root of a number. That is the number that when multiplied together four times equals the goal. For example $2\times 2 \times 2 \times 2 ~ = 16$. If I gave you 16, your program ought to put out something close to 2. You should need to make very few changes to what you have already written to have a working program.

@;    If this comes easy to you consider doing this *bonus* assignment. Have your program work for any root. That is I can give it goal and 'n' and it will calculate the $n^{th}$ root. For example ~(nth-root 16 2)~ gives me four, but ~(nth-root 16 4)~ gives me two.
   

@; * Practice Simulating With DEs

@; ** Frictionless Springs

@; Motivation: I know we want to code neurons, but to get there we should feel comfortable with the underlying tool or we won't be able to adapt it or re-use it for some new purpose. I don't want to give you a fish. I want to teach you how to fish.

@; By working with an example simpler than a neuron, and one for which you might have more intuition, such as a simple spring or "slinky" I hope you will get a better /feel/ for how the numbers, equations, and code all relate. Then we can move on to the neuronal application.

@; ***  The equation of a frictionless spring?

@; \begin{equation}
@; \label{eq:1}
@; \frac{d^2 s}{dt^2} = -P~s
@; \end{equation}


@; What does it mean? What is multiplying what here? What do the parentheses mean, and how do you know what they mean? 

@; **** How do we finesse the derivatives?
@; Use definitions [[#derivative]].

@; Imagine a little time has gone by ($\Delta~t$). What is our new position ($s$ is for space)?

@; $$\frac{s(t + \Delta t) - s(t)}{\Delta t} = velocity \approx \frac{d s}{d t}$$.

@; What is our initial velocity? Assume zero.

@; How will our velocity change with time?

@; $$\frac{v(t + \Delta t) - v(t)}{\Delta t} = acceleration \approx \frac{d v}{d t} = \frac{d^2 s}{d t^2}$$

@; And we have a formula for this. We can now bootstrap our simulation.

@; #+Name: Basic Spring Functions
@; #+BEGIN_SRC lisp
@; (defun s-of-t (delta-t v s)
@;   (+ s (* v delta-t)))

@; (defun v-of-t (delta-t a v)
@;   (+ v (* a delta-t)))

@; (defun a-of-t (p s)
@;   (* -1 p s))
@; #+END_SRC

@; #+RESULTS: Basic Spring Functions
@; : A-OF-T


@; Note the similiarity of the two functions. You could write a helper function that was generic to this pattern of old value + rate of change times the  time step, and just used the pertinent values. 

@; How do we know the formula for acceleration? We were given it \ref{eq:1}. 

@; #+Name: constants and variables
@; #+BEGIN_SRC lisp
@; @; generating initial values and variables
@; (defconstant +init-v+ 0
@;   "The initial Velocity")
@; (defconstant +init-s+ 10)
@; (defconstant +p+ 2)
@; (defconstant +delta-t+ 0.05)
@; #+END_SRC

@; #+RESULTS: constants and variables
@; : +DELTA-T+

@; NB: you should consider doing something similar with your code. Put all the numbers and values you don't expect to change very much in a block at the top of your code. Then refer to them by their variable names later on. This is much more convenient if you want to change or update something. You have one spot to look to find the value to change. Changing it in one place propagates through all the rest of your code. 

@; NB: I give an example of a "doc string". Most programming languages have a way of adding documentation. It is much nicer to add this as you write your code than to go in at the end and try to do it all at once. Try to use short simple, but *meaningful*, variable names and give longer explanations as documentation.

@; #+Name: looping
@; #+BEGIN_SRC lisp
@; (defun release-spring (&optional (repeat-n 5))
@;   (loop
@;     repeat repeat-n
@;     for a = (a-of-t +p+ +init-s+)         then (a-of-t +p+ s)
@;     for v = +init-v+                      then (v-of-t +delta-t+ a v)
@;     for s = +init-s+                      then (s-of-t +delta-t+ v s)
@;     for time = 0                          then (+ time +delta-t+)
@;     collect (list a v s time)))
@; #+END_SRC

@; #+RESULTS: looping
@; : RELEASE-SPRING

@; NB: The code above creates a series of steps that happen one after the other. This is *not* a nested loop, though it will look similar to the nesting of loops you can see in python and R. This is just a common lisp idiosyncrasy. You will again need to use a loop. This time I am using what kind of loop? The ~collect~ statement is a short hand for gathering all my values up and returing them. You will probably need something like a =return= statement or a variable that you use to catch the updated values and append them on to a growning list. 

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

