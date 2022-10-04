#lang scribble/book

@(require plot/pict 
	  scribble/base
	  scribble-math/dollar
	  scribble/example
          symalg
          scriblib/figure)

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


@title{Integrate and Fire Neuron}

In this section we take a look at the history and math of the computational model of neuron firing called "Integrate and Fire" (I&F).
 The I&F model uses math essentially the same as the spring example.

@margin-note{Is the integrate and fire model used much in modeling in the present time.? @hyperlink["https://scholar.google.com/scholar?as_ylo=2020&q=%22integrate+and+fire%22+neuron&hl=en&as_sdt=7,39"]{Answer}.}

@section{History of the Integrate and Fire Model}
@subsection{Louis Lapicque - Earlier Computational Neuroscientist}

@hyperlink["https://link.springer.com/content/pdf/10.1007/s00422-007-0190-0.pdf"]{Modern Commentary on Lapique's Neuron Model}

@; *** Image of the Laboratory
@figure[
  "fig:lapique-lab"
  @elem{The Lapique Lab at the Sorbonne 190something}
  @elem{@image[#:scale 0.5]{./images/Lapicque_laboratoire.png}}]

@; haven't figure out how to scale the image. 

@hyperlink["http://www.snv.jussieu.fr/brette/papers/Lap07.pdf"]{Original Lapique Paper (scanned; pdf)}
@hyperlink["https://fr.wikipedia.org/wiki/Louis_Lapicque"]{Brief Biographical Details of Lapicque}.

@subsection{Lord Adrian and the All-or-None Action Potential}
@margin-note{When was the Action Potential Demonstrated? What was the experimental animal used by Adrian? @hyperlink["https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1420429/pdf/jphysiol01990-0084.pdf"]{Answer (pdf).}}

@margin-note{Want more details? There is an excellent free book available @hyperlink["https://lcnwww.epfl.ch/gerstner/SPNM/SPNM.html"]{@bold{Spiking Neurons}.} They also have another more modern book out too @hyperlink["http://neuronaldynamics.epfl.ch/online/index.html"]{@bold{Neuronal Dynamics}.}}

@section[#:tag "sec:iandf"]{The Integrate and Fire Equation}
@(use-mathjax)
@$$["\\tau \\frac{dV(t)}{dt} = -V(t) + R~I(t)"]{\tag{1}}

@seclink["sec:iandf"]

@section{Electronics Background}
@itemlist[#:style 'ordered @item{What is Ohm's Law?}
          @item{What is Kirchoff's Point Rule?}
          @item{What is Capacitance?}
          @item{What is the relation between current and capacitance?}]

@;  *Ohm's Law* (empirically observed): $V = IR$
@; *** Explain what is the relationship between current and charge?
@; :QUESTION: Current: The derivative of charge with respect to time, $$I = \frac{dQ}{dt}$$
@; *** Explain *Kirchoff's Point Rule*
@; :QUESTION: Current sums to zero: All the current sources going to a node in a circuit must sum to zero.
@; *** What is capacitance?
@; :QUESTION: Capacitance is a source of current. A capacitor is a sandwich of two conducting surfaces with a non-conducting body in between. If you a charge to one side, the electrons gather there. They can't leap the gap, so they exert an attraction for particles of the opposite charge on the other side of the gap. If you suddenly stop the charge then charge races around and you discharge a current.
@; :END:
@; *** Explain the relationship, mathematically, between capacitance, charge, and voltage.
@; :PROPERTIES:
@; :QUESTION: $C = Q/V.$ The volume of charge, per unit area, divided by the voltage that produces this imbalance in charge.
@; :END:
@; *** What happens when you differentiate this equation with respect to time and treat the capacitance as a constant?
@; :PROPERTIES:
@; :QUESTION: $C \frac{dV}{dt} = \frac{dQ}{dt} = I$
@; :END:

@subsection{Formula Discussion Questions}
@itemlist[#:style 'ordered @item{What does @($ "\\frac{dV}{dt}") mean?}
          @item{What does @($ "\\frac{1}{\\tau}") mean?}
          @item{Why does the voltage term on the right have a negative sign?}
          @item{What is @($ "I(t)")?}]


@; :QUESTION: It is the derivative. It is the how the voltage changes as a function of how time changes.
@; :Question: It is the membrane time constant and can be related to the membrane capacitance. Since it is a constant, with a clever choice of units you can assume it to be one and make it disappear.
@; :QUESTION: To get the intuition of a model you don't always have to compute things. You can also get some qualitative behaviour just by looking at it. The larger the voltage the more negative becomes its rate of change and vice versa. It drives everything back to some point at which the rate of change to an equilibrium point. We will come back to this notion of a fixed point or attractor.
@; :QUESTION: It is the current term. $I$ is the common abbreviation for current. Why? I don't know, can someone help?

          
Put it all together

The voltage in the future will be a sum of whatever current is being added minus a function of the current voltage.

Why, if we don't reach a threshold to fire an action potential, do we see an exponential decay?  

This is where you need to remember what came before. See that the change in voltage is proportional to itself. Solutions to these types of equations involve exponentials. Remember? But note the sign. It is negative. That is why it is an exponential decay instead of an exponential growth.


Deriving the IandF Equation
@($$ "\\begin{align*}
     I &= I_R + I_C \\\\
       &= I_R + C\\frac{dV}{dt} \\\\
       &= \\frac{V}{R} + C\\frac{dV}{dt}\\\\
     RI  &= V + RC\\frac{dV}{dt} \\\\
     \\frac{1}{\\tau}(RI-V)  &= \\frac{dV}{dt}\\\\
     \\end{align*}")

\subsection{Coding up the Integrate and Fire Neuron}
You will have as your main homework for this week to write a functioning version of this.
You can use my code as an example of what you are trying to implement if you get stuck on your own.
@examples[#:no-prompt
          #:label @bold{@italic{Defining our parameters:}}
(define dt 0.05)
(define max-t 10)
(define init-t 0.0)
(define start-time 1.0)
(define stop-time 6.0)
(define cap 1)
(define res 2)
(define threshold 3.0)
(define spike-display 8.0)
(define init-v 0.0)
(define voltage init-v)
(define injection-current 4.3)
(define injection-time (cons start-time stop-time))
(define tau (* res cap))]

This is a good habit to develop with your code.
Do not "hard code" in values for variables that you will have to write in multiple locations in a file.
It makes it hard to update and debug your code.
Give sensible and short names to things you will use in your code.
Then define values for those at the top of your code.
This gives you one place to look for explanations and reminders, and also gives you a place where when you make a single change it will propagate through your code.

@examples[#:no-prompt
          #:label @bold{@italic{Euler's Method (again):}}
          (define (update old-value rate-of-change time-step)
            (+ (* rate-of-change time-step) old-value))]

This is the same updating rule that we used in the spring example.
It is a rewriting of the definition of the derivative.
This is sometimes referred to as [[https://en.wikipedia.org/wiki/Euler_method][Euler's method]].


@examples[#:no-prompt
          #:label @bold{@italic{Helper functions:}}
          (define (dv-dt localres locali localv)
            (* (/ 1 tau) (- (* localres locali) localv)))


          (define (between x #:lower [lower (car injection-time)]
                           #:upper [upper (cdr injection-time)]
                           #:if-true [if-true injection-current]
                           #:if-false [if-false 0.0])
            (if (and (>= x lower) (<= x upper)) if-true if-false))

          (define (voltage-choice curr-volt spike-status #:thr [thr threshold]
                                  #:sd [sd spike-display])
            (cond
              ((and (> curr-volt thr) (not spike-status)) sd)
              (spike-status 0.0d0)
              (#t curr-volt)))]
          
Just as we were given the equation for a spring, here we are given the equation for the I&F neuron, which we translate from math to code.
These are being defined as functions.
You can do the same in both Python and R, but will need a different keyword and syntax.

In addition, I create some smaller "helper" functions.
It would be possible to collapse all this into one big function, but that would be harder for me to understand, and harder for you to understand.
In general, try to write short little functions that do one thing.
Then you can chain those small functions together to accomplish the larger task.

@examples[#:no-prompt
          #:label @bold{@italic{Running our model:}}
          (define (run-iandf-sim #:tolerance [tolerance 0.1] #:max-time [max-time 10] #:max-iter [max-iter 10000])
            (for*/fold ([t 0]
                        [i 0]
                        [v 0]
                        [accum '()]
                        #:result (reverse accum))
                       ([n (in-range max-iter)])
              #:break (> t max-time)
              (let ([spike (< (abs (- v spike-display)) tolerance)])
                (values (+ dt t)
                        (between t)
                        (voltage-choice (update v
                                                (dv-dt res i v) dt) spike)
                        (cons (list t i v) accum)))))]


@examples[#:eval plot-eval
          (require "./code/iandf.rkt")
          (begin
            (define iandf-results (run-iandf-sim #:max-time 10.0 ))
            (plot (lines (map vector (map first iandf-results) (map third iandf-results)))))]


Again, if you squint, you will see similarities to the Spring exercise.
Though things may look more complex here it is only because I have so many more /local/ variables to define.
The basic flow is still just a *loop*.
Each of those local variables gets a start value and then a rule for updating each time through the loop.
Later local variables can depend on the values of that came earlier in the list (that is the reason for the asterisk in =do*=).
The loop also has a test condition for when to quit (like a "while" loop), and what it should do when that condition is met.
Here it collects all the data into a big long list and reverses the order.
I was pushing the recent values on to the front of the list each time, but now I need to reverse it so that time flows as we expect.


Define variables, and even functions, where you need them.
It might be overkill here, but the idea is a good one to try and get in the habit of.
When you need a function or a variable for only a small part of your program, make them local.
Then they won't interfere with other parts of your program, and after you use them your programming language system can @italic{garbage collect} them freeing up your computer's memory and your namespace.
Local variables, local functions, and even un-named, so-called @italic{lambda} functions, can make your code easier to read and understand as things are defined where they are needed and used.
Defining local variables and functions does not require special keywords in Python and R, but can be inferred from the code itself.

Visualizations can be essential in helping you to see and understand the function of your computational program.
Gain a good familiarity and facility with the plotting functions of whatever programming language you plan to use.


In this function we can give a name to our plot and feed in the data it will use. In fact, I did not have to create and save the data. I was able to generate it internal to the function itself. This is sometimes thought of as function /composition/. You will also hear people talk of /chaining/ functions or /piping/. Think of how you can connect a series of pipes together to get a flow from beginning to end. In the case of a programming language each of the pipes may do something to what it is carrying and the result can be a processed data stream. 




* Homework
@itemlist[#:style 'ordered @item{This weeks homework is to write a I&F program that does what I just did. It should generate a "spike" when given a constant input. Be sure to look at my code to see how that spike is created. If you don't understand that you will have a hard time.}
          @item{Related to the last point, does the I&F neuron truly spike?}
          @item{If (1) goes easy then here are some other things to try:
                   @itemlist[@item{create a refractory period for your neuron.}
                                  @item{give a noisy input rather than the single flat line I demonstrate.}
                                  @item{Look at how many times your neuron spikes to constant input. Is that what a real neuron does (try searching for Mainin and Sejnowski)? Does that affect the utility of the I&F model for computational neuroscience?}
                                  @item{Lastly, if all that goes quickly, and it will for some of you, but not most of you, try creating variations of this simple I&F model.}]}]


Upload your code to LEARN for credit.     



