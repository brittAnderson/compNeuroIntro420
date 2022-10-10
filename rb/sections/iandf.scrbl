#lang scribble/book

@(require plot/pict 
	  scribble/base
	  scribble-math/dollar
	  scribble/example
          scribble/manual
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
One of the first demonstrations of the all-or-none nature of the neuronal action potential was made by Lord Adrian. Lord Adrian was an interesting scientific figure how asserted that some of his prowess at electrophysiology stemmed from his interest in and training in fencing.

To answer the questions:
@itemlist[@item{When was the action potential demonstrated?}
               @item{What was the experimental animal used by Adrian?}]
Consult this @hyperlink["https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1420429/pdf/jphysiol01990-0084.pdf"]{this pdf for answers.}                                                                       

@margin-note{Want more details? There is an excellent free book available @hyperlink["https://lcnwww.epfl.ch/gerstner/SPNM/SPNM.html"]{@bold{Spiking Neurons}.} They also have another more modern book out too @hyperlink["http://neuronaldynamics.epfl.ch/online/index.html"]{@bold{Neuronal Dynamics}.}}

@section[#:tag "sec:iandf"]{The Integrate and Fire Equation}

While Hodgkin and Huxley provided the first robust computational model of the neuronal action potential there model is quite complex, as we will soon see. Is all that complexity necessary? That of course depends on the nature of the scientific question, and if we are primarily intersted in whether a spike has or has not occured, and not the ionic events that produced the spike, we may find our experimental questions dealt with much more concisely by a much simpler model of neuronal spiking: the leaky integrate and fire model.

The formula for the leaky integrate and fire neuron is:
@(use-mathjax)

@elemtag{iandf-eq}
@$$["\\tau \\frac{dV(t)}{dt} = -V(t) + R~I(t) ."]{\tag{I}}
@(linebreak)

In the next sections we will describe how this simplification came to be, and use it as the basis for learning some of the elementary electrical laws and relations upon which it is based. 

@subsection{Electronics Background}
The following questions are the ones we need answers to to derive our integrte and fire model.

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
To understand our formula clearly we should review the meaning of the key symbols and notation.

@itemlist[#:style 'ordered @item{What does @($ "\\frac{dV}{dt}") mean?}
          @item{What does @($ "\\frac{1}{\\tau}") mean?}
          @item{Why does the voltage term on the right have a negative sign?}
          @item{What is @($ "I(t)")?}]


@; :QUESTION: It is the derivative. It is the how the voltage changes as a function of how time changes.
@; :Question: It is the membrane time constant and can be related to the membrane capacitance. Since it is a constant, with a clever choice of units you can assume it to be one and make it disappear.
@; :QUESTION: To get the intuition of a model you don't always have to compute things. You can also get some qualitative behaviour just by looking at it. The larger the voltage the more negative becomes its rate of change and vice versa. It drives everything back to some point at which the rate of change to an equilibrium point. We will come back to this notion of a fixed point or attractor.
@; :QUESTION: It is the current term. $I$ is the common abbreviation for current. Why? I don't know, can someone help?

To derive our equation we need to put all these fact together. 
          
We recall that just like we used the derivative to help us figure out where the spring would be some small increment of time into the future, we use the same approach to compute our future voltage. That future voltage will also include a term that reflects an additional current that we have "experimentally" injected. 

@margin-note{Can you tell why, looking at the integrate and fire equation, if we don't reach the firing threshold, we see an exponential decay?}  

Deriving the IandF Equation
@($$ "\\begin{align*}
     I &= I_R + I_C & (a) \\\\
       &= I_R + C\\frac{dV}{dt} & (b)\\\\
       &= \\frac{V}{R} + C\\frac{dV}{dt} & (c)\\\\
     RI  &= V + RC\\frac{dV}{dt} & (d) \\\\
     \\frac{1}{\\tau} (RI-V)  &= \\frac{dV}{dt} & (e)\\\\
     \\end{align*}")

@itemlist[@item{a: Kirchoff's point rule,}
               @item{b: the relationship between current, charge, and their derivatives} 
               @item{c: Ohm's law}
               @item{d:multiply through by R}
               @item{e: rearrange and define @($ "\\tau")}]

@subsection{Coding up the Integrate and Fire Neuron}
Most of the integrate and fire implementation is conceptually and practically identical to the spring example. You assume a starting voltate (initial state) and then you update that voltage using the differential equation for how voltage changes with time (@($ "\\frac{dV}{dt}")).

There is one critical difference though. Unlike real neurons the Integrate and Fire neuron model does not have a natural threshold and spiking behavior. You pick a threshold and everyone time your voltage reaches that threshold you designate a spike and reset the voltage.

What I added below is a strictly cosmetic amendment that changes the first value after the threshold to a number much higher than the threshold so that when plotted it creates the visual appearance of a spike.

@subsubsection{Class Exercise: Adding a refractory period to the Integrate and Fire model}
@margin-note{What is the refractory period for a neuron?}

In class make sure you can get the @hyperlink["./../code/iandf.rkt"]{integrate and fire model} working in Dr. Racket. After you get the basic model working trying altering the input current to see how that affects the number of spikes and the regularity of their spiking.

Next, change the form of the input current to be something other than a constant. I suggest trying a sine wave. This will give you a chance to sample some of racket's potential.

Find out how to take the sin of a number. Then learn how to @tt{map} the sin function over a list of numbers. If you use @tt{in-range} you can create a stream of numbers from a minimum to a maximun for a given step size. Then you may want to shift up or scale all the numbers to make them non-zero. This could be done by mapping again. The @tt{map} function is very powerful and allows you to avoid writing a lot of lengthy looping code.

After you have done that edit the code to include a refractory period. First, decide on the logic of how to do this and only after that start editing the code to implement it.



The next examples walk through the code and describe some of the ideas.

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
This is sometimes referred to as @hyperlink["https://en.wikipedia.org/wiki/Euler_method"]{Euler's method}.


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
          
Just as we were given the equation for a spring, here we are given the equation for the I&F neuron, which we translate from math to code. In addition, I created some smaller "helper" functions. I like a style that gives my functions default values. Then I don't have to enter so many arguments when I call the function. This, of course, only makes sense if there are values which you input to your function and that rarely change. I also find it convenient to use a style where I have keywords for my functions. Then I can change the order that I enter things. It does make my code longer, because I have to type the keywords when specifying the input to my functions. This is what I am doing with the lines that look like: @code[#:lang "racket"]|{#:if-false [if-false 0.0]}|

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


Though things may look more complex than the spring example, it is because I have so many more @italic{local} variables to define.
The basic flow is still just a @bold{loop}.

Visualizations can be essential in helping you to see and understand the function of your computational program. Thus, while it is only cosmetic, I find the addition of the apparent spike helps me to see what the output of my simulation is. In another context, e.g. if were only counting spikes this decorative element would be un-needed complexity. 

@section{Homework}

The Integrate and Fire homework has two components. One practical and one theoretical.

Practically, submit an integrate and fire racket program that alters mine in some meaningful way. You might change the plot or the type of current input. You might examine how the results depends on the size of the time step used. Just something to show that you can edit code and keep it working.

Theoretically, look at @hyperlink["https://redwood.berkeley.edu/wp-content/uploads/2018/08/mainen-sejnowski.pdf"]{this article (pdf)} and tell me how you feel our integrate and fire model compares to these actual real world spiking data when both are give constant input. What are the implications for using the integrate and fire model as a model of neuronal function? 



