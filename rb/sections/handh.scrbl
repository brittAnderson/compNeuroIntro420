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


@title{Hodgkin and Huxley Model of the Action Potential}
@section{Background}

Hodgkin and Huxley, the people as well as their model, provide a nice example for how to structure one's education to do this sort of work, as well as providing an exemplary example of doing the work.
 
One of the lessons is that you need to train broadly.
You may not understand in the beginning what the problem is.
You will need to be prepared for it to appear, and when it does to be able to attack it.
For Hodgkin and Huxley this meant training in both the physical and non-physical sciences.
Each emphasized a different side of that divide, but both made sure they could converse across the divide.
If you want to apply computational tools to social science problems then you will need to make sure your course work and your practical skills bridge that divide too.
 
@subsection{Biographical Sources}
The Nobel Prize organization keeps biographies of all recipients @hyperlink["https://www.nobelprize.org/prizes/medicine/1963/hodgkin/biographical/"]{Hodgkin},  @hyperlink["https://www.nobelprize.org/prizes/medicine/1963/huxley/biographical/"]{Huxley}.

 This @hyperlink["https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3424716/pdf/tjp0590-2571.pdf"]{article (pdf)} is a nice summary of the work. You might look for how long it took Huxley to calculate his simulation of one action potential numerically using the method, basically, that we will be using, and compare it to how long it takes you.
 
@subsection{Model Description (detailed)}
Gersnter's @hyperlink["https://lcnwww.epfl.ch/gerstner/SPNM/node14.html#table-HH1"]{book's chapter} on the HHM goes into more detail than I do. If you have problems getting things to work, or just want a more detailed mathematical explanation this is an excellent resource. 
   
@section{The HHM}
 
Some introductory reminders and admonitions:
 
The current going in to the cell is what we inject.
The current coming out is the sum of the capacitance (due to the lipid bilayer), and the resistance (due to the ion channels).
This is *Kirchoff's* rule.

Recall that in the Integrate and Fire model we lumped all our ionic
events together into one term:
@(use-mathjax)
@($$ "\\tau \\frac{dV(t)}{dt} = -V(t) + R~I(t)")
 
The HHM is basically the same except we have a resistance @italic{for each ion channel} (ask yourself,before reading the next paragraph)  what those ions are or at least how many terms you will need).
The rule for currents in parallel is to apply Kirchoff's and Ohm's laws realizing that they all experience the same voltage, thus the currents sum.

But in the HHM we treat these different ionic components with their own terms to capture the difference between the Sodium (Na), Potassium (K), and negative anions (still lumped as "leak" l).

@$${\sum_i I_R(t) = \bar{g}_{Na} m^3 h (V(t) - E_{Na}) + \bar{g}_{K} n^4 (V(t) - E_{K}) + \bar{g}_{L} (V(t) - E_{L})}

@subsection{Putting it together: The HHM}
@$${I_{tot} = I_r + I_C}

By the same logic as for the integrate and fire @${I_C = c~\frac{dV}{dt}}.

@$${I_{tot} = \bar{g}_{Na} m^3 h (V(t) - E_{Na}) + \bar{g}_{K} n^4 (V(t) - E_{K}) + \bar{g}_{L} (V(t) - E_{L}) + c~\frac{dV}{dt}}

Rearrange to get the @${\frac{dV}{dt}} on a side by itself.

@$$["c~\\frac{dV}{dt} = I_{tot} - (\\bar{g}_{Na} m^3 h (V(t) - E_{Na}) + \\bar{g}_{K} n^4 (V(t) - E_{K}) + \\bar{g}_{L} (V(t) - E_{L}))"]{\tag 1}

@subsubsection{Test your understanding}

You cannot program what you don't understand.
Sometimes people think that the act of programming will bring them understanding.
It doesn't.
It may provoke understanding.
It may be that you did not realize that you did not understand something until you try to code it.
That can be helpful.
But if you are fuzzy on the basics coding will not grant you insight.
Simulating models with code is most useful for helping you see the consequences of something.
If you understand the above model, what its components stand for and represent, then you might wonder what would happen if ...? What would happen if there were five @italic{n} channel components? And so on.
It is not that you do not understand what @italic{n} represents, but that you are unable to do the math in your head.
That is where the computer helps: making concrete the consequences of your well understood system.

So, in that light, and before you start coding, ask yourself,

@itemlist[#:style 'ordered
          @item{What are the @${\bar{g}_*} terms?}
          @item{What are the @${E_{*}} terms?}
          @item{What do m,n, and h represent?}
          @item{Where did these equations come from?}]
   
@subsection{It's Differential Equations All the Way Down}
Although the HHM uses just the same mathematics as the I&F model, and we will use the same Euler's method to step forward and calculate model terms that evolve over time, this model is more complex in two ways. First, it has multiple derivatives and derivatives at multiple levels. Each of the @italic{m}, @italic{n}, and @italic{h} terms are also changing and regulated by a differential equation. They are dependent on voltage. For example, @${\dot{m} = \alpha_m (V)(1 - m) - \beta_m (V) m}.

@bold{More food for thought}
@itemlist[#:style 'ordered
          @item{Each of the m,n, and h terms have their own equation of exactly the same form, but with their unique alphas and betas (that is what the subscript means).}
          @item{What does the V in parentheses mean?}
          @item{When they were finally sequenced (decades later), what do you think was the number of sub-units that the sodium and potassium channels were found to have?}]

@subsection{Getting Started}
You will need to make some assumptions to get your initial conditions. 

@itemlist[@item{If you allow @${t \rightarrow \infty \mbox{, then } \frac{dV}{dt}=?}}
               @item{You assume that it goes to zero; that is, you reach steady state. Then you can solve for some of the constants.}
               @item{Where do the constants come from?}
               @item{They come from experiments, and you use what you are given.}
               @item{Assume the following constants - they are set to assume a resting potential of zero (instead of what and why doesn't this matter)?}
               @item{These constants also work out to enforce a capacitance of 1}]
@subsubsection{Constants}
@tabular[#:sep @hspace[1]
         (list (list @bold["Constant"] @bold["Value"])
               (list "ena"  "115")
               (list "gna"  "120" )
               (list "ek"   "-12" )
               (list "gk"   "36"  )
               (list "el"   "10.6")
               (list "gl"   "0.3" ))]

@bold{WARNING} These constants are adjusted to make the resting potential 0 and the capacitance 1.0. If you want your model to have a biological resting potential you will need to adjust these values, but when you think about it the scale is rather arbitrary. What does water freeze at 0 or -32? Well it depends on the scale: centigrade or fahrenheit. Same for neurons. Why not use a scale that makes the math simpler. Focus on the relative behavior not some absolute, and rather arbitrary, numbers?

@subsection{Alpha and Beta Formulas}

@${\alpha_{n}(V_{m})={\frac {0.01(10-V_{m})}{\exp {\big (}{\frac{10-V_{m}}{10}}{\big )}-1}}}

@${\alpha_{m}(V_{m})={\frac {0.1(25-V_{m})}{\exp {\big (}{\frac {25-V_{m}}{10}}{\big )}-1}}}

@${\alpha _{h}(V_{m})=0.07\exp {\bigg (}{\frac {-V_{m}}{20}}{\bigg )}}

@${\beta _{n}(V_{m})=0.125\exp {\bigg (}{\frac {-V_{m}}{80}}{\bigg )}}

@${\beta _{m}(V_{m})=4\exp {\bigg (}{\frac {-V_{m}}{18}}{\bigg )}}

@${\beta_{h}(V_{m})={\frac {1}{\exp {\big (}{\frac {30-V_{m}}{10}}{\big)}+1}}}

@section{Coding the HHM}
We have a lot of variables and constants with particular values. It is inconvenient to keep entering each as a very long list of arguments, so we can construct a collection of all of them so we can pass around a single collection that will contain all the elements we need. It would also be convenient to be able to refer to them by name and not to have to remember their exact location in the collection. We can use a simple association list for this, but I will use a @italic{hash-table}, which is more like a python dictionary.

@examples[#:no-prompt
          #:label "Defining the Basic Neuron Parameters"
       (define neuron-details (hash 'dt 0.05
                             'init-t 0.0
                             'start-time 10.0
                             'stop-time 34.05
                             'cap 1.0
                             'init-v 0.0
                             'injection-current 20.0
                             'ena 115.0
                             'gna 120.0
                             'ek -12.0
                             'gk 36.0
                             'el 10.6
                             'gl 0.30))]        

                 


@examples[#:no-prompt
          #:label "All the Helper Functions:"
(define (alpha-n volt)
  (/ (- 0.1 (* 0.01 volt)) (- (exp (- 1 (* 0.1 volt))) 1.0)))
(define (alpha-m volt)
  (/ (- 2.5 (* 0.1 volt)) (- (exp (- 2.5 (* 0.1 volt))) 1.0)))
(define (alpha-h volt)
  (* 0.07 (exp (/ (* -1.0 volt) 20.0))))
(define (beta-n volt)
  (* 0.125 (exp (/ (* -1.0 volt) 80.0))))
(define (beta-m volt)
  (* 4.0 (exp (/ (* -1.0 volt) 18.0))))
(define (beta-h volt)
  (/ 1.0 (+ (exp (- 3.0 (* 0.1 volt))) 1.0)))
(define (m-dot volt m)
  (- (* (alpha-m volt) (- 1 m)) (* (beta-m volt) m)))
(define (n-dot volt n)
  (- (* (alpha-n volt) (- 1 n)) (* (beta-n volt) n)))
(define (h-dot volt h)
  (- (* (alpha-h volt) (- 1 h)) (* (beta-h volt) h)))
(define (m-infinity volt)
  (/ (alpha-m volt) (+ (alpha-m volt) (beta-m volt))))
(define (n-infinity volt)
  (/ (alpha-n volt) (+ (alpha-n volt) (beta-n volt))))
(define (h-infinity volt)
  (/ (alpha-h volt) (+ (alpha-h volt) (beta-h volt))))

(define (between x nps)
  (let ([lower (hash-ref nps 'start-time)]
        [upper (hash-ref nps 'stop-time)]
        [if-true (hash-ref nps 'injection-current)]
        [if-false 0.0])
  (if (and (>= x lower) (<= x upper)) if-true if-false)))]

@subsection{Updating the Voltage}
Look back at the @${\frac{dv}{dt}} formula for the I&F model and try to see the similarities. Although this function looks more complex it is still the basic Euler Method we used from the I&F model. In fact, if you look where the @code{update} function comes from you will see it is literally the one from the I&F model.

@examples[#:no-prompt
          #:label "Computing the Change of Voltage"
(define (dvdt voltage-now curr-in hh-m hh-n hh-h neuron-parameters)
  (let ([ena (hash-ref neuron-parameters'ena)]
        [gna (hash-ref neuron-parameters'gna)]
        [ek  (hash-ref neuron-parameters 'ek)]
        [gk  (hash-ref neuron-parameters'gk) ]
        [el  (hash-ref neuron-parameters'el) ]
        [gl  (hash-ref neuron-parameters'gl) ])
    (- curr-in (+ (* gna (expt hh-m 3.0) hh-h (- voltage-now ena))
                  (* gk (expt hh-n 4.0) (- voltage-now ek))
                  (* gl (- voltage-now el))))))]

@examples[#:no-prompt
          #:label "Running the Model"
(define (run-hh-sim nps #:max-time (max-time 60.0) #:max-iter (max-iter 50000)) 
  (let ([dt (hash-ref nps 'dt)]
        [init-v (hash-ref nps 'init-v)])
	(for*/fold
         ([t (hash-ref nps 'init-t)]
          [hh-m (m-infinity init-v)] 
          [hh-n (n-infinity init-v)]
          [hh-h (h-infinity init-v)] 
          [i 0.0]
          [v init-v ]          
          [accum '()]
          #:result (reverse accum))
         ([n (in-range max-iter)])
          #:break (> t max-time)
         (values (+ t dt)
                 (update hh-m (m-dot v hh-m) dt )
                 (update hh-n (n-dot v hh-n) dt )
                 (update hh-h (h-dot v hh-h) dt )
                 (between t nps)
                 (update v
                         (dvdt v i hh-m hh-n hh-h nps) dt)
                 (cons (list t i v) accum)))))]



Testing
@examples[#:no-prompt
          #:label "Running the HH Model"
          #:eval plot-eval
          (begin
            (require "./../code/handh.rkt")
            (define run (run-hh-sim neuron-details)))
          (plot (list (lines (map vector (map first run) (map second run))) (lines (map vector (map first run) (map third run)))))]



