#lang scribble/book

@(require plot/pict 
	  scribble/base
	  scribble-math/dollar
	  scribble/example
          scriblib/autobib
          "./code/refs.rkt")

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

@(define-cite ~cite citet-id generate-bibliography #:style author+date-style)

@title[#:tag "Projects"]{Topics for Final Projects}
@section{Agent Based Modeling}
Provide an overview of the idea behind agent based models in psychology. Give a demonstration by implementing in racket a simple version (including graphics) of the classic work of Schelling @~cite[neighborhood-agent-model].
@section{Genetic Algorithms}
Genetic algorithms are algorithms where the "learning" takes place via selection and recombination in analogy to evolution. It has been used to select the parameters of otherwise conventional neural networks, but can also be used on its own. If you select this project you will need to explain what a genetic algorithm is, make the case that it is relevant to some aspect of neural or psychological modeling, and provide some racket code implementing some examples. A @hyperlink["https://www.whitman.edu/Documents/Academics/Mathematics/2014/carrjk.pdf"]{tutorial paper (pdf)} with MATLAB® code is available. A more academic treatment (and much deeper treatment) is available as a review @~cite[neuroevo]. A recent podcast features a discussion with one of the authors, @hyperlink["https://thegradientpub.substack.com/p/joel-lehman-open-endedness-and-evolution#details"]{Joel Lehman}.
@section{Quantum Probability}
Developed for quantum mechanics the formalism of quantum probability has been suggested to be a better approach to human decision making than conventional, classical probability@~cite[quantum-prob]; @~cite[quantum-prob-2022]. It seems that one of the authors has some MATLAB® @hyperlink["https://jbusemey.pages.iu.edu/quantum/HilbertSpaceModelPrograms.htm"]{programs} available for some of the published models. For this project you will need to provide @bold{one} example of a different prediction between classical and quantum probability, review the empirical results in favor of the latter, and show a racket program that computes some of the key values.
@; a blog about quantum probability in general: https://www.math3ma.com/blog/a-first-look-at-quantum-probability-part-2
An additional review came out recently @~cite[quantum-modeling].
@section{Vector Symbolic Architectures}
Provide an overview of vector symbolic architectures and provide a short racket implementation of Kanerva's "what is Mexico's dollar?" example @~cite[vsa-dollar].
@section{Linear Ballistic Accumulators}
Many models of human decision making envision the process as one of evidence accumulation that drifts us towards a threshold. Hit one border and you decide "no"; the other direction and you decide "yes". The amount of time it takes is a proxy for reaction time. The proportion of times you hit one border is a proxy for accuracy. The full drift diffusion models have some complex mathematics so the @italic{Linear Ballistic Accumulator Model}@~cite[lba] was proposed as a much simpler alternative. For this project you will explain the LBA model and demonstrate an implementation in Racket. It would be nice to be able to show how to fit data to this model, but that will be more challenging. There are some statistics functions in Racket for @hyperlink["https://docs.racket-lang.org/math/stats.html#%28def._%28%28lib._math%2Fstatistics..rkt%29._statistics%29%29"]{simulation}, but this will take more work to get working right. 
@section{Fitzhugh-Nagamo Neuron Model}
This is a @hyperlink["https://en.wikipedia.org/wiki/FitzHugh%E2%80%93Nagumo_model"]{model} of neuronal firing that is very commonly used as a simpler, but informative, example of neuronal dynamics. For this project you will implement this model and demonstrate how to visualize the effects of parameter manipulations, e.g. by using @hyperlink["https://docs.racket-lang.org/plot/renderer2d.html#%28def._%28%28lib._plot%2Fmain..rkt%29._vector-field%29%29"]{vector field} plots.
@; some relevant python code: https://www.normalesup.org/~doulcier/teaching/modeling/excitable_systems.html


@generate-bibliography[#:sec-title "Project References"
                       #:tag "ref:projects"]
