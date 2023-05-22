#lang scribble/book

@(require plot/pict
          scribble/manual
	  scribble/base
	  scribble-math/dollar
	  scribble/example
	  racket/math
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


@(define-cite ~cite citet-id generate-bibliography #:style author+date-style)

@title{Preface}

Experimental psychology was invented as a counterweight to the physical sciences. It is the difference between a science of mass and of weight, luminance and brightness. A pound of feathers has the same mass as a pound of pennies, but clearly the latter weighs more. Just try it.

To make a science of such subjective experience as to whether one thing is heavier or brighter than another there needed to be methods for human experimentation that were scientific. That is, they combined a subject matter of subjective experience with the standard procedures of empirical sciences: repeat measurements, control conditions, and systematic variation. By convention Wilhelm Wundt is taken as Empirical Psychology's founder and 1879, the year he established his independent experimental laboratory, as the date for the founding. It is only in the 1800s that we see the emergence of scientific experiments that look like modern psychology: Weber's weights, Helmholtz's mercury lamp flash experiments on attention, and Wundt's own experiments on attention.


@(require scribble-embedding)

@(youtube "https://www.youtube.com/embed/Zr7O41r8uEI")

While Wundt was merging the experimental methods of physics and physiology with the content of human awareness, it was a generation before Wundt that Weber collected the data that led Gustav Fechner, a physicist, to express mathematically a procedure for measuring psychological magnitudes as functions of physical intensities: @emph{psychophysics}@~cite[fechner-psychophysics].

Despite this early and potent demonstration of the power of using math for achieving insight into human subjective experience, quantitative models were not frequent in psychology for the next hundred years, and even now, despite notable and influential exceptions (the Rescorla-Wagner model, developed in the context of conditioning and the source of  modern reinforcement learning, Rosenblatt's perceptrons: the font from which neural networks flowed), mathematical models form only a small portion of published psychological research.  While the contemporary content of scientific psychology has greatly expanded, the predominant use of quantitative methods in psychology is still @italic{statistical inference}. That reliance on statistics may be both cause and consequence for why mathematics, such as calculus and linear algebra, are not curricular requirements for many psychology undergraduate programs though statistics courses are. We are much quicker to deploy complicated statistical methodologies than to use math as the language for expressing concretely, concisely, and unambiguously our psychological theories. Nor do we use computer programs based on psychological theories to explore model implications via simulations as much as we should.

@bold{This course is intended as a corrective. It endeavors to give undergraduates who may not have had any post-secondary math courses to speak of an exposure to some of the terminology and notation for the areas of mathematics most used in psychological and neuroscience models. The course combines this exposure with a heavy dose of programming exercises to practice concrete use. The goal is to build familiarity with terms and to desensitize some of the math and computing anxiety that formula and code excerpts can induce. In addition, and perhaps most importantly, the course wants to give students practice in seeing how formal mathematical ideas can be a potent source for focusing our discussion of what key psychological concepts @italic{are}}.

Of course, one cannot explore computational and mathematical ideas without having some familiarity with computing basics: writing code, markup syntax for reports and documentation, and ancillary tools such as @emph{git} for sharing. In years past I combined all these content areas into this single course. The heterogeneity of student backgrounds made that tough, but as there were no alternatives it was necessary. Now, however, I have split off the computing tools part from this content part. Students can and should come to this course with some basic familiarity with using their computer as a research tool. If they do not have that knowledge they can gain it from a variety of on-line sources. I outline my approach @hyperlink["https://brittanderson.github.io/Intro2Computing4Psychology/book/index.html"]{here}.

@(youtube "https://player.vimeo.com/video/448900968?h=eff8e7355a&amp;badge=0&amp;autopause=0&amp;player_id=0&amp;app_id=58479" )

Freeing this course from the constraints of teaching computing basics provides the space for including new content and teaching the older material differently. I would like both novice programmers and those with more experience programming to be able to get something from the exercises. I have explicitly decided not to use more common programming languages, such as python, so that everyone can focus on what it is we are trying to do, and not just what library can we import or what code we can find online to cut and paste? With the freedom to select any computing language I had the chance to hearken back to the early days of artificial intelligence (AI); an era when AI was about thinking and reasoning and not about how to import a model pretrained on billions of examples. By choosing a LISP I can also engage in a discussion of how programming languages differ, and how the design choices and features of a programming language may influence the expression of our theoretical ideas. Can a particular programming language lead us to new ways of thinking and conceiving of the problem space we wish to explore theoretically and via simulation?

All that is grand, but the course is still intended for undergraduates, many of whom may only possess programming rudiments. How to get them all, the Mac Users (both Intel and M1/2), as well as Windows and *Nix users, to have a common environment so that I can teach the same thing to all and so that they can get the tools installed on their computers in less than a month? Common-Lisp (CL) would be ideal, and I wrote some of the code for an @hyperlink["https://github.com/brittAnderson/compNeuroIntro420/tree/lisp"]{earlier offering} in CL, but installing CL and getting a sane working environment can be challenging. Thus, I decided to try @hyperlink["https://racket-lang.org/"]{Racket}. It is a language designed to support teaching, and has the DrRacket IDE. This works pretty much out of the box on Linux, Windows, and OSX systems. It even has a documentation system, @hyperlink["https://docs.racket-lang.org/scribble/index.html"]{scribble}, built-in, and which I am using to write this document. 

The remaining question is what new content to include? So far, I plan to expand the section on neuron modeling with an additional example, the Morris-Lecar model, that gives us a chance to explore how the differential equation formulation gives us additional information about our model via visualizing the phase space.

I also can now include something more traditional in the history of computational models of mind. We can code a simple Turing machine solving the busy beaver problem. We gain familiarity with this oft cited entity, and some concrete experience with the idea of computability and halting. How much more space is left for additional models? I hope to get to the Kohonen neural network for a week too. We will see from this fresh offering if there is time.

In summary, the goals for this course and this document are to give
students a familiarity with the mathematical terminology and domains
that form the backdrop to modeling in psychology. I still feel some
basic understanding of what certain mathematical gadgets are is
important, e.g. what a differential equation is is something
psychologists modeling memory should know about, but that most of them
do not. They do not, most of them, need to know how to analytically
solve the equations, but they should be able to use their own
programmed implementation to explore the implications of their ideas.
The basic constructs of linear algebra, matrices and vectors, are also
critical. It is essential for implementing many common neural
networks, but vector spaces also comprise a theoretical account of
representation. How much I can move beyond these fundamentals now that
I am not also trying to combine it with an introduction to programming
is a continuing experiment as we prepare to launch the Spring 2023 offering.



@generate-bibliography[#:sec-title "Preamble Bibliography"
                       #:tag "ref:preamble"]
