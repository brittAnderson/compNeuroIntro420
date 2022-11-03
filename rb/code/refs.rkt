#lang racket/base

(require
  scriblib/autobib)

(provide (all-defined-out))

(define-cite ~cite citet generate-bibliography
  #:style author+date-square-bracket-style)

(define vsa-dollar
  (make-bib
   #:author (authors "Pentti Kanerva")
   #:title "What We Mean When We Say \"What's the Dollar of Mexico?\" : Prototypes and Mapping in Concept Space"
   #:location (proceedings-location "AAAI Fall Symposium Series")
   #:date 2010
   #:url "https://www.aaai.org/ocs/index.php/FSS/FSS10/paper/view/2243"))

(define neighborhood-agent-model
  (make-bib
   #:author (authors "Thomas C. Schelling")
   #:title "Dynamic models of segregation"
   #:date 1971
   #:location (journal-location "The Journal of Mathematical Sociology" #:volume 1
                                #:pages '(143 186))
   #:url "https://doi.org/10.1080/0022250X.1971.9989794"))

(define what-is-cognition
  (make-bib
   #:author (authors "Tim Bayne"
                     "David Brainard"
                     "Richard Byrne"
                     "Lars Chittka"
                     "Nicky Clayton"
                     "Cecilia Heyes"
                     "Jennifer Mather"
                     "Bence Ölveczky"
                     "Michael Shadlen"
                     "Thomas Suddendorf"
                     "Barbara Webb")
   #:title "What Is Cognition?"
   #:date 2019
   #:location (journal-location "Current Biology" #:volume 29)
   #:url "https://doi.org/10.1016/j.cub.2019.05.044"))


(define cog-comp-neurosci-is
  (make-bib
   #:author (authors "Thomas Naselaris"
                     "Danielle Bassett"
                     "Alyson Fletcher"
                     "Konrad Kording"
                     "Nikolaus Kriegeskorte"
                     "Hendrikje Nienborg"
                     "Russell A Poldrak"
                     "Daphna Shohamy"
                     "Kendrick Kay")
   #:title "Cognitive Computational Neuroscience: A New Conference for an Emerging Discipline"
   #:location (journal-location "Trends in Cognitive Sciences" #:volume 22 #:pages '(365 367))
   #:date 2018
   #:url "https://dx.doi.org/10.1016/j.tics.2018.02.008"))

(define turing-machine
  (make-bib
   #:author (authors "A. M. Turing")
   #:title "On computable numbers with an application to the Entscheidungsproblem"
   #:location (journal-location "Proc. London Math. Soc." #:volume 42 #:pages '(230 265))
   #:date 1936))

(define lambda-intro
  (make-bib
   #:author (authors "Greg Michaelson")
   #:title  "An introduction to functional programming through Lambda calculus"
   #:date   2011
   #:is-book? "yes"
   #:location (book-location #:publisher "Dover Publications")
   #:url "https://www.cs.rochester.edu/~brown/173/readings/LCBook.pdf"))

(define behav-logic
  (make-bib
   #:author (authors "Michael Schütte")
   #:title  "Logical Behaviorism"
   #:date 2008
   #:location (journal-location "Encyclopedia of Neuroscience" #:pages '(372 375))
   #:url "http://dx.doi.org/10.1007/978-3-540-29678-2_596"))

(define marr-re-evaluated
  (make-bib
   #:author (authors "R. McClamrock")
   #:title "Marr's three levels: A re-evaluation"
   #:location (journal-location "Minds and Machines" #:volume 1 #:pages '(185 196))
   #:date 1991
   #:url "https://doi.org/10.1007/BF00361036"))
  
(define handh-k-hw
  (make-bib
   #:author (authors "Omar A. Hafez"
                     "Allan Gottschalk")
   #:title "Altered Neuronal Excitability in a Hodgkin-Huxley Model Incorporating Channelopathies of the Delayed Rectifier Potassium Channel"
   #:location (journal-location "Journal of Computational Neuroscience"
                                #:volume 48
                                #:pages '(377 386))
   #:date 2020
   #:url "http://dx.doi.org/10.1007/s10827-020-00766-1"))

(define neuroevo
  (make-bib
   #:author (authors "Kenneth O. Stanley"
                     "Jeff Clune"
                     "Joel Lehman"
                     "Risto Mikkulainen")
   #:title "Designing neural networks thought neuroevolution"
   #:location (journal-location "Nature Machine Intelligence"
                                #:volume 1
                                #:pages '(24 35))
   #:date 2019
   #:url "https://www.researchgate.net/profile/Jeff-Clune/publication/330203191_Designing_neural_networks_through_neuroevolution/links/5e7243fc92851c93e0ac18ea/Designing-neural-networks-through-neuroevolution.pdf?_sg%5B0%5D=started_experiment_milestone&_sg%5B1%5D=started_experiment_milestone&origin=journalDetail"))

(define quantum-prob
  (make-bib
   #:author (authors "Peter D. Bruza"
                     "Zheng Wang"
                     "Jerome R. Busemeyer")
   #:title "Quantum cognition: a new theoretical approach to psychology"
   #:location (journal-location "Trends in Cognitive Science" #:volume 19 #:pages '(383 393))
   #:date 2015
   #:url "http://dx.doi.org/10.1016/j.tics.2015.05.001"))

(define quantum-prob-2022
  (make-bib
   #:author (authors "Pothos, Emmanuel M."
                     "Busemeyer, Jerome R.")
   #:title "Quantum Cognition"
   #:location (journal-location "Annual Review of Psychology" #:volume 73 #:pages '(749 778))
   #:date 2022
   #:url "https://dx.doi.org/10.1146/annurev-psych-033020-123501"))

(define quantum-prob-2015
  (make-bib
   #:author (authors "Jerome R. Busemeyer"
                      "Zheng Wang")
   #:title "What is Quantum Cognition, and How is it Applied to Psychology?"
   #:location (journal-location "Current Directions in Psychological Science" #:volume 24 #:pages '(163 169))
   #:date 2015
   #:url "https://jbusemey.pages.iu.edu/quantum/CDinQC.pdf"))

(define lba
  (make-bib
   #:title "The simplest complete model of choice response time: Linear ballistic accumulation"
   #:location (journal-location "Cognitive Psychology" #:volume 57 #:pages '(153 178))
   #:date 2008
   #:url "https://dx.doi.org/10.1016/j.cogpsych.2007.12.002"
   #:author (authors "Scott D. Brown"
                     "Andrew Heathcote")))

(define hopfield-orig
  (make-bib
   #:author (authors "J. J. Hopfield")
   #:title "Neural networks and physical systems with emergent collective computational abilities."
   #:location (journal-location "PNAS" #:volume 79 #:pages '(2554 2558))
   #:date 1982
   #:url "https://www.pnas.org/doi/abs/10.1073/pnas.79.8.2554"))
   
  
(define kohonen-book
  (make-bib 
            #:title "Self-organization and associative memory"
            #:author (authors "Kohonen, Teuvo")
            #:is-book? "yes"
            #:location (book-location #:publisher "Springer Science & Business Media")
            #:date 2012
            #:url "https://books.google.com/books?hl=en&lr=&id=cSzwCAAAQBAJ&oi=fnd&pg=PA1&dq=Self-Organization+and+Associative+Memory&ots=kJXwlePW0S&sig=rc_eCyYJ7IqmOthPHajxdCZTFpE"))
