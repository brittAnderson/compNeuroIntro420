#lang racket/base

(provide
 ~cite citet generate-bibliography
 what-is-cognition
 cog-comp-neurosci-is
 turing-machine)

(require
  scriblib/autobib)

(define-cite ~cite citet generate-bibliography
  #:style author+date-square-bracket-style)



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
   #:author "A. M. Turing"
   #:title "On computable numbers with an application to the Entscheidungsproblem"
   #:location (journal-location "Proc. London Math. Soc." #:volume 42 #:pages '(230 265))
   #:date 1936))
