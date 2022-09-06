#lang racket/base

(provide
 ~cite citet generate-bibliography
 what-is-cognition
  )

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
