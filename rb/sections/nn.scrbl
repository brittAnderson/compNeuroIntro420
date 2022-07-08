#lang scribble/book

@(require plot/pict
          scribble/manual
	  scribble/base
	  scribble-math/dollar
	  scribble/example
	  racket/math)

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


@title[#:tag "neural-network"]{Neural Networks}

Here is some random text. 

@(require scribble-embedding)

@(youtube "https://player.vimeo.com/video/465411716?h=4b8d9ad497&amp;badge=0&amp;autopause=0&amp;player_id=0&amp;app_id=58479")
