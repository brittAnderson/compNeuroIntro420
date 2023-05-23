#lang scribble/manual
@; taken from https://racket.discourse.group/t/usage-of-code-examples-in-scribble/1628/3
@(require scribble/examples)

A minimal example, I would like to use the A procedure in the example block.

@(define my-eval
   (make-base-eval #:lang 'racket/base))

@examples[#:eval my-eval #:hidden
  (define (A x y)
    (cond ((= y 0) 0)
          ((= x 0) (* 2 y))
          ((= y 1) 2)
          (else (A (- x 1) (A x (- y 1))))))]

@examples[#:eval my-eval
  (define (not-hidden x y)
    (cond ((= y 0) 0)
          ((= x 0) (* 2 y))
          ((= y 1) 2)
          (else (A (- x 1) (A x (- y 1))))))
]

The code for defining @racket{A} is hidden, but not for @racket{not-hidden}. 

@examples[#:eval my-eval
          (A 1 10)]

@examples[#:eval my-eval
          (not-hidden 1 10)]


