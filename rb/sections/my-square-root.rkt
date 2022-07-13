#lang racket
(provide my-sqrt)

(define (df g) (* 2.0 g))

(define (update-guess g target)
  (/ (- target (expt g 2.0) ) (df g)))

(define (my-sqrt [target 128.0] [guess 7.0] [tol 0.000001])
  (let* ([udg (update-guess guess target)]
         [current-guess (+ guess udg)])
    (if (< udg tol)
        current-guess
        (my-sqrt target current-guess))))
