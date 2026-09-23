#lang racket
(require plot)

(plot (list (function (lambda (x) (exp (/ 1 x))) -4 -0.01)
            (function (lambda (x) (exp (/ 1 x))) 0.01 4))
      #:y-min 0
      #:y-max 4)