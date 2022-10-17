#lang racket

(require math/matrix)

(define input (col-matrix [0.3 0.7]))

(define wts (row-matrix [-0.6 0.8]))

(define (get-new-wts inp ws input-class #:threshold [threshold 0])
  (let* ([est-class (if (> (matrix-ref
                            (matrix* wts inp) 0 0)
                           threshold)
                        1 -1)]
         [beta (* input-class est-class)])
    (matrix+ (matrix-scale (matrix-transpose inp) (* beta input-class ))
             wts)))
  
