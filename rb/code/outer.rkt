#lang racket

(require math/matrix)
(provide outer-prod pp-matrix)

(define (pp-matrix m)
  (for ([i (in-range (matrix-num-rows m))])
    (for ([j (in-range (matrix-num-cols m))])
      (fprintf (current-output-port) "~a\t" (matrix-ref m i j)))
    (fprintf (current-output-port) "\n")))

(define cm (col-matrix [1 2 3]))

(define rm (row-matrix [4 5 6]))

(define (outer-prod v1 v2)
  (matrix* (if (col-matrix? v1) v1 (matrix-transpose v1))
           (if (row-matrix? v2) v2 (matrix-transpose v2)))

