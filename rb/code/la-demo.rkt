#lang racket
(require math/matrix)

(define a-mat (matrix [[1 2 3]]))

(define b-mat (matrix [[4 5 6]]))

;adding two vectors

(matrix+ a-mat b-mat)

(matrix* a-mat (matrix-transpose b-mat))


