#lang racket

(require math/matrix
         math/array
         racket/random
         racket/snip
         racket/draw
         "outer.rkt")

(provide (all-defined-out))

(define (hf-gen-size-n n)
  (for/matrix (sqrt n) (sqrt n)
  ([entry (random-sample (in-list (list 1.0 -1.0)) n)])
  entry))

(define (hf-gen-size-n-not-sq n)
  (list->matrix n 1 (for/list ([i (in-range n)])
    (random-ref (in-list (list 1.0 -1.0))))))
  

  

(define (hf-make-n-patts-size-m n m)
  (for/list ([i (in-range n)])
    (hf-gen-size-n m)))

(define (zero-diag m)
  (let-values ([(r c) (matrix-shape m)])
    (for*/matrix r c
                 ([i (in-range r)]
                  [j (in-range c)]) 
                 (if (= i j) 0.0 (matrix-ref m i j)))))
                 

(define (hf-mk-wts patt-list)
  (let*-values ([(r c) (matrix-shape (first patt-list))]
                [(rc) (* r c)])
    (for/fold ([m (make-matrix (* r r) (* c c) 0)]
               #:result (array->mutable-array (matrix-scale (zero-diag m) (/ 1 (length patt-list)))))
              ([p patt-list])
      (let ([p-col (array-reshape p (vector (* r c) 1))])

        (matrix+ m (outer-prod p-col p-col))))))
    

(define (hf-calc-new-node-value r c #:threshold [threshold 0.0])
  (if (< threshold (matrix-ref (matrix* r c) 0 0)) 1.0 -1.0))

(define (hf-async1 wts pattern)
  (let-values ([(working-p) (mutable-array-copy pattern)]
               [(test-p)    (mutable-array-copy pattern)]
               [(r c) (matrix-shape wts)]
               [(b?) #f])
    (for ([i (in-naturals)])
      #:break b?
      (for ([rr (shuffle (range r))])
        (array-set! working-p (vector rr 0) (hf-calc-new-node-value (matrix-row wts rr) working-p))
        (set! b? (matrix= working-p test-p))
        (set! test-p working-p)))
    working-p))

(define (hf-loop-patts wts patterns)
  (for/fold ([trained-patterns (list )]
             #:result (reverse trained-patterns))
            ([p patterns])
    (let-values ([(r c) (matrix-shape p)])
      (cons
       (array-reshape (hf-async1 wts (array->mutable-array (array-reshape p (vector (array-size p) 1)))) (vector r c)) trained-patterns))))

    
(define (draw-one-pattern p dc scale shift-row shift-col)
  (let* ([r (matrix-num-rows p)]
         [c (matrix-num-cols p)])
    (for ([ci (in-range c)]
          [cc (in-range shift-col (+ shift-col (* c scale)) scale)])
      (for ([rj (in-range r)]
            [rr (in-range shift-row (+ shift-row (* r scale)) scale)])
      (match (matrix-ref p ci rj)
        [1.0 (send dc set-brush "red" 'solid)]
        [-1.0 (send dc set-brush "green" 'solid)])
      (send dc set-pen "white" 1 'transparent)
      (send dc draw-rectangle cc rr
            scale scale)))))

(define (draw-multiple-pattern ps ts dc scale)
  (for ([p ps]
        [i (in-range (length ps))]
        [t ts])
    (let-values ([(r c) (matrix-shape p)])
      (draw-one-pattern p dc scale (+ (* i scale) (* i (* scale r))) 0)
      (draw-one-pattern t dc scale (+ (* i scale) (* i (* scale r))) (+ (* scale c) (* scale c))))))


(define (hf-draw-wrapper ps ts #:scale [scale 10])
  (let* ([rs (matrix-num-rows (first ps))]
         [cs (matrix-num-cols (first ps))]
         [pn (length ps)]
         [img-x (* cs scale 3)]
         [img-y (+ (* scale (- pn 1)) (* rs scale pn)) ]
         [target (make-bitmap img-x img-y)]
         [dc (new bitmap-dc% [bitmap target])])
    (draw-multiple-pattern ps ts dc scale) 
    (make-object image-snip% target)))

; Example of how to use
; Make your test patterns
; (define test (hf-make-n-patts-size-m 4 16))
; Then make the weights from the test patterns
; (define wts (hf-mk-wts test))
; Then train
; (define outputs (hf-loop-patts wts test))
; Then visualize the results
; (hf-draw-wrapper test outputs #:scale 30)

