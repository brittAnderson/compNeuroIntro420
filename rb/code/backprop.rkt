#lang racket

(provide (all-defined-out))

(define (sig x) (/ 1.0 (+ 1 (exp (* -1.0 x)))))

(define (deriv-sig x) (* (sig x) (- 1 (sig x))))

(define (relu x)
  (if (< x 0) 0 x))

(define (deriv-relu x)
  (if (< x 0) 0 1))

(define (random-uniform-wt small big)
  (let ([range (abs (- small big))])
    (+ small (* (random) range))))

(define (random-1to1) (random-uniform-wt -1 1))

(struct network (layers) #:transparent)

(struct layer (nodes bias layer-before layer-after) #:transparent)

(struct node (wts act-fxn deriv-act-fxn prior-out curr-out bias) #:transparent) 

(define (init-node-first-layer  #:weights [wts (list 1.0)] #:activation-function [act-fxn identity]
                                #:derivative-activation-function [deriv-act-fxn 'deriv-sig]
                                #:prior-out [po 0.0] #:current-out [co 0.0] #:bias [b 0.0])
  (node
   wts act-fxn deriv-act-fxn po co b))

(define (init-node-not-first-layer  #:weights [wts null] #:activation-function [act-fxn sig]
                                #:derivative-activation-function [deriv-act-fxn 'deriv-sig]
                                #:prior-out [po 0.0] #:current-out [co 0.0] #:bias [b 0.0])
  (node
   (cond
     [(number? wts) (for/list ([i (in-range wts)])
                      (random-1to1))]
     [#t null])
   act-fxn deriv-act-fxn po co b))

(define (init-layer #:nodes [ns null] #:bias [b 0.0] #:layer-before [lb null] #:layer-after [la null])
  (layer ns b lb la))

(define (init-network units-per-layer)
  (let ([first-layer 0]
        [last-layer (- (length units-per-layer) 1)])
    (network 
     (for/list ([l (in-range (length units-per-layer))]
                [n units-per-layer]
                [next-layer-number (append (rest units-per-layer) '(-1))]
                [prior-layer-number-of-nodes (cons -1 units-per-layer)])
       (init-layer #:layer-before (if (not (= l first-layer)) (- l 1) null)
                   #:layer-after (if (not (= l last-layer)) (+ l 1) null)
                   #:nodes (cond
                             [(= l first-layer) (for/list ([i (in-range n)])
                                                  (init-node-first-layer))]
                             [(and (number? n) (positive? n))
                              (for/list ([i (in-range n)])
                                (init-node-not-first-layer
                                 #:weights prior-layer-number-of-nodes))]
                             [#t null]))))))

(define (temp-node-calc lin nd)
  (let* ([net (apply + (cons (node-bias nd) (map * lin (node-wts nd))))]
         [out ((node-act-fxn nd) net)])
    (node (node-wts nd)
          (node-act-fxn nd)
          (node-deriv-act-fxn nd)
          (node-curr-out nd)
          out
          (node-bias nd))))
          
(define (map-node-calc-across-layer lin lay)
  (layer (if (null? (layer-layer-before lay))
             (for/list ([n (layer-nodes lay)]
                        [inputs (map list lin)])
               (temp-node-calc inputs n))
             (for/list ([n (layer-nodes lay)])
               (temp-node-calc lin n)))
         (layer-bias lay)
         (layer-layer-before lay)
         (layer-layer-after lay)))
      
(define (compute-layer-output lay)
  (map node-curr-out (layer-nodes lay)))

(define (forward-pass lin net)
  (for/fold ([accum (list )]
             [inputs lin]
            #:result (network (reverse accum)))
            ([l (network-layers net)])
    (let ([temp-l (map-node-calc-across-layer inputs l)])
      (values (cons temp-l accum)
            (compute-layer-output temp-l)))))
    
             

(define test-net (init-network (list 2 2 1)))
  
; To-do
; handle biases better
; Right now there are slots for bias weights for each node and each layer
; But I don't have a way to really turn them on and off practically. Right
; the bias is set to zero and added so it is essentially "off". 
