#lang racket

(provide (all-defined-out))

;; convenience functions for activations
;; with their derivatives
(define (sig x) (/ 1.0 (+ 1 (exp (* -1.0 x)))))
(define (deriv-sig x) (* (sig x) (- 1 (sig x))))

(define (relu x)
  (if (< x 0) 0 x))
(define (deriv-relu x)
  (if (< x 0) 0 1))
;; to-do: programatically generate the derivatives
;; from the functions

;; convenience functions for random weight generation
;; when building networks
(define (random-uniform-wt small big)
  (let ([range (abs (- small big))])
    (+ small (* (random) range))))
(define (random-1to1) (random-uniform-wt -1 1))


;; structures for building networks
;; a *network* is a list of layers
;; a *layer* is a list of nodes as well as the
;;   integers representing the layer before and after
;;   a slot for a bias is present but not used
;; a *node* is the neuron and possesses its own
;;   weights (the weights coming in, post-synaptic)
;;   slots for the activation function and its derivative
;;   slots for input values, its prior and current output as
;;       well as its current net actitivity
;;   slots for a bias (used, but set to zero and never updated)
;;   slot for the "deltas" that need to be passed back for
;;       error backpropagation
(struct network (layers) #:transparent)
(struct layer (nodes bias layer-before layer-after) #:transparent)
(struct node (wts act-fxn deriv-act-fxn input-values prior-out curr-out net bias deltas) #:transparent) 
;; to-do: incorporate updating of the layer bias
;;        add types to enforce the structural model
;;        incorporate updating of the node bias


;; convenience function for initializing nodes/layer/network
;; of a random starting network. 
(define (init-node layer-id #:weights [wts (list 1.0)] #:activation-function [act-fxn identity]
                   #:derivative-activation-function [deriv-act-fxn deriv-sig]
                   #:input-values [iv (list )]
                   #:prior-out [po 0.0] #:current-out [co 0.0] #:bias [b 0.0])
  (if (equal? layer-id "first")
      (node
       wts act-fxn deriv-act-fxn iv po co 0.0 b (list 0.0))
      (node
       (cond
         [(number? wts) (for/list ([i (in-range wts)])
                          (random-1to1))]
         [#t null])
       sig deriv-act-fxn iv po co 0.0 b (list 0.0))))
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
                                                  (init-node "first"))]
                             [(and (number? n) (positive? n))
                              (for/list ([i (in-range n)])
                                (init-node "not-first"
                                           #:weights prior-layer-number-of-nodes))]
                             [#t null]))))))


;; functions related to the forward pass
;; such as calculating the output of a node based
;; on current input and storing the intermediate
;; values that will be needed for computing the
;; amount of error to be backpropagated

;; fp-node takes in the inputs as a list (lin)
;; and a current node (nd)
;; it returns a NEW node; it does not update in place
(define (fp-node lin nd)
  (let* ([net (apply + (cons (node-bias nd) (map * lin (node-wts nd))))]
         [out ((node-act-fxn nd) net)])
    (node (node-wts nd)
          (node-act-fxn nd)
          (node-deriv-act-fxn nd)
          lin
          (node-curr-out nd)
          out
          net
          (node-bias nd)
          (node-deltas nd))))

;; fp-layer maps the node updating function
;; across all the nodes in a layer and returns
;; a NEW copy of a layer after updating.
;; a slight difference is needed for handling the raw
;; input to the network and the input from other layers
(define (fp-layer lin lay)
  (layer (if (null? (layer-layer-before lay))
             (for/list ([n (layer-nodes lay)]
                        [inputs (map list lin)])
               (fp-node inputs n))
             (for/list ([n (layer-nodes lay)])
               (fp-node lin n)))
         (layer-bias lay)
         (layer-layer-before lay)
         (layer-layer-after lay)))
      
;; a helper function needed in the feedforward
;; calculation. The output of one layer becomes the
;; input to the next layer. Used in fp-net
(define (compute-layer-output lay)
  (map node-curr-out (layer-nodes lay)))

;; the forward pass function that loops over
;; all layers. This function requires the input
;; to be learned and a network to teach. It outputs
;; a NEW copy of the network after forward processing
(define (fp-net lin net)
  (for/fold ([accum (list )]
             [inputs lin]
            #:result (network (reverse accum)))
            ([l (network-layers net)])
    (let ([temp-l (fp-layer inputs l)])
      (values (cons temp-l accum)
            (compute-layer-output temp-l)))))

;; Backward pass processing functions.
;; There are three types of layers that require
;; unique handling: first, last, and all others in
;; between. Each node and layer has its own version of the
;; bp functions

;; for last layer nodes the gradient for their input weights
;; is only dependent on their error, activation, and input
;; through a weight. This function returns a NEW node after
;; updating the bp values needed for the layer below and
;; updating its own weights. It needs to know the "correct"
;; answer (desired)
;; delta is used locally to correct the weights of this node
;; delta-backward is the "delta" to be passed down to the layer
;; before for their updating.
(define (bp-last-layer-node desired n)
  (let* ([delta (* (- (node-curr-out n) desired) ((node-deriv-act-fxn n) (node-net n)))]
         [delta-backward (map (curry * delta) (node-wts n))]
         [diffs (map (curry * delta) (node-input-values n))])
    (node (map - (node-wts n) diffs)
          (node-act-fxn n)
          (node-deriv-act-fxn n)
          (node-input-values n)
          (node-prior-out n)
          (node-curr-out n)
          (node-net n)
          (node-bias n)
          delta-backward)))

;; simply loops over all the nodes in
;; the last layer using the above function
(define (bp-last-layer desired l)
  (layer (for/list  ([n (layer-nodes l)]
              [d desired])
           (bp-last-layer-node d n))
         (layer-bias l)
         (layer-layer-before l)
         (layer-layer-after l)))

;; function for updating middle layer nodes
;; needs to get its deltas from the layer above
;; (one for each neuron it contacts in the above layer)
;; returns a NEW node after updating
(define (bp-ml-node above-deltas n)
  (let* ([ws (node-wts n)]
         [deriv-out ((node-deriv-act-fxn n) (node-net n))]
         [local-deltas (* deriv-out (apply + above-deltas))]
         [diffs (map (curry * local-deltas) (node-input-values n))]
         [backward-deltas (map (curry * local-deltas) ws)])
    (node
     (map - ws diffs)
     (node-act-fxn n)
     (node-deriv-act-fxn n)
     (node-input-values n)
     (node-prior-out n)
     (node-curr-out n)
     (node-net n)
     (node-bias n)
     backward-deltas)))

;; loops over all nodes in a middle
;; layer and returns a NEW layer
;; with the NEW nodes
(define (bp-ml-layer deltas l)
  (layer
   (for/list ([n (layer-nodes l)]
              [i (in-range (length (layer-nodes l)))])
     (if (list? (car deltas))
         (bp-ml-node (map (lambda (x) (list-ref x i)) deltas) n)
         (bp-ml-node ((lambda (x) list-ref x i) deltas) n)))
   (layer-bias l)
   (layer-layer-before l)
   (layer-layer-after l)))

;; no separate updating function
;; needed for first layer nodes as they
;; never change. We can just copy this
;; layer and return a NEW one
(define (bp-first-layer l)
  (layer
   (layer-nodes l)
   (layer-bias l)
   (layer-layer-before l)
   (layer-layer-after l)))

;; the function that stitches the
;; above together to complete one
;; complete backward pass across a
;; network. Needs to know the goal
;; value and the network to train
;; returns a NEW copy of the backward
;; error corrected network
(define (bp-net desired ntwrk)
  (for/fold ([pass-back desired]
             [accum-layers (list )]
             #:result (network accum-layers))
            ([nt-layer (reverse (network-layers ntwrk))])
    (let ([outlayer (cond
                      [(null? (layer-layer-after nt-layer)) (bp-last-layer pass-back nt-layer)]
                      [(null? (layer-layer-before nt-layer)) (bp-first-layer nt-layer)]
                      [#t (bp-ml-layer pass-back nt-layer)])])
      (values (map node-deltas (layer-nodes outlayer)) (cons outlayer accum-layers)))))

;; Run a network forward and backward multiple
;; times. Assumes an input data format in which
;;     input has a list of two lists for each pattern
;;     to be learned. The first sublist is the input
;;     pattern and the second the desired output
;; The network returned by the forward loop is immediately
;; passed to the backpropagation function. Only the
;; final copy of the network is returned.
(define (bp-loop data network #:loop-no [ln 1000])
  (for*/fold ([tmp network])
              ([i (in-range ln)]
               [d data])
    (bp-net (second d) (fp-net (first d) tmp))))
;; to-do create some error tracking and visualization for
;; the run. Maybe allow for saving some of the interim networks
;; or testing results



;; convenience function for testing the classification
;; of a network. Give it an input and a network and it
;; returns the current output of the last layer
(define (test-learning input nw)
  (map node-curr-out (layer-nodes (last (network-layers (fp-net input nw))))))


;; convenience variables for testing
(define test-net (init-network (list 2 5 1)))

(define data-xor (list (list (list 0 0)  (list 0))
                       (list (list 0 1)  (list 1))
                       (list (list 1 0)  (list 1))
                       (list (list 1 1)  (list 0))))
;; Example use of above:
;; (define tmany (bp-loop data-xor test-net #:loop-no 10000))
;; (for ([i (map first data-xor)])
;;                (displayln (test-learning i t10000)))



