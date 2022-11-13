#lang racket

(require math/array
         racket/match)

(define train (array #[#[ 0.05 0.1]]))

(define desired (array #[#[0.01 0.99]]))

(define wts (list (array #[#[0.15 0.20] #[0.25 0.30]])
                  (array #[#[0.4 0.45] #[0.1 -0.5] #[0.50 0.55] #[0.22 0.33]])
                  (array #[#[1.2 -2.3 2.3 0.2]  #[-2.003 -0.45 0.34 0.05]])))

(struct fp-strct (nets outs) #:transparent)

(define (sig x) (/ 1.0 (+ 1 (exp (* -1.0 x)))))

(define (deriv-sig x) (* (sig x) (- 1 (sig x))))

(define (relu x)
  (if (< x 0) 0 x))

(define (deriv-relu x)
  (if (< x 0) 0 1))

(define (cost-output-unit d o)
  (/ (expt (- d o) 2) 2))


(define (net-input output-prior-layer w)
  (array-axis-sum (array* w output-prior-layer) 1))

(define (outputs1 net-ins non-linearity)
  (cond  
    [(equal? non-linearity 'sig)    (array-map sig net-ins)]
    [(equal? non-linearity 'relu)   (array-map relu net-ins)]))

(define (one-forward output-prior-layer w nl)
  (let* ([ns (net-input output-prior-layer w)]
         [os (outputs1 ns nl)])
    (fp-strct ns os)))

(define (fp train wts #:nl [nl 'sig])
  (let ([last-layer (one-forward train (list-ref wts 0) nl)])
    (for/fold ([nets (list (fp-strct-nets last-layer))]
               [outs (list (fp-strct-outs last-layer))]
               ;               #:result (list  nets outs))
               #:result (values  nets outs))
              ([w (drop wts 1)])
      (let ([current-layer (one-forward (first outs) w nl)])
        (values (cons (fp-strct-nets current-layer) nets)
                (cons (fp-strct-outs current-layer) outs))))))

(define (out-layer-mk-delta ds o-c)
  (array* (array- o-c ds) o-c
          (array- (make-array (array-shape o-c) 1.0) o-c)))

(define (out-layer-mk-diff dlta o-n)
  (array* (array-reshape dlta (vector (array-size dlta) 1)) o-n))



(define (other-layers-mk-delta deltas o-c ws)
  (let ([sz-to-reshape (vector-ref (array-shape ws) 0)])
  (array*
   (array-reshape (array* (array-axis-fold deltas 1 +) o-c (array- (make-array (array-shape o-c) 1) o-c))
                  (vector sz-to-reshape 1)) ws)))

(define (other-layers-mk-diff deltas o-n)
  (array* deltas o-n))
  
(define (bp-debug wts ins ds ns os nl)
  (let* ([rwts (reverse wts)]
        [augmented-outs (append os (list (array-reshape train (vector 2))))]
        [out-curr (first augmented-outs)]
        [out-next (second augmented-outs)]
        [deltas (list (out-layer-mk-delta ds out-curr))]
        [diffs (list (out-layer-mk-diff (first deltas) out-next))]
        [new-wts (list (array- (first rwts) (first diffs)))])
    (for/fold ([accum-deltas deltas]
               [accum-diffs  diffs]
               [accum-new-wts new-wts])
              ([rwt (drop rwts 1)]
               [aug-o (drop augmented-outs 1)]
               [next-o (drop augmented-outs 2)])
      (values deltas diffs new-wts))))

(define (bp wts ins ds ns os nl)
  (let* ([rwts (reverse wts)]
        [augmented-outs (append os (list (array-reshape train (vector 2))))]
        [out-curr (first augmented-outs)]
        [out-next (second augmented-outs)]
        [deltas (list (out-layer-mk-delta ds out-curr))]
        [diffs (list (out-layer-mk-diff (first deltas) out-next))]
        [new-wts (list (array- (first rwts) (first diffs)))])
    (for/fold ([accum-deltas deltas]
               [accum-diffs  diffs]
               [accum-new-wts new-wts])
     ;          #:result (list accum-deltas accum-diffs accum-new-wts))
              ([rwt (drop rwts 1)]
               [aug-o (drop augmented-outs 1)]
               [next-o (drop augmented-outs 2)])
      (let* ([cur-dlta (other-layers-mk-delta (first deltas) aug-o rwt)]
             [cur-diff (other-layers-mk-diff cur-dlta next-o)])
        (values (cons cur-dlta accum-deltas)
                (cons cur-diff accum-diffs)
                (cons (array- cur-diff rwt) accum-new-wts))))))

(define-values (ns os) (fp train wts))
(define-values (deltas diffs nw) (bp wts train desired  ns os 'sig) )

  
