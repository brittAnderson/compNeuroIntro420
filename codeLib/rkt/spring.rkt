#lang racket

(require plot/pict)
(require plot/utils)

(provide release-spring)

(define (s-of-t delta-t v s)
  (+ s (* v delta-t)))
(define (v-of-t delta-t a v)
  (+ v (* a delta-t)))
(define (a-of-t p s)
  (* -1 p s))

(define +init-v+ 0.0)
(define +init-s+ 10.0)
(define +p+ 2.0)
(define +delta-t+ 0.05)


(define (release-spring #:max-time [max-time 15] #:max-iter [max-iter 3000])
  (for/fold ([a (a-of-t +p+ +init-s+)]
             [v +init-v+]
             [s +init-s+]
             [time 0]
             [acc '()]
             #:result (reverse acc))
            ([n (in-range max-iter)])
    #:break (> time max-time)
    (let* ([acc (cons (list a v s time) acc)]
           [a (a-of-t +p+ s)]
           [v (v-of-t +delta-t+ a v)]
           [s (s-of-t +delta-t+ v s)]
           [time (+ time +delta-t+)])
      (values a v s time acc))))

(define (plot-spring)
  (let ([spring-results (release-spring)])
  (plot (lines (map vector (map fourth spring-results) (map third spring-results))))))