#lang racket

(require (only-in "iandf.rkt" update))

(provide run-hh-sim neuron-details)

(define neuron-details (hash 'dt 0.05
                             'init-t 0.0
                             'start-time 10.0
                             'stop-time 34.05
                             'cap 1.0
                             'init-v 0.0
                             'injection-current 20.0
                             'ena 115.0
                             'gna 120.0
                             'ek -12.0
                             'gk 36.0
                             'el 10.6
                             'gl 0.30))        


(define (alpha-n volt)
  (/ (- 0.1 (* 0.01 volt)) (- (exp (- 1 (* 0.1 volt))) 1.0)))

(define (alpha-m volt)
  (/ (- 2.5 (* 0.1 volt)) (- (exp (- 2.5 (* 0.1 volt))) 1.0)))

(define (alpha-h volt)
  (* 0.07 (exp (/ (* -1.0 volt) 20.0))))

(define (beta-n volt)
  (* 0.125 (exp (/ (* -1.0 volt) 80.0))))

(define (beta-m volt)
  (* 4.0 (exp (/ (* -1.0 volt) 18.0))))

(define (beta-h volt)
  (/ 1.0 (+ (exp (- 3.0 (* 0.1 volt))) 1.0)))

(define (m-dot volt m)
  (- (* (alpha-m volt) (- 1 m)) (* (beta-m volt) m)))

(define (n-dot volt n)
  (- (* (alpha-n volt) (- 1 n)) (* (beta-n volt) n)))

(define (h-dot volt h)
  (- (* (alpha-h volt) (- 1 h)) (* (beta-h volt) h)))

(define (m-infinity volt)
  (/ (alpha-m volt) (+ (alpha-m volt) (beta-m volt))))

(define (n-infinity volt)
  (/ (alpha-n volt) (+ (alpha-n volt) (beta-n volt))))

(define (h-infinity volt)
  (/ (alpha-h volt) (+ (alpha-h volt) (beta-h volt))))

(define (dvdt voltage-now curr-in hh-m hh-n hh-h neuron-parameters)
  (let ([ena (hash-ref neuron-parameters'ena)]
        [gna (hash-ref neuron-parameters'gna)]
        [ek  (hash-ref neuron-parameters 'ek)]
        [gk  (hash-ref neuron-parameters'gk) ]
        [el  (hash-ref neuron-parameters'el) ]
        [gl  (hash-ref neuron-parameters'gl) ])
    (- curr-in (+ (* gna (expt hh-m 3.0) hh-h (- voltage-now ena))
                  (* gk (expt hh-n 4.0) (- voltage-now ek))
                  (* gl (- voltage-now el))))))


(define (between x nps)
  (let ([lower (hash-ref nps 'start-time)]
        [upper (hash-ref nps 'stop-time)]
        [if-true (hash-ref nps 'injection-current)]
        [if-false 0.0])
  (if (and (>= x lower) (<= x upper)) if-true if-false)))

(define (run-hh-sim nps #:max-time (max-time 60.0) #:max-iter (max-iter 50000)) 
  (let ([dt (hash-ref nps 'dt)]
        [init-v (hash-ref nps 'init-v)])
	(for*/fold
         ([t (hash-ref nps 'init-t)]
          [hh-m (m-infinity init-v)] 
          [hh-n (n-infinity init-v)]
          [hh-h (h-infinity init-v)] 
          [i 0.0]
          [v init-v ]          
          [accum '()]
          #:result (reverse accum))
         ([n (in-range max-iter)])
          #:break (> t max-time)
         (values (+ t dt)
                 (update hh-m (m-dot v hh-m) dt )
                 (update hh-n (n-dot v hh-n) dt )
                 (update hh-h (h-dot v hh-h) dt )
                 (between t nps)
                 (update v
                         (dvdt v i hh-m hh-n hh-h nps) dt)
                 (cons (list t i v) accum)))))
