#lang racket

(provide run-iandf-sim update between)

(define dt 0.05)
(define max-t 10)
(define init-t 0.0)
(define start-time 1.0)
(define stop-time 6.0)
(define cap 1)
(define res 2)
(define threshold 3.0)
(define spike-display 8.0)
(define init-v 0.0)
(define voltage init-v)
(define injection-current 4.3)
(define injection-time (cons start-time stop-time))
(define tau (* res cap))

(define (update old-value rate-of-change time-step)
            (+ (* rate-of-change time-step) old-value))

(define (dv-dt localres locali localv)
  (* (/ 1 tau) (- (* localres locali) localv)))

(define (between x #:lower [lower (car injection-time)]
                 #:upper [upper (cdr injection-time)]
                 #:if-true [if-true injection-current]
                 #:if-false [if-false 0.0])
  (if (and (>= x lower) (<= x upper)) if-true if-false))

(define (voltage-choice curr-volt spike-status #:thr [thr threshold]
                        #:sd [sd spike-display])
  (cond
    ((and (> curr-volt thr) (not spike-status)) sd)
    (spike-status 0.0d0)
    (#t curr-volt)))

(define (run-iandf-sim #:tolerance [tolerance 0.1] #:max-time [max-time 10] #:max-iter [max-iter 10000])
  (for*/fold ([t 0]
              [i 0]
              [v 0]
              [accum '()]
              #:result (reverse accum))
             ([n (in-range max-iter)])
    #:break (> t max-time)
    (let ([spike (< (abs (- v spike-display)) tolerance)])
      (values (+ dt t)
              (between t)
              (voltage-choice (update v
                                      (dv-dt res i v) dt) spike)
              (cons (list t i v) accum)))))
