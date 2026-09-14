#lang racket

;; ---------------------------------------------------------------------------
;; Hodgkin-Huxley single-compartment neuron model
;;
;; Standard squid giant axon parameters (Hodgkin & Huxley, 1952).
;; Units: mV, ms, uF/cm^2, mS/cm^2, uA/cm^2.
;; Integration: fixed-step forward Euler (simple, not the most accurate —
;; see the note at the bottom for why you might switch to RK4).
;; ---------------------------------------------------------------------------

(provide hh-simulate
         hh-params
         (struct-out hh-state))

;; --- Parameters ------------------------------------------------------------

(struct hh-params
  (Cm    ; membrane capacitance (uF/cm^2)
   gNa   ; max sodium conductance (mS/cm^2)
   gK    ; max potassium conductance (mS/cm^2)
   gL    ; leak conductance (mS/cm^2)
   ENa   ; sodium reversal potential (mV)
   EK    ; potassium reversal potential (mV)
   EL)   ; leak reversal potential (mV)
  #:transparent)

(define default-params
  (hh-params 1.0 120.0 36.0 0.3 50.0 -77.0 -54.4))

;; --- State -------------------------------------------------------------
;; V: membrane potential (mV)
;; m, h: sodium activation / inactivation gates (dimensionless, [0,1])
;; n: potassium activation gate (dimensionless, [0,1])

(struct hh-state (V m h n) #:transparent)

;; --- Rate functions (alpha_x, beta_x), classic HH form ------------------
;; V here is in mV using the *modern* convention (resting ~ -65 mV, V_rest
;; not shifted to 0). These are the standard closed-form fits from the
;; original 1952 paper, expressed in absolute mV.

(define (alpha-m V)
  (define x (+ V 40.0))
  (if (< (abs x) 1e-6)
      1.0 ; removable singularity at V = -40mV, limit = 1.0
      (/ (* 0.1 x) (- 1.0 (exp (- (/ x 10.0)))))))

(define (beta-m V)
  (* 4.0 (exp (- (/ (+ V 65.0) 18.0)))))

(define (alpha-h V)
  (* 0.07 (exp (- (/ (+ V 65.0) 20.0)))))

(define (beta-h V)
  (/ 1.0 (+ 1.0 (exp (- (/ (+ V 35.0) 10.0))))))

(define (alpha-n V)
  (define x (+ V 55.0))
  (if (< (abs x) 1e-6)
      0.1 ; removable singularity at V = -55mV, limit = 0.1
      (/ (* 0.01 x) (- 1.0 (exp (- (/ x 10.0)))))))

(define (beta-n V)
  (* 0.125 (exp (- (/ (+ V 65.0) 80.0)))))

;; Steady-state gate value and time constant, handy for initializing
;; the gates at rest instead of guessing.
(define (x-inf alpha beta V) (/ (alpha V) (+ (alpha V) (beta V))))

;; --- Ionic currents ------------------------------------------------------

(define (I-Na p V m h)
  (* (hh-params-gNa p) (expt m 3) h (- V (hh-params-ENa p))))

(define (I-K p V n)
  (* (hh-params-gK p) (expt n 4) (- V (hh-params-EK p))))

(define (I-L p V)
  (* (hh-params-gL p) (- V (hh-params-EL p))))

;; --- Derivatives -----------------------------------------------------------

(define (dV/dt p s Iext)
  (/ (- Iext
        (I-Na p (hh-state-V s) (hh-state-m s) (hh-state-h s))
        (I-K  p (hh-state-V s) (hh-state-n s))
        (I-L  p (hh-state-V s)))
     (hh-params-Cm p)))

(define (dm/dt V m) (- (* (alpha-m V) (- 1.0 m)) (* (beta-m V) m)))
(define (dh/dt V h) (- (* (alpha-h V) (- 1.0 h)) (* (beta-h V) h)))
(define (dn/dt V n) (- (* (alpha-n V) (- 1.0 n)) (* (beta-n V) n)))

;; --- One forward-Euler step --------------------------------------------

(define (hh-step p s Iext dt)
  (define V (hh-state-V s))
  (define m (hh-state-m s))
  (define h (hh-state-h s))
  (define n (hh-state-n s))
  (hh-state (+ V (* dt (dV/dt p s Iext)))
            (+ m (* dt (dm/dt V m)))
            (+ h (* dt (dh/dt V h)))
            (+ n (* dt (dn/dt V n)))))

;; --- Resting initial state at a given holding voltage -----------------

(define (rest-state [V0 -65.0])
  (hh-state V0
            (x-inf alpha-m beta-m V0)
            (x-inf alpha-h beta-h V0)
            (x-inf alpha-n beta-n V0)))

;; --- Simulation driver ---------------------------------------------------
;; Runs for `t-max` ms at step `dt` ms, applying stimulus current
;; `Iext-fn` (a function of time in ms, returning uA/cm^2).
;; Returns a list of (time V m h n) records.

(define (hh-simulate #:params [p default-params]
                      #:t-max [t-max 50.0]
                      #:dt [dt 0.01]
                      #:V0 [V0 -65.0]
                      #:Iext-fn Iext-fn)
  (define n-steps (inexact->exact (round (/ t-max dt))))
  (for/fold ([s (rest-state V0)]
             [acc (list (list 0.0 (rest-state V0)))]
             #:result (reverse acc))
            ([i (in-range 1 (add1 n-steps))])
    (define t (* i dt))
    (define s* (hh-step p s (Iext-fn t) dt))
    (values s* (cons (list t s*) acc))))

;; --- Demo: a step current injection, print a coarse trace -----------------

(module+ main
  (define (step-current t) (if (and (>= t 5.0) (<= t 30.0)) 10.0 0.0))
  (define trace (hh-simulate #:t-max 50.0 #:dt 0.01 #:Iext-fn step-current))
  (printf "t(ms)\tV(mV)\tm\th\tn\n")
  ;; print every 1ms-ish sample (every 100th point at dt=0.01)
  (for ([rec trace] [i (in-naturals)])
    #:break #f
    (when (zero? (modulo i 100))
      (define t (first rec))
      (define s (second rec))
      (printf "~a\t~a\t~a\t~a\t~a\n"
              (real->decimal-string t 2)
              (real->decimal-string (hh-state-V s) 3)
              (real->decimal-string (hh-state-m s) 3)
              (real->decimal-string (hh-state-h s) 3)
              (real->decimal-string (hh-state-n s) 3)))))
