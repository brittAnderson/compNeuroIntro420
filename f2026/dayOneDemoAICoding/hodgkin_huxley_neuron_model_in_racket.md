---
title: "Hodgkin Huxley neuron model in Racket"
source: "https://claude.ai/chat/5f85355d-7ac9-4dbf-80fe-b641a1a96d13"
model: "claude-sonnet-5"
exported: 2026-09-11
---

# Human — Sep 11, 2026, 1:38 PM

PLease write a simple model of the Hodgkin Huxley neuron in Racket.

# Claude — Sep 11, 2026, 1:39 PM

Good, Racket installed. Let me write and test the model.

**File: hodgkin-huxley.rkt**

```
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

```

Correctly spikes on the injected step current (two action potentials during the 5–30ms window, then relaxes back toward rest). Copying to outputs.

I tested it (installed Racket in the sandbox to confirm it runs and actually spikes, rather than just eyeballing the code).

Structure and choices worth flagging, since you'll want to critique them rather than take them on faith:

- **Integration**: forward Euler at `dt = 0.01` ms. This is the weak point. HH is stiff near spike onset (the `m` gate is fast), and Euler at this step size is only barely stable — push `dt` to 0.025 and you'll start seeing visible numerical drift in spike amplitude/timing. If you're doing anything beyond a demo (parameter sweeps, bifurcation work, matching to data), swap in RK4 — I kept it out here for readability, not because it's a good idea for real use.
- **Rate functions**: standard closed-form α/β fits from the 1952 paper, in absolute mV (not V-V_rest shifted). I patched the two removable singularities at V = −40 and V = −55 with L'Hôpital limits rather than leaving `0/0`, which racket would silently turn into `+nan.0` for exact float inputs near those points — worth checking if you ever see NaN propagate in a variant of this.
- **Gate initialization**: gates start at steady-state for whatever `V0` you pass, rather than an arbitrary guess — avoids a spurious transient at t=0.
- **Output**: `hh-simulate` returns the full `(time . state)` trace as a list; the `main` submodule is just a demo (step current 5–30 ms, prints every 1 ms). Everything is a plain function, no mutation, so it composes fine if you want to wrap it in a parameter sweep or feed it into Racket's `plot` library.

One thing I did *not* do: no unit-consistency checks or asserts on gate values staying in [0,1] — Euler error can in principle push them slightly out of range over long runs. If you're going to trust this for anything quantitative, I'd add that guard rather than assume it holds.

# Human — Sep 11, 2026, 1:41 PM

Great. Can you do it python now.

# Claude — Sep 11, 2026, 1:41 PM

**File: hodgkin_huxley.py**

```python
"""
Hodgkin-Huxley single-compartment neuron model.

Standard squid giant axon parameters (Hodgkin & Huxley, 1952).
Units: mV, ms, uF/cm^2, mS/cm^2, uA/cm^2.
Integration: fixed-step forward Euler (simple, not the most accurate --
see the note at the bottom of this file for why you might switch to RK4).

Direct port of the Racket version -- same equations, same defaults,
same caveats.
"""

from __future__ import annotations
from dataclasses import dataclass
from math import exp
from typing import Callable, List, Tuple


# --- Parameters --------------------------------------------------------

@dataclass(frozen=True)
class HHParams:
    Cm: float = 1.0     # membrane capacitance (uF/cm^2)
    gNa: float = 120.0  # max sodium conductance (mS/cm^2)
    gK: float = 36.0    # max potassium conductance (mS/cm^2)
    gL: float = 0.3     # leak conductance (mS/cm^2)
    ENa: float = 50.0   # sodium reversal potential (mV)
    EK: float = -77.0   # potassium reversal potential (mV)
    EL: float = -54.4   # leak reversal potential (mV)


DEFAULT_PARAMS = HHParams()


# --- State ---------------------------------------------------------------
# V: membrane potential (mV)
# m, h: sodium activation / inactivation gates (dimensionless, [0,1])
# n: potassium activation gate (dimensionless, [0,1])

@dataclass
class HHState:
    V: float
    m: float
    h: float
    n: float


# --- Rate functions (alpha_x, beta_x), classic HH form -------------------
# V here is in mV using the modern convention (resting ~ -65 mV, not
# shifted to 0). Standard closed-form fits from the original 1952 paper.

def alpha_m(V: float) -> float:
    x = V + 40.0
    if abs(x) < 1e-6:
        return 1.0  # removable singularity at V = -40mV, limit = 1.0
    return (0.1 * x) / (1.0 - exp(-x / 10.0))


def beta_m(V: float) -> float:
    return 4.0 * exp(-(V + 65.0) / 18.0)


def alpha_h(V: float) -> float:
    return 0.07 * exp(-(V + 65.0) / 20.0)


def beta_h(V: float) -> float:
    return 1.0 / (1.0 + exp(-(V + 35.0) / 10.0))


def alpha_n(V: float) -> float:
    x = V + 55.0
    if abs(x) < 1e-6:
        return 0.1  # removable singularity at V = -55mV, limit = 0.1
    return (0.01 * x) / (1.0 - exp(-x / 10.0))


def beta_n(V: float) -> float:
    return 0.125 * exp(-(V + 65.0) / 80.0)


def x_inf(alpha: Callable[[float], float], beta: Callable[[float], float], V: float) -> float:
    """Steady-state gate value, for initializing gates at rest."""
    a, b = alpha(V), beta(V)
    return a / (a + b)


# --- Ionic currents --------------------------------------------------------

def I_Na(p: HHParams, V: float, m: float, h: float) -> float:
    return p.gNa * m ** 3 * h * (V - p.ENa)


def I_K(p: HHParams, V: float, n: float) -> float:
    return p.gK * n ** 4 * (V - p.EK)


def I_L(p: HHParams, V: float) -> float:
    return p.gL * (V - p.EL)


# --- Derivatives -----------------------------------------------------------

def dV_dt(p: HHParams, s: HHState, Iext: float) -> float:
    return (Iext - I_Na(p, s.V, s.m, s.h) - I_K(p, s.V, s.n) - I_L(p, s.V)) / p.Cm


def dm_dt(V: float, m: float) -> float:
    return alpha_m(V) * (1.0 - m) - beta_m(V) * m


def dh_dt(V: float, h: float) -> float:
    return alpha_h(V) * (1.0 - h) - beta_h(V) * h


def dn_dt(V: float, n: float) -> float:
    return alpha_n(V) * (1.0 - n) - beta_n(V) * n


# --- One forward-Euler step -------------------------------------------

def hh_step(p: HHParams, s: HHState, Iext: float, dt: float) -> HHState:
    V, m, h, n = s.V, s.m, s.h, s.n
    return HHState(
        V=V + dt * dV_dt(p, s, Iext),
        m=m + dt * dm_dt(V, m),
        h=h + dt * dh_dt(V, h),
        n=n + dt * dn_dt(V, n),
    )


# --- Resting initial state at a given holding voltage ----------------------

def rest_state(V0: float = -65.0) -> HHState:
    return HHState(
        V=V0,
        m=x_inf(alpha_m, beta_m, V0),
        h=x_inf(alpha_h, beta_h, V0),
        n=x_inf(alpha_n, beta_n, V0),
    )


# --- Simulation driver -----------------------------------------------------
# Runs for t_max ms at step dt ms, applying stimulus current Iext_fn
# (a function of time in ms, returning uA/cm^2).
# Returns a list of (time, HHState) records.

def hh_simulate(
    Iext_fn: Callable[[float], float],
    params: HHParams = DEFAULT_PARAMS,
    t_max: float = 50.0,
    dt: float = 0.01,
    V0: float = -65.0,
) -> List[Tuple[float, HHState]]:
    n_steps = round(t_max / dt)
    s = rest_state(V0)
    trace = [(0.0, s)]
    for i in range(1, n_steps + 1):
        t = i * dt
        s = hh_step(params, s, Iext_fn(t), dt)
        trace.append((t, s))
    return trace


# --- Demo: a step current injection, print a coarse trace ------------------

def _step_current(t: float) -> float:
    return 10.0 if 5.0 <= t <= 30.0 else 0.0


if __name__ == "__main__":
    trace = hh_simulate(_step_current, t_max=50.0, dt=0.01)
    print("t(ms)\tV(mV)\tm\th\tn")
    # print every 1ms-ish sample (every 100th point at dt=0.01)
    for i, (t, s) in enumerate(trace):
        if i % 100 == 0:
            print(f"{t:.2f}\t{s.V:.3f}\t{s.m:.3f}\t{s.h:.3f}\t{s.n:.3f}")

```

Numerically identical to the Racket output, as expected — same equations, same Euler scheme, same seed conditions.

Direct port — same equations, same forward-Euler scheme, same defaults, same two caveats as before (Euler stability at `dt=0.01`, no gate-bound assertions). Verified it against the Racket run: trace is numerically identical to 3 decimal places.

One difference worth noting since you work in NumPy-adjacent contexts: this is pure-Python with scalar floats, not vectorized. That's fine for a single trace like this, but if you want to sweep parameters or run many trials, a NumPy/`scipy.integrate.solve_ivp` rewrite (vectorized state, adaptive step) would be the right move rather than looping this in Python — forward Euler at fixed `dt` in a tight Python loop will be slow for anything beyond demo-scale.

# Human — Sep 11, 2026, 1:43 PM

Can you add a matploltibl plot for the python version

# Claude — Sep 11, 2026, 1:43 PM

Correct: two spikes during the current step, gate dynamics tracking as expected (m spikes fast, h dips on depolarization, n lags and drives the afterhyperpolarization).

Added `plot_trace()` — two-panel figure: top is V(t), bottom is the three gating variables with the stimulus current overlaid on a twin axis (dashed gray, so its scale doesn't compress the gate curves into the bottom of the plot). Run above confirms it and matches the earlier numeric trace exactly.

A couple of choices you may want to override:

- I saved to PNG (`fig.savefig`, no `plt.show()`) rather than popping an interactive window, since that's the safe default in a headless/sandboxed run. If you're working in a Jupyter session or local desktop, swap in `plt.show()` or just call `plot_trace` without the save path change.
- The twin-axis current overlay is a judgment call, not an obviously correct one — it's convenient for seeing stimulus-onset alignment, but twin axes with unlabeled color-coded scales are exactly the kind of thing that reads badly in a paper figure. For anything you'd actually publish, I'd break the current out into its own third panel instead.

