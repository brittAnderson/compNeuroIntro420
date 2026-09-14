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


def plot_trace(trace: List[Tuple[float, HHState]], Iext_fn: Callable[[float], float],
               save_path: str = "hh_trace.png") -> None:
    """Two-panel plot: membrane voltage on top, gating variables and the
    stimulus current below. Saves to save_path and does not call plt.show()
    (headless-safe); open the PNG to view it."""
    import matplotlib.pyplot as plt

    t = [rec[0] for rec in trace]
    V = [rec[1].V for rec in trace]
    m = [rec[1].m for rec in trace]
    h = [rec[1].h for rec in trace]
    n = [rec[1].n for rec in trace]
    I = [Iext_fn(ti) for ti in t]

    fig, (ax_v, ax_gate) = plt.subplots(2, 1, sharex=True, figsize=(8, 6),
                                         height_ratios=[2, 1])

    ax_v.plot(t, V, color="black", linewidth=1.2)
    ax_v.set_ylabel("V (mV)")
    ax_v.set_title("Hodgkin-Huxley membrane potential")

    ax_gate.plot(t, m, label="m (Na activation)")
    ax_gate.plot(t, h, label="h (Na inactivation)")
    ax_gate.plot(t, n, label="n (K activation)")
    ax_gate.set_ylabel("gate value")
    ax_gate.set_xlabel("t (ms)")
    ax_gate.legend(loc="upper right", fontsize=8)

    # stimulus current on a twin axis so its scale doesn't distort the gates
    ax_i = ax_gate.twinx()
    ax_i.plot(t, I, color="gray", linewidth=0.8, linestyle="--", alpha=0.6)
    ax_i.set_ylabel("I_ext (uA/cm^2)", color="gray")
    ax_i.tick_params(axis="y", labelcolor="gray")

    fig.tight_layout()
    fig.savefig(save_path, dpi=150)
    plt.close(fig)


if __name__ == "__main__":
    trace = hh_simulate(_step_current, t_max=50.0, dt=0.01)
    print("t(ms)\tV(mV)\tm\th\tn")
    # print every 1ms-ish sample (every 100th point at dt=0.01)
    for i, (t, s) in enumerate(trace):
        if i % 100 == 0:
            print(f"{t:.2f}\t{s.V:.3f}\t{s.m:.3f}\t{s.h:.3f}\t{s.n:.3f}")
    plot_trace(trace, _step_current, save_path="hh_trace.png")
    print("\nSaved plot to hh_trace.png")
