"""
Homework: Simulating Two Springs with Euler's Method
Reference: https://en.wikipedia.org/wiki/Euler_method

Euler's method approximates the solution of y' = f(t, y) by stepping
forward in small increments of size dt:
    y(t + dt) = y(t) + dt * f(t, y(t))

Each spring's motion is a 2nd-order ODE, so we rewrite it as a system of
two 1st-order ODEs in position s(t) and velocity v(t) = ds/dt, then apply
Euler's method to both at once, one timestep at a time.

Spring 1 (frictionless):      d^2s/dt^2 = -P * s
    ds/dt = v
    dv/dt = -P * s

Spring 2 (damped oscillator): d^2s/dt^2 = -P * s - k * v
    ds/dt = v
    dv/dt = -P * s - k * v

where s = position of the free (unattached) end of the spring, t = time,
P = spring constant, and k = damping constant (spring 2 only).
"""

import numpy as np
import matplotlib.pyplot as plt

# ----- Step 1: Set up the simulation parameters -----

P = 4.0          # spring constant, shared by both springs for a fair comparison
k = 0.5          # damping constant, used only by the damped spring
dt = 0.001       # Euler step size (smaller dt = more accurate, slower)
t_max = 20.0     # total simulation time

s0 = 1.0         # initial position (displacement from equilibrium)
v0 = 0.0         # initial velocity

n_steps = int(t_max / dt)


def simulate_spring(P, k, s0, v0, dt, n_steps):
    """
    Run Euler's method on s'' = -P*s - k*v.
    Passing k=0 reduces this to the frictionless spring's equation.
    Returns arrays of time values and position values.
    """
    t = np.zeros(n_steps + 1)
    s = np.zeros(n_steps + 1)
    v = np.zeros(n_steps + 1)

    s[0] = s0
    v[0] = v0

    for i in range(n_steps):
        a = -P * s[i] - k * v[i]       # acceleration from the ODE
        s[i + 1] = s[i] + dt * v[i]    # Euler step for position
        v[i + 1] = v[i] + dt * a       # Euler step for velocity
        t[i + 1] = t[i] + dt

    return t, s


# ----- Step 2: Run the simulation for each spring -----

t1, s1 = simulate_spring(P, 0.0, s0, v0, dt, n_steps)   # frictionless (k = 0)
t2, s2 = simulate_spring(P, k, s0, v0, dt, n_steps)      # damped

# ----- Step 3: Plot position vs. time for both springs -----

fig, ax = plt.subplots(figsize=(10, 6))

ax.plot(t1, s1, color="blue", label="Frictionless spring (k=0)")
ax.plot(t2, s2, color="red", label=f"Damped spring (k={k})")
ax.axhline(0, color="black", linewidth=0.5)

ax.set_xlabel("Time (t)")
ax.set_ylabel("Position (s)")
ax.set_title("Spring Position vs. Time (Euler's Method)")
ax.legend()
ax.grid(True, linestyle="--", alpha=0.5)

plt.tight_layout()
plt.savefig("spring_simulation.png")
plt.show()
