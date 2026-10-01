"""
Spring simulations using Euler's method.

Equation of motion (mass on a spring):
    m * a = -k * x - b * v
      a = -(k/m) * x - (b/m) * v

Frictionless (undamped):  b = 0
Damped:                   b > 0

Euler's method update (explicit):
    a_n     = -(k/m) * x_n - (b/m) * v_n
    x_{n+1} = x_n + v_n * dt
    v_{n+1} = v_n + a_n * dt
"""

import numpy as np
import matplotlib.pyplot as plt


def simulate_spring(x0, v0, k, m, b, dt, t_max):
    """Simulate a spring with Euler's method.

    Returns arrays of time, position, velocity, and total energy.
    """
    n_steps = int(t_max / dt)
    t = np.zeros(n_steps + 1)
    x = np.zeros(n_steps + 1)
    v = np.zeros(n_steps + 1)

    x[0], v[0] = x0, v0

    for i in range(n_steps):
        a = -(k / m) * x[i] - (b / m) * v[i]   # acceleration from spring + damping
        x[i + 1] = x[i] + v[i] * dt            # Euler update: position
        v[i + 1] = v[i] + a * dt               # Euler update: velocity
        t[i + 1] = t[i] + dt

    energy = 0.5 * m * v**2 + 0.5 * k * x**2
    return t, x, v, energy


def analytic_solution(t, x0, v0, k, m, b):
    """Exact solution (for comparison), valid for the underdamped case."""
    omega0 = np.sqrt(k / m)
    gamma = b / (2 * m)
    if gamma == 0:
        return x0 * np.cos(omega0 * t) + (v0 / omega0) * np.sin(omega0 * t)
    omega_d = np.sqrt(omega0**2 - gamma**2)
    return np.exp(-gamma * t) * (
        x0 * np.cos(omega_d * t) + ((v0 + gamma * x0) / omega_d) * np.sin(omega_d * t)
    )


def main():
    # --- Parameters ---
    m = 1.0       # mass (kg)
    k = 10.0      # spring constant (N/m)
    x0 = 1.0      # initial displacement (m)
    v0 = 0.0      # initial velocity (m/s)
    dt = 0.001    # time step (s)
    t_max = 20.0  # total time (s)
    b_damped = 0.5  # damping coefficient (kg/s)

    # --- Run simulations ---
    t, x_free, v_free, E_free = simulate_spring(x0, v0, k, m, b=0.0, dt=dt, t_max=t_max)
    t, x_damp, v_damp, E_damp = simulate_spring(x0, v0, k, m, b=b_damped, dt=dt, t_max=t_max)

    x_free_exact = analytic_solution(t, x0, v0, k, m, 0.0)
    x_damp_exact = analytic_solution(t, x0, v0, k, m, b_damped)

    # --- Plot ---
    fig, axes = plt.subplots(2, 2, figsize=(13, 8))

    # Frictionless position
    ax = axes[0, 0]
    ax.plot(t, x_free, label="Euler", color="tab:blue")
    ax.plot(t, x_free_exact, "--", label="Exact", color="black", alpha=0.6)
    ax.set_title("Frictionless Spring: Position vs. Time")
    ax.set_xlabel("Time (s)")
    ax.set_ylabel("Position (m)")
    ax.legend()
    ax.grid(alpha=0.3)

    # Damped position
    ax = axes[0, 1]
    ax.plot(t, x_damp, label="Euler", color="tab:red")
    ax.plot(t, x_damp_exact, "--", label="Exact", color="black", alpha=0.6)
    ax.set_title(f"Damped Spring (b = {b_damped}): Position vs. Time")
    ax.set_xlabel("Time (s)")
    ax.set_ylabel("Position (m)")
    ax.legend()
    ax.grid(alpha=0.3)

    # Phase space (velocity vs position)
    ax = axes[1, 0]
    ax.plot(x_free, v_free, label="Frictionless", color="tab:blue")
    ax.plot(x_damp, v_damp, label="Damped", color="tab:red")
    ax.set_title("Phase Space")
    ax.set_xlabel("Position (m)")
    ax.set_ylabel("Velocity (m/s)")
    ax.legend()
    ax.grid(alpha=0.3)

    # Energy
    ax = axes[1, 1]
    ax.plot(t, E_free, label="Frictionless", color="tab:blue")
    ax.plot(t, E_damp, label="Damped", color="tab:red")
    ax.set_title("Total Energy vs. Time")
    ax.set_xlabel("Time (s)")
    ax.set_ylabel("Energy (J)")
    ax.legend()
    ax.grid(alpha=0.3)

    fig.suptitle("Spring Oscillator Simulated with Euler's Method", fontsize=14)
    fig.tight_layout()
    fig.savefig("spring_plot.png", dpi=150)
    plt.show()


if __name__ == "__main__":
    main()
