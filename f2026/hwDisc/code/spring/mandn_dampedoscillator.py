import numpy as np
import matplotlib.pyplot as plt

# Parameters
m = 1.0      # mass (kg)
k = 4.0      # spring constant (N/m)
b = 0.5      # damping coefficient (kg/s)

x = 1.0      # initial position (m)
v = 0.0      # initial velocity (m/s)

dt = 0.01    # time step (s)
t_max = 10   # total simulation time (s)

# Lists for plotting
times = []
positions = []

t = 0

while t < t_max:
    # Acceleration from spring force and damping force
    a = -(k / m) * x - (b / m) * v

    # Update velocity and position (Euler method)
    v += a * dt
    x += v * dt

    # Store data
    times.append(t)
    positions.append(x)

    t += dt

# Plot results
plt.plot(times, positions)
plt.xlabel("Time (s)")
plt.ylabel("Position (m)")
plt.title("Damped Oscillating Spring")
plt.grid(True)
plt.show()