#I remember doing oscillators/springs during my PHYS 111 Lab in 1A, but I haven't seen the code in python. 
import matplotlib.pyplot as plt
import numpy as np

# First, we define the parameters
dt = 0.01  # Time step
t_max = 20.0  # Total duration in seconds
P = 4.0  # Spring constant term (k/m)
k_damp = 0.5  # Damping coefficient for when we deal with the damped oscillator

# Setting the time array
t = np.arange(0, t_max, dt)
n_steps = len(t)

# Let's establish the initial conditions! Velocity at T=0 is 0 
s0 = 1.0  # Initial position
v0 = 0.0  # Initial velocity


# Let's start with the Undamped (Frictionless) Spring 

pos_undamped = np.zeros(n_steps)
vel_undamped = np.zeros(n_steps)

pos_undamped[0] = s0
vel_undamped[0] = v0

for i in range(n_steps - 1):
    acc = -P * pos_undamped[i]
    vel_undamped[i + 1] = vel_undamped[i] + acc * dt
    # Euler-Cromer step: uses newly computed velocity (i + 1). Without this step, the position would be updated using the old velocity (i), which is less accurate for the oscillatory system we are looking at. Energy consumption is better represented with this method.
    pos_undamped[i + 1] = pos_undamped[i] + vel_undamped[i + 1] * dt


# The Damped Spring Now!

pos_damped = np.zeros(n_steps)
vel_damped = np.zeros(n_steps)

pos_damped[0] = s0
vel_damped[0] = v0

for i in range(n_steps - 1):
    acc = -P * pos_damped[i] - k_damp * vel_damped[i]
    vel_damped[i + 1] = vel_damped[i] + acc * dt
    # Euler-Cromer step: uses newly computed velocity (i + 1)
    pos_damped[i + 1] = pos_damped[i] + vel_damped[i + 1] * dt


# After setting up our values, we can plot the results.

fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(10, 8))

# Adding my name and course info to the top right corner of the figure!
fig.text(
    0.98,
    0.98,
    "Mathura Murugesan | PSYCH 420 | Dr. Britt Anderson",
    fontsize=9,
    color="green",
    ha="right",
    va="top",
    style="italic",
)

# Top plot: Frictionless Spring
ax1.plot(t, pos_undamped, color="blue", label="Frictionless (Constant Amplitude)")
ax1.axhline(0, color="black", linewidth=0.5, linestyle=":")
ax1.set_title("Undamped Oscillator")
ax1.set_xlabel("Time (s)")
ax1.set_ylabel("Position")
ax1.grid(True, alpha=0.3)
ax1.legend()
# Bottom plot: Damped Spring
ax2.plot(t, pos_damped, color="magenta", linestyle="--", label="Damped")
ax2.axhline(0, color="black", linewidth=0.5, linestyle=":")
ax2.set_title("Damped Oscillator")
ax2.set_xlabel("Time (s)")
ax2.set_ylabel("Position")
ax2.grid(True, alpha=0.3)
ax2.legend()

plt.tight_layout()
plt.show()
plt.close()
# The end!