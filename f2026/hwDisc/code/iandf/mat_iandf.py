# Using the oscillator python file, I am making direct changes to this.
# Something to note is that I can't use the Euler-Cromer method, since the integrate and fire neuron is a first-order derivative (aka change in voltage over time)
# I will be using Euler's method. 
import matplotlib.pyplot as plt
import numpy as np
# No changes here

# First, we define the parameters for the tntegrate and fire neuron
dt = 0.01  # Time step
t_max = 20.0  # Total duration in seconds
#Instead of having spring constant + damping coefficient, we add these parameters:tau which is membrane time constant, resting potential, minumum threshold, voltage resets
tau = 5.0  # how quickly voltage responds to current
v_rest = 0.0  # Resting potential
v_thresh = 1.0  # Mimumum threshold
v_reset = 0.0  # Where the voltage resets after a spike occurs
v_spikepeak = 1.5  # The peak voltage of the spike, could be used for plotting 

# Setting the time array, no changes from oscillator
t = np.arange(0, t_max, dt)
n_steps = len(t)

# creating a current pulse that turns on at 2 seconds and stays on for 6 seconds. 
# We are going to add current here, since that wasn't a variable in the oscillator
I_ext= np.zeros(n_steps)
I_ext[(t >= 2.0) & (t <= 6.0)] = 1.5 

# Let's establish the initial conditions! Velocity at T=0 is 0 
V = np.zeros(n_steps)
V[0] = v_rest
spikes = np.zeros(n_steps) # This is a new variable that will be used to record when spikes occur.

# Create a loop
for i in range(n_steps - 1):
   dV = (I_ext[i] - (V[i] - v_rest)) / tau # dV/dt = Current - (voltage-rest)
if V[i + 1] >= v_thresh:
        V[i] = v_spikepeak 
        V[i + 1] = v_reset # requests to reset the voltage instantly
        spikes[i + 1] = 1.0 # creates a record the spike occured

# For the question in part 2: Once the current shuts off @ t = 6ms, I_ext becomes 0. The differential equation
# simplifies from dV/dt = (I- (V[i] - v_rest) / tau to dV/dt = -(V[i] - v_rest) / tau. 
# The rate of change that we notice is actually proportional to the difference between the current voltage and the resting potential.
# I've metioned in class that there is a potential for a leak in the integrate and fire neuron as its bringing the membrane potential to equilibrium. 
# We can plot now! No change from oscillator so far

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

# Top plot: Voltage Over Time
ax1.plot(t, V, color="blue", label="Membrane Voltage")
ax1.axhline(v_thresh, color="red", linewidth=0.5, linestyle=":", label="threshold")
ax1.set_title("Integrate and Fire Neuron")
ax1.set_xlabel("Time (ms)") # note it is in ms, as opposed to s like in the oscillator
ax1.set_ylabel("Voltage (V)")
ax1.grid(True, alpha=0.3)
ax1.legend()

# Bottom plot: Injected current
ax2.plot(t, I_ext, color="magenta", label="Injected")
ax2.axhline(0, color="black", linewidth=0.5, linestyle=":")
ax2.set_title("Stimulus")
ax2.set_xlabel("Time (ms)")
ax2.set_ylabel("Current")
ax2.grid(True, alpha=0.3)
ax2.legend()

plt.tight_layout()
plt.show()
plt.close()
# The end!