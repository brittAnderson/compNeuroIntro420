import numpy as np
import matplotlib.pyplot as plt

# -------------------------
# Parameters
# -------------------------
V_rest = 0.0       # Resting potential
V_reset = 0.0      # Reset voltage after spike
V_thresh = 1.0     # Spike threshold
tau = 10.0         # Membrane time constant

dt = 0.1
t_max = 100.0

# -------------------------
# Time array
# -------------------------
time = np.arange(0, t_max + dt, dt)

# -------------------------
# Storage arrays
# -------------------------
voltage = np.zeros(len(time))
current = np.zeros(len(time))

# -------------------------
# Input current pulse
# -------------------------
for i, t in enumerate(time):
    if 10 <= t <= 60:
        current[i] = 1.5

# -------------------------
# Initial condition
# -------------------------
voltage[0] = V_rest

# Spike times
spikes = []

# -------------------------
# Euler integration
# -------------------------
for i in range(len(time) - 1):

    # LIF equation
    dVdt = (-(voltage[i] - V_rest) + current[i]) / tau

    # Euler update
    voltage[i + 1] = voltage[i] + dVdt * dt

    # Spike detection
    if voltage[i + 1] >= V_thresh:
        spikes.append(time[i + 1])

        # Reset immediately
        voltage[i + 1] = V_reset

# -------------------------
# Plot
# -------------------------
fig, (ax1, ax2) = plt.subplots(
    2, 1,
    figsize=(10, 6),
    sharex=True
)

# Voltage trace
ax1.plot(time, voltage,
         color='blue',
         linewidth=2,
         label='Membrane Voltage')

ax1.axhline(
    V_thresh,
    color='red',
    linestyle='--',
    label='Threshold'
)

# Spike markers
for spike_time in spikes:
    ax1.axvline(
        spike_time,
        color='black',
        alpha=0.3
    )

ax1.set_ylabel("Voltage")
ax1.set_title("Leaky Integrate-and-Fire Neuron")
ax1.legend()
ax1.grid(True)

# Current trace
ax2.plot(
    time,
    current,
    color='green',
    linewidth=2,
    label='Input Current'
)

ax2.set_xlabel("Time")
ax2.set_ylabel("Current")
ax2.legend()
ax2.grid(True)

plt.tight_layout()
plt.show()

print("Number of spikes:", len(spikes))
print("Spike times:", spikes)

##The voltage looks that way after the current stops because it decays slowly after the current drops. Displaying a leaky membrane model. −(V − V_rest)/tau is what slowly drains it, showing an exponential decay towards V_rest.