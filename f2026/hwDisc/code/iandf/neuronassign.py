
import numpy as np
import matplotlib.pyplot as plt

V_rest = 0
threshold = 1
tau = 0.02

dt = 0.001
t_max = 0.4


V = V_rest

times = []
voltages = []
currents = []


t = 0

while t < t_max:
    if t < 0.25:
        I = 1.5
    else:
        I = 0

    dV = (-(V - V_rest) + I) / tau

    V = V + dV * dt

    if V >= threshold:
        V = V_rest

    times.append(t)
    voltages.append(V)
    currents.append(I)

    t = t + dt

# Plot voltage and current
plt.subplot(2, 1, 1)
plt.plot(times, voltages)
plt.axhline(threshold, linestyle="--")
plt.ylabel("Voltage")
plt.title("Integrate-and-Fire Neuron")

plt.subplot(2, 1, 2)
plt.plot(times, currents)
plt.xlabel("Time (seconds)")
plt.ylabel("Current")

plt.tight_layout()
plt.show()

# When the current stops, the voltage returns to rest.
# The neuron stops firing because there is no input current. 
