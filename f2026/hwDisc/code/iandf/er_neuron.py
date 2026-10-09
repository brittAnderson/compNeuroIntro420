from dataclasses import dataclass

import numpy as np
import matplotlib.pyplot as plt

# Time Constants
dt = 0.05 #Time Step
initt = 0.0 #Initial Time
maxt = 10.0 #Maximum Time
t = np.linspace(initt, maxt, int((maxt - initt) / dt))  # Time Array
# Unit Constants
I = [0] # Current Array
V = [0] # Voltage Array
C = 1.0  # Capacitance
R = 2.0  # Resistance
Tau = R * C  # Time (Tau) constant
# Spike/threshold constants
spikeStatus = [False] # Spike status array
Threshold = 3.0  # Threshold for firing
Spike = 8.0  # Spike value
injected_current = 4.3  # Injected current
injection_time = [1.0, 6.0] # Time interval for current injection

# Differential Equation
def dvdt(I, V):
    return (R*I - V) / Tau

# Determines if current should be injected based on time
def between(t):
    if (injection_time[0] <= t <= injection_time[1]):
        return injected_current
    else:
        return 0.0

# Determines if voltage should reset to resting or spike
def vchoice(cv, ss, thr, sd):
    if ss:
        return 0.0
    elif cv > thr:
        return sd
    else:
        return cv    

# Scary math (generates our voltage array)
for i in range(1, len(t)):
    cv = V[-1]
    ci = between(t[i])
    new_voltage = (dvdt(ci, cv))*(t[i] - t[i-1]) + cv
    nv = vchoice(new_voltage, spikeStatus[-1], Threshold, Spike)
    spikeStatus.append(abs(nv-Spike) < Threshold),
    I.append(ci)
    V.append(nv)

plt.plot(t, V)
plt.xlabel("Time (s)")
plt.ylabel("Voltage (V)")
plt.title("Integrate and Fire Neuron Model using Euler's Method")
plt.show()

# Given that a Neuron is 'leaky', if it doesn't reach the threshold it slowly loses it's voltage due to ion channels removing the remaining current (derived from ion charge), causing the I in V = I*R to slowly drop to 0, as a lack of further current is being injected to make up for the loss.