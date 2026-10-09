## ---------- Part One - I & F Script ----------

import numpy as np
import matplotlib.pyplot as plt

t_init = 0.0
t_final = 10.0

I_start = 1.0 # 1 second
I_stop = 6.0 # 6 seconds 

C = 1.0
R = 2.0
I = 0.0 

V_reset = 0.0 
V_threshold = 3.0 
spike = 10.0 

Tau = R * C
t_step = 0.01 

t = np.arange(t_init, t_final + t_step, t_step)

V = np.zeros(len(t))
V[0] = 0.0 # initial voltage

spiking = False # switch between spike/no spike

for i in range(len(t) - 1):

    # only injecting current within 1-6 seconds 
    if I_start <= t[i] <= I_stop:
        I = 5.0
    else:
        I = 0.0 

    dV_dt = (1 / Tau) * (R * I - V[i])
    V[i + 1] = V[i] + dV_dt * t_step 

    # drop after spike to 10.0 V, 
    # go up to spike when past voltage threshold
    if spiking: 
        V[i + 1] = V_reset
        spiking = False 

    elif V[i + 1] > V_threshold:
        V[i + 1] = spike
        spiking = True
 

## ---------- Part Two - Plot and Explain ----------

plt.plot(t, V, color = 'pink')

plt.title("Integrate and Fire Neuron Model")
plt.xlabel("Time")
plt.ylabel("Voltage")

plt.legend()
plt.grid(True)
plt.show()

## After the current pulse ends at t = 6 seconds, the voltage decays
## back toward 0 instead of continuing to rise. This is because I = 0.0 A, 
## meaning the differential equation would look like:

## dV_dt = (1 / Tau) * (2.0 * 0.0 - V[i])
## dV_dt = -V[i] / Tau 

## Tau, the time constant that represents resistance * capacitance 
## determines how fast the voltage decays once the current is turned off.

## Capacitance represents the leftover charge stored in the neuronal membrane.
## Resistance represents the leak that allows the charge to dissipate. 

