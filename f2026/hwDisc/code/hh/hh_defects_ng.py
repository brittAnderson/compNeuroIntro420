"""
Hodgkin Huxley model with the F184C mutation

F184C (episodic ataxia type 1) changes the K channel 4 ways:
   gammaA = 0.5   n gate opens half as fast
   gammaB = 1     n gate closes at normal speed
   gammaK = 0.15  only 15% of the normal K conductance
   dV     = 27    K channel voltage sensitivity shifted +27 mV
   
Changed formulas
   iK = gammaK * gK * n^4 * (v - eK)
   dn = gammaA * alphaN(v - dV) * (1 - n) - gammaB * betaN(v - dV) * n
   
The healthy model fires repeatedly. Each F184C change on its own still
fires repeatedly (lower threshold, as in the paper). All changes together give
one spike and then silence, which the paper also reports for F184C. The authors
suggest real neurons compensate with a healthy gene copy and other channels.
"""
import math
import matplotlib.pyplot as plt

# constants from Hodgkin and Huxley (resting voltage is 0 mV)
C = 1.0
gNa = 120.0
gK = 36.0
gL = 0.3
eNa = 115.0
eK = -12.0
eL = 10.6

dt = 0.01        # time step in ms
duration = 100   # total time in ms

# current injection
I_amount = 10.0  # current in uA/cm^2
I_start = 10     # turn current on at this time (ms)
I_end = 90       # turn current off at this time (ms)

# F184C mutation values
gammaA = 0.5
gammaB = 1.0
gammaK = 0.15
dV = 27.0


# rate functions (alpha = opening, beta = closing)
def alphaN(v):
    return 0.01 * (10 - v) / (math.exp((10 - v) / 10) - 1)

def betaN(v):
    return 0.125 * math.exp(-v / 80)

def alphaM(v):
    return 0.1 * (25 - v) / (math.exp((25 - v) / 10) - 1)

def betaM(v):
    return 4 * math.exp(-v / 18)

def alphaH(v):
    return 0.07 * math.exp(-v / 20)

def betaH(v):
    return 1 / (math.exp((30 - v) / 10) + 1)


# starting values
v = 0.0
# at the start the gates are at their resting (steady state) values
n = gammaA * alphaN(v - dV) / (gammaA * alphaN(v - dV) + gammaB * betaN(v - dV))
m = alphaM(v) / (alphaM(v) + betaM(v))
h = alphaH(v) / (alphaH(v) + betaH(v))

t = 0.0

# lists to save everything for plotting
timeList = []
voltList = []
currentList = []

while t < duration:
    # is the current on or off right now?
    if t >= I_start and t <= I_end:
        I = I_amount
    else:
        I = 0.0

    # the three ion currents
    iNa = gNa * m**3 * h * (v - eNa)
    iK = gammaK * gK * n**4 * (v - eK)
    iL = gL * (v - eL)

    # derivatives
    dv = (I - iNa - iK - iL) / C
    dn = gammaA * alphaN(v - dV) * (1 - n) - gammaB * betaN(v - dV) * n
    dm = alphaM(v) * (1 - m) - betaM(v) * m
    dh = alphaH(v) * (1 - h) - betaH(v) * h

    # save values
    timeList.append(t)
    voltList.append(v)
    currentList.append(I)

    # Euler update
    v = v + dv * dt
    n = n + dn * dt
    m = m + dm * dt
    h = h + dh * dt
    t = t + dt


# plot
plt.figure(figsize=(12, 7))
plt.subplot(2, 1, 1)
plt.plot(timeList, voltList)
plt.ylabel("Voltage (mV)")
plt.title("Hodgkin Huxley Model - F184C mutation")
plt.grid(True)

plt.subplot(2, 1, 2)
plt.plot(timeList, currentList, color="red")
plt.xlabel("Time (ms)")
plt.ylabel("Current (uA/cm^2)")
plt.grid(True)

plt.show()
