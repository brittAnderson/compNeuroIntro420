# Healthy vs Diseased Hodgkin-Huxley Model

import numpy as np
import matplotlib.pyplot as plt

# HH model parameters
c = 1.0 #capacitance
eNa = 115.0 #sodium reversal potential
gNa = 120.0 #sodium conductance
eK = -12.0 #potassium reversal potential
gK = 36.0 #potassium conductance
eL = 10.6 #leakage reversal potential
gL = 0.3 #max leak conductance

# Diseased potassium channel parameters
deltaV = 64.0 #used E325D delta variant here 
gamma_tau = 1.0 
gamma_alpha = 6.0 #E325D gamma alpha 
gamma_beta = 6.0 #E325D gamma beta

# Simulation parameters
dt = 0.01
FinalTime = 50.0
tStimStart = 5.0
tStimStop = 30.0
curIn = 10.0
RestingPotential = 0.0

alNVal = []
alMVal = []
alHVal = []
beNVal = []
beMVal = []
beHVal = []

naCurVal = []
kCurVal = []
lCurVal = []

# Rate functions
def alphN(v):
    newAlN = 0.01 * (10 - v) / (np.exp((10 - v) / 10) - 1)
    alNVal.append(newAlN)
    return newAlN

def alphM(v):
    newAlM = 0.1 * (25 - v) / (np.exp((25 - v) / 10) - 1)
    alMVal.append(newAlM)
    return newAlM

def alphH(v):
    newAlH = 0.07 * np.exp(-v / 20)
    alHVal.append(newAlH)
    return newAlH

def betaN(v):
    newBeN = 0.125 * np.exp(-v / 80)
    beNVal.append(newBeN)
    return newBeN

def betaM(v):
    newBeM = 4 * np.exp(-v / 18)
    beMVal.append(newBeM)
    return newBeM

def betaH(v):
    newBeH = 1 / (np.exp((30 - v) / 10) + 1)
    beHVal.append(newBeH)
    return newBeH


# Ionic currents
def naCur(v, m, h):
    return gNa * m**3 * h * (v - eNa)

def kCur(v, n):
    return gK * n**4 * (v - eK)

def lCur(v):
    return gL * (v - eL)

def dVolt(inCur, v, n, m, h):
    total_current = (
        naCur(v, m, h)
        + kCur(v, n)
        + lCur(v)
    )
    return (inCur - total_current) / c


# Run HH simulation
def hHmodel(duration, dt, curIn, tStimStart, tStimStop,
            diseased=False):

    time = np.arange(0.0, duration + dt / 2, dt)
    voltage = np.zeros(len(time))
    current = np.zeros(len(time))

    m = np.zeros(len(time))
    h = np.zeros(len(time))
    n = np.zeros(len(time))

    # Initial membrane potential
    voltage[0] = RestingPotential
    v0 = voltage[0]

    # Initial gating variables
    m[0] = alphM(v0) / (alphM(v0) + betaM(v0))
    h[0] = alphH(v0) / (alphH(v0) + betaH(v0))
    n[0] = alphN(v0) / (alphN(v0) + betaN(v0))

    # Euler 
    for i in range(len(time) - 1):

        # apply stimulus between start and stop 
        if tStimStart <= time[i] < tStimStop:
            current[i] = curIn
        else:
            current[i] = 0.0

        v = voltage[i]

        # sodium variables are the same in healthy and diseased models
        # no adjustment necessary

        m[i + 1] = m[i] + dt * (
            alphM(v) * (1 - m[i])
            - betaM(v) * m[i]
        )

        h[i + 1] = h[i] + dt * (
            alphH(v) * (1 - h[i])
            - betaH(v) * h[i]
        )

        # potassium variable
        if diseased:
    
            vK = v - deltaV

            n[i + 1] = n[i] + dt * gamma_tau * (
                gamma_alpha * alphN(vK) * (1 - n[i])
                - gamma_beta * betaN(vK) * n[i]
            )
        else:
            # healthy potassium channel
            n[i + 1] = n[i] + dt * (
                alphN(v) * (1 - n[i])
                - betaN(v) * n[i]
            )

        # voltage update
        voltage[i + 1] = voltage[i] + dt * dVolt(
            current[i], v, n[i], m[i], h[i]
        )

    # final current value
    if tStimStart <= time[-1] < tStimStop:
        current[-1] = curIn

    return time, voltage, current, m, h, n


# healthy model
time, healthyVolt, healthyCurrent, healthyM, healthyH, healthyN = (
    hHmodel(FinalTime, dt, curIn, tStimStart, tStimStop,
            diseased=False)
)

# diseased model
time, diseaseVolt, diseaseCurrent, diseaseM, diseaseH, diseaseN = (
    hHmodel(FinalTime, dt, curIn, tStimStart, tStimStop,
            diseased=True)
)


# Healthy vs Diseased Plot 
plt.figure(figsize=(12, 7))
plt.plot(time, healthyVolt, color="pink", label="Healthy version")
plt.plot(time, diseaseVolt, color="blue", label="Diseased version")
plt.xlabel("Time (mS)")
plt.ylabel("Voltage (mV)")
plt.title("Healthy vs Diseased Hodgkin-Huxley Model")
plt.legend()
plt.grid(True)
plt.tight_layout()
plt.show()