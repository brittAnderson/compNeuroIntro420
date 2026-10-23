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
n = alphaN(v) / (alphaN(v) + betaN(v))
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
    iK = gK * n**4 * (v - eK)
    iL = gL * (v - eL)

    # derivatives
    dv = (I - iNa - iK - iL) / C
    dn = alphaN(v) * (1 - n) - betaN(v) * n
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
plt.plot(timeList, voltList)
plt.ylabel("Voltage (mV)")
plt.title("Hodgkin Huxley Model")
plt.grid(True)
plt.xlim(0, 120)
plt.grid(True, alpha=0.4)
plt.tight_layout()
plt.show()

