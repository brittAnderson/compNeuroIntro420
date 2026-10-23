import numpy as np
import matplotlib.pyplot as plt

# Constants
gNa = 120
gK = 36
gL = 0.3

eNa = 115
eK = -12
eL = 10.6

dt = 0.01

# Gamma values from Table 1 Properties of Kv1.1 channelopathies associated with episodic ataxia type 1
gamma_alpha = 0.5
gamma_beta = 0.5
gamma_K = 0.05


# Rate functions
def alpha_n(v):
    if abs(v - 10) < 1e-6:
        return 0.1
    return 0.01 * (10 - v) / (np.exp((10 - v) / 10) - 1)


def beta_n(v):
    return 0.125 * np.exp(-v / 80)


def alpha_m(v):
    if abs(v - 25) < 1e-6:
        return 1.0
    return 0.1 * (25 - v) / (np.exp((25 - v) / 10) - 1)


def beta_m(v):
    return 4 * np.exp(-v / 18)


def alpha_h(v):
    return 0.07 * np.exp(-v / 20)


def beta_h(v):
    return 1 / (np.exp((30 - v) / 10) + 1)


# User inputs
resting_potential = float(
    input("Resting potential value (0 recommended): ")
)

while resting_potential < 0:
    print("Resting potential cannot be negative.")
    resting_potential = float(
        input("Resting potential value: ")
    )

currentinput_start = float(
    input("Current starts at (ms): ")
)

while currentinput_start < 0:
    print("Start time cannot be negative.")
    currentinput_start = float(
        input("Current starts at (ms): ")
    )

currentinput_end = float(
    input("Current ends at (ms): ")
)

while currentinput_end <= currentinput_start:
    print("End time must be greater than start time.")
    currentinput_end = float(
        input("Current ends at (ms): ")
    )

inputcurrent = float(
    input("Current amplitude: ")
)

while inputcurrent < 0 or inputcurrent > 50:
    print("Current must be between 0 and 50.")
    inputcurrent = float(
        input("Current amplitude: ")
    )

# Simulation length
ms_after_currentinput_end = 200
time_end = currentinput_end + ms_after_currentinput_end

# Initial conditions
voltage = resting_potential

n = 0.317
m = 0.053
h = 0.596

times = []
voltages = []
currents = []
spikes = []

# Simulation loop
for t in np.arange(0, time_end, dt):

    current = (
        inputcurrent if currentinput_start <= t <= currentinput_end else 0)
    
    # Modified potassium gating equation
    dn = gamma_K * ( gamma_alpha * alpha_n(voltage) * (1 - n) - gamma_beta * beta_n(voltage) * n)

    # Standard HH equations
    dm = (alpha_m(voltage) * (1 - m) - beta_m(voltage) * m)

    dh = (alpha_h(voltage) * (1 - h) - beta_h(voltage) * h)

    n += dn * dt
    m += dm * dt
    h += dh * dt

    # Ionic currents
    INa = gNa * (m ** 3) * h * (voltage - eNa)
    IK = gK * (n ** 4) * (voltage - eK)
    IL = gL * (voltage - eL)

    # Voltage update
    dV = current - INa - IK - IL

    old_voltage = voltage
    voltage += dV * dt

    # Spike detection
    if old_voltage < 50 and voltage >= 50: spikes.append(round(t, 2))

    times.append(t)
    voltages.append(voltage)
    currents.append(current)

# Output
print("\nCurrent starts at:", currentinput_start)
print("Current ends at:", currentinput_end)
print("Total simulation time:", time_end)
print("Spike times:", spikes)
print(f"Total spikes observed: {len(spikes)}")

# Plot
plt.figure(figsize=(12, 7))

plt.plot(times, voltages,label="Membrane Voltage")

plt.plot(times,currents,label="Input Current")

plt.axvline(currentinput_start,color="green",linestyle=":",label="Current On")

plt.axvline(currentinput_end,color="red",linestyle=":",label="Current Off")

for spike in spikes: plt.axvline(spike,color="purple",linestyle="--",alpha=0.5)

plt.title("Modified Hodgkin-Huxley Neuron Simulation\n" "γα = 0.5, γβ = 0.5, γK = 0.05")

plt.xlabel("Time (ms)")
plt.ylabel("Voltage / Current")

plt.grid(True)
plt.legend()

plt.show()
