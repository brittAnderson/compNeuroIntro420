# -*- coding: utf-8 -*-
"""
Created on Thu Oct 29 21:40:52 2026

@author: rukhs
"""

# Name: Hijjab Lariab Shah
# Course: Intro to Computational Neuroscience Methods
# Assignment: Hodgkin-Huxley Diseased Model (Potassium Channelopathy / EA1)

import numpy as np
import matplotlib.pyplot as plt

# --- 1. Simulation Setup & Diseased Parameters ---
dt = 0.01          # Time step (ms)
T = 50.0           # Total simulation time (ms)
steps = int(T / dt)

C_m = 1.0          # Membrane capacitance (uF/cm^2)

# --- DISEASED MODIFICATION ---
# Normal g_K is 36.0. We model a loss-of-function genetic mutation 
# (such as KCNA1 mutation in Episodic Ataxia Type 1) by severely reducing 
# the maximum potassium conductance to g_K = 8.0. Fewer functional channels 
# mean the cell loses its primary "brakes" for repolarization.
g_Na = 120.0
g_K = 8.0          # <-- REDUCED due to simulated channel defect!
g_L = 0.3

# Reversal potentials (mV)
E_Na = 50.0
E_K = -77.0
E_L = -54.387

# --- 2. Gating Variable Rate Functions ---
def alpha_m(V):
    return 0.1 * (V + 40.0) / (1.0 - np.exp(-(V + 40.0) / 10.0))

def beta_m(V):
    return 4.0 * np.exp(-(V + 65.0) / 18.0)

def alpha_h(V):
    return 0.07 * np.exp(-(V + 65.0) / 20.0)

def beta_h(V):
    return 1.0 / (1.0 + np.exp(-(V + 35.0) / 10.0))

def alpha_n(V):
    return 0.01 * (V + 55.0) / (1.0 - np.exp(-(V + 55.0) / 10.0))

def beta_n(V):
    return 0.125 * np.exp(-(V + 65.0) / 80.0)

# --- 3. Initialization ---
time = np.arange(0, T, dt)
V = -65.03  # Initial resting membrane potential (mV)

m = alpha_m(V) / (alpha_m(V) + beta_m(V))
h = alpha_h(V) / (alpha_h(V) + beta_h(V))
n = alpha_n(V) / (alpha_n(V) + beta_n(V))

voltages = []
currents = []

# --- 4. Simulation Loop (Euler's Method) ---
for t in time:
    if 10.0 <= t <= 30.0:
        I_inj = 10.0  
    else:
        I_inj = 0.0
    currents.append(I_inj)
    
    # Calculate ionic currents with the impaired potassium conductance
    I_Na = (g_Na * (m**3) * h) * (V - E_Na)
    I_K  = (g_K * (n**4)) * (V - E_K)  # Uses our lowered g_K = 8.0
    I_L  = g_L * (V - E_L)
    
    dV = (I_inj - (I_Na + I_K + I_L)) / C_m * dt
    V += dV
    voltages.append(V)
    
    dm = (alpha_m(V) * (1.0 - m) - beta_m(V) * m) * dt
    dh = (alpha_h(V) * (1.0 - h) - beta_h(V) * h) * dt
    dn = (alpha_n(V) * (1.0 - n) - beta_n(V) * n) * dt
    
    m += dm
    h += dh
    n += dn

# --- SCIENTIFIC EXPLANATION (Required Code Comment) ---
# WHY DOES THE DISEASED VOLTAGE TRACE LOOK DIFFERENT?
# Because g_K is reduced from 36.0 to 8.0, the outward potassium current (I_K) 
# is significantly weakened. During an action potential, potassium is responsible 
# for repolarizing the membrane back down to rest. With fewer functional channels, 
# the outward positive current cannot easily counter the inward sodium current. 
# Consequently, the action potential repolarizes much more slowly, appears 
# abnormally broadened/widened, and can lead to membrane hyperexcitability 
# and repetitive firing behaviors characteristic of potassium channelopathies.

# --- 5. Plotting Results ---
fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(10, 6), sharex=True)

ax1.plot(time, voltages, color='purple', linewidth=2, label='Diseased ($g_K = 8.0$)')
ax1.set_ylabel('Membrane Voltage (mV)')
ax1.set_title('Hodgkin-Huxley Diseased Model (Potassium Channel Defect)')
ax1.legend()
ax1.grid(True)

ax2.plot(time, currents, color='dodgerblue', linewidth=2)
ax2.set_ylabel('Injected Current ($\mu$A/cm$^2$)')
ax2.set_xlabel('Time (ms)')
ax2.grid(True)

plt.tight_layout()
plt.show()