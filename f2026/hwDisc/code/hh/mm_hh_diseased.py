## Hafez & Gottschalk Hodgkin-Huxley Modification
# Episodic Ataxia Type 1
# Kv1.1 Channel Variant F184C 

import numpy as np 
import matplotlib.pyplot as plt  

t_init = 0.0
t_final = 50.0
t_step = 0.01 

E_Na   = 115   # mV
E_K    = -12   # mV
E_L    = 10.6  # mV

## F184C Variant Properties 
gamma_tau = 1.0 # time constant modulator
gamma_alpha = 0.5 # activation modulator
gamma_beta = 1.0 # deactivation modulator 
gamma_K = 0.15 # channel conductance modulator 
delta_V = 27.0 # mV, voltage sensitivity modulator 

## Wildtype K+ Channel Properties
# gamma_tau = 1.0 # time constant modulator
# gamma_alpha = 1.0 # activation modulator
# gamma_beta = 1.0 # deactivation modulator 
# gamma_K = 1.0 # channel conductance modulator 
# delta_V = 0 # mV, voltage sensitivity modulator 

g_Na   = 120   # mS/cm^2
g_K    = 36    # mS/cm^2
g_L    = 0.3   # mS/cm^2
C = 1.0 # uF/cm^2

t = np.arange(t_init, t_final + t_step, t_step)

I = np.zeros(len(t))
V = np.zeros(len(t))
V[0] = 0.0 # initial voltage

n = np.zeros(len(t))
m = np.zeros(len(t))
h = np.zeros(len(t))

## n associated with K+
def alpha_n(V): 
   alpha_n = (0.1 - 0.01 * V) / (np.exp(1 - 0.1 * V) - 1) if V != 10 else 0.1
   return alpha_n
def beta_n(V):
   beta_n  = 0.125 * np.exp(-V / 80)
   return beta_n

## m associated with Na+ 
def alpha_m(V):
   alpha_m = (2.5 - 0.1 * V) / (np.exp(2.5 - 0.1 * V) - 1) if V != 25 else 1
   return alpha_m
def beta_m(V):
   beta_m  = 4 * np.exp(-V / 18)
   return beta_m

# h associated with Na+ 
def alpha_h(V):
   alpha_h = 0.07 * np.exp(-V / 20)
   return alpha_h
def beta_h(V):
   beta_h = 1 / (np.exp(3 - 0.1 * V) + 1)
   return beta_h
      
for i in range(len(t) - 1):
   if 5.0 <= t[i] <= 40.0:
      I[i] = 20.0

   n[i + 1] = n[i] + t_step * gamma_tau * (gamma_alpha * alpha_n(V[i] - delta_V) * (1 - n[i]) - 
                                           gamma_beta * beta_n(V[i] - delta_V) * n[i])

   ## Same as in the healthy neuron, as m and h affect Na+ channel behaviour
   m[i + 1] = m[i] + t_step * (alpha_m(V[i]) * (1 - m[i]) - beta_m(V[i]) * m[i])
   h[i + 1] = h[i] + t_step * (alpha_h(V[i]) * (1 - h[i]) - beta_h(V[i]) * h[i])

   V[i + 1] = V[i] + (I[i] - 
                      (g_Na * (m[i] ** 3) * h[i] * (V[i] - E_Na) + 
                       gamma_K * g_K * (n[i] ** 4) * (V[i] - E_K) + 
                       g_L * (V[i] - E_L))) / C * t_step 
   
plt.plot(t, V, label = "Voltage (mV)", color = 'hotpink')
plt.plot(t, I, label = "Current (A)", color = 'pink')

plt.title("Hafez & Gottschalk's Hodgkin-Huxley Neuron Model")
plt.xlabel("Time (ms)")

plt.legend()
plt.grid(True)
plt.show()