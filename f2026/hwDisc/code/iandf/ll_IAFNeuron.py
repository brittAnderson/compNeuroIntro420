#imports
import matplotlib.pyplot as plt
import numpy as np


#setup
timeStep = 0.001
time = np.arange(0, 10, timeStep)

initI = 0               #for spring, was initVel = 0
I = np.zeros_like(time)
I[0] = initI

initV = 0               #for spring, was initLoc = 10
V = np.zeros_like(time)
V[0] = initV

threshold = 3
spike = 8

starttime = 1
stoptime = 7
injectiontime = np.arange(starttime, stoptime, timeStep)

R = 2
C = 1
tau = R * C


#work
for i in range(len(injectiontime) - 1):
    #line = V[i] - (R * I[i]) <-- changed from oscillator example
    if injectiontime[starttime]:
        I[i+1] = I[i] + 4 + (V[i] - (R * I[i]))     #current
        V[i+1] = V[i] + (I[i+1] * timeStep)         #voltage
        if V[i] >= threshold:           #rapid spike after meeting threshold
            V[i+1] = V[i] + (spike * (I[i+1] * timeStep))
        if V[i] >= spike:               #spike peak; resetting
            V[i+1] = 0


#plotting
plt.plot(time, V, label='I&F Neuron', color='black')
plt.xlabel('Time (ms)')
plt.ylabel('Voltage (V)')
plt.title('I&F Neuron')
plt.show()




# Answering question 2
# I was not able to exactly get the tappering off effect that I was hoping to, with a more accurate model
# I assume that a tappering off would be due to the fact that current should raise and lower voltage, and if current was cut off, the voltage would lower, not just immediately fall
