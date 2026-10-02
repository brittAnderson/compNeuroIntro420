#imports
import matplotlib.pyplot as plt
import numpy as np


#Setup
initV = 0
initLoc = 10
SpringConst = 2
mass = 1
timeStep = 0.05
d = 0.05

time = np.arange(0, 100, timeStep)

Loc = np.zeros_like(time)
Vel = np.zeros_like(time)

Loc[0] = initLoc
Vel[0] = initV


#Spring
for i in range(len(time) - 1):
    accel = (-(SpringConst / mass) * Loc[i]) - ((d / mass) * Vel[i])
    Vel[i+1] = Vel[i] + (accel * timeStep)
    Loc[i+1] = Loc[i] + (Vel[i+1] * timeStep)


#Plotting
plt.plot(time, Loc, label='Oscillating Spring', color='black')
plt.xlabel('Time')
plt.ylabel('Location')
plt.title('Damped Spring Oscillation')
plt.show()
