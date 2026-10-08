# due to this np=numpy
import numpy as np


resting_potential = float(input("Resting potential value: "))    #without voltage this is where neuron will be 
threshold = float(input("Threshold: "))   #the voltage needed to fire, aka afterwhich the graph will spike
tau = 20    # controls how fast voltage changes and as tau gets bigger the voltage or dV gets smaller and vice versa 
dt = 1     # delta time: how much time passes in ms between neuron voltage updates, its like taking a screenshot every this many ms

# this is where the action potential begins 
voltage = resting_potential

# input the current being injected into the neuron
inputcurrent = float(input("Current: "))

# the number you intput into time is how many ms the action potential simulation will run for
time = float(input("Time in ms: "))


# these are the bags we fill
times = []  # stores every time point
voltages = []  # stores the neurons voltage
currents = []
spikes = []   #stores when spikes happen in ms


# np.arange( starting point, ending point, by how much togo up/down) and creates time stamps. 
for t in np.arange(0, time, dt):  #t is the time at whcih we wnat to know the voltage 
    current = inputcurrent  # the extrenal electrical input

    dV = (-voltage + current) / tau   # we are assuming R is 1
    voltage += dV * dt
  # if the voltage reachers the threshold (meaning the neuron fires) the time at which this happens (t) gets stores in spike [] and the voltage is reset to  resting potential
    if voltage >= threshold:
        spikes.append(t)
        voltage = resting_potential

    times.append(t)  #the .append helps put the t at the end of teh list isnetad of overiding the previous t. it records every t (ms) the pass reagurdless of spike or not
    voltages.append(voltage) #keeps track of the voltage at every t (ms) and add oit to voltage []
    currents.append(current) # Keeps track of how much external current was being injected at that moment which is the current = , in line 24

print(f"Total spikes observed: {len(spikes)}")