"""
# due to this np=numpy
import numpy as np
import matplotlib.pyplot as plt

# asks the user for the resting potential (the voltage the neuron sits at when no current is applied)
# the resting potential cannot be negative, so the program keeps asking until a value 0 or greater is entered
resting_potential = float(input("Resting potential value: "))
while resting_potential < 0:
    print("resting potential must be greater than 0")
    resting_potential = float(input("Resting potential value:: "))

# the threshold is the voltage required for the neuron to fire a spike
# it must be greater than both 0 and the resting potential and if the input number doenst obide by this the program keeps asking for a new value
threshold = float(input("Threshold: "))
while threshold <= 0 or threshold <= resting_potential:
    print("Threshold must be greater than the resting potential and 0")
    threshold = float(input("Threshold: "))

# controls how fast voltage changes and as tau gets bigger the voltage or dV gets smaller and vice versa
tau = 20

# delta time: how much time passes in ms between neuron voltage updates,
# its like taking a screenshot every this many ms
dt = 1

# this is where the action potential begins
voltage = resting_potential

# gives the current an ending and beginning.
# where neither start or end can be negative and end cant be smaller than start
currentinput_start = float(input("The current input starts at (in ms): "))
while currentinput_start < 0:
    print("Start time cannot be negative.")
    currentinput_start = float(input("The current input starts at (in ms): "))

currentinput_end = float(input("The current input ends at (in ms): "))
while currentinput_end < 0:
    print("End time cannot be negative or smaller than the starting point")
    currentinput_end = float(input("The current input ends at (in ms): "))

while currentinput_end <= currentinput_start:
    print("End time must be greater than start time.")
    currentinput_end = float(input("The current input ends at (in ms): "))

# input the current being injected into the neuron
# the current must be between 0 and 20
# if not, the user is asked to enter another value
inputcurrent = float(input("Current: "))

while inputcurrent < 0 or inputcurrent > 20:
    print("Current must be between 0 and 20.")
    inputcurrent = float(input("Current: "))

# how many ms after the end of the current input we get to see
# (i did this so we can observe the decay)
ms_after_currentinput_end = 200
time = currentinput_end + ms_after_currentinput_end

# these are the bags we fill
times = []      # stores every time point
voltages = []   # stores the neurons voltage
currents = []   # stores the current we are using
spikes = []     # stores when spikes happen in ms

# np.arange(starting point, ending point, by how much to go up/down)
# and creates time stamps
for t in np.arange(0, time, dt):

    # the start, t, and end are all in ms
    # if t is before the start time there is no current
    # if t is after the end time there is no current
    # if t is between or equal to the start and end,
    # the current is applied during those ms
    current = (
        inputcurrent
        if currentinput_start <= t <= currentinput_end
        else 0
    )

    # we assume R is 1
    # thsi is a simplified version of dV = (-voltage + current) / tau and voltage += dV * dt
    voltage += ((-voltage + current) / tau) * dt

    # if the voltage reaches the threshold (meaning the neuron fires)
    # the time at which this happens gets stored in spikes[]
    # and the voltage is reset to resting potential
    if voltage >= threshold:
        spikes.append(float(t))
        voltage = resting_potential

    # the .append helps put the value at the end of the list
    # instead of overriding the previous value it records every t (ms) that passes regardless of spike or not times.append(t)

    # keeps track of the voltage at every t (ms) and adds it to voltages[]
    voltages.append(voltage)

    # keeps track of how much external current was being injected at that moment
    currents.append(current)

print("Current starts at:", currentinput_start)
print("Current ends at:", currentinput_end)
print("The window of time we see in ms:", time)
print("The spikes can be seen at time:", spikes)

# the f is an f string combines the text and variables
# len counts how many items are in the list
print(f"Total spikes observed: {len(spikes)}")

# starts a new graph
plt.figure()

plt.title("Voltage Spike Graph")

# its (x,y) we need the label to know which line is which
# on the graph since they will show up on the same graph
plt.plot(times, voltages, label="Voltage")
plt.plot(times, currents, label="Current")

# axvline plots a vertical line at currentinput_end
# its going to be green, dotted line style,
# and "Current Off" will be in the legend
plt.axvline(
    currentinput_end, color="green", linestyle=":",label="Current Off")

# lists each spike value one at a time the "spike" 
for spike in spikes:
    plt.axvline(spike,color="purple",linestyle="--",alpha=0.5)

plt.xlabel("Time (ms)")
plt.ylabel("Voltage / Current")

plt.legend()
plt.show()
"""

import numpy as np
import matplotlib.pyplot as plt

resting_potential = float(input("Resting potential value: "))
while resting_potential < 0:
    print("resting potential must be greater than 0")
    resting_potential = float(input("Resting potential value:: "))


threshold = float(input("Threshold: "))
while threshold <= 0 or threshold <= resting_potential:
    print("Threshold must be greater than the resting potential and 0")
    threshold = float(input("Threshold: "))
tau = 20
dt = 1

voltage = resting_potential

currentinput_start = float(input("The current input starts at (in ms): "))
while currentinput_start < 0:
    print("Start time cannot be negative.")
    currentinput_start = float(input("The current input starts at (in ms): "))

currentinput_end = float(input("The current input ends at (in ms): "))
while currentinput_end < 0:
    print("End time cannot be negative or smaller than the starting point")
    currentinput_end = float(input("The current input ends at (in ms): "))

while currentinput_end <= currentinput_start:
    print("End time must be greater than the start time ")
    currentinput_end = float(input("The current input ends at (in ms): "))

inputcurrent = float(input("Current: "))

while inputcurrent < 0 or inputcurrent > 20:
    print("Current must be between 0 and 20.")
    inputcurrent = float(input("Current: "))

ms_after_currentinput_end = 200
time = currentinput_end + ms_after_currentinput_end

times = []
voltages = []
currents = []
spikes = []

for t in np.arange(0, time, dt):

    current = (inputcurrent if currentinput_start <= t <= currentinput_end else 0)

    voltage += ((-voltage + current) / tau) * dt

    if voltage >= threshold:
        spikes.append(float(t))
        voltage = resting_potential

    times.append(t)
    voltages.append(voltage)
    currents.append(current)

print("Current starts at:", currentinput_start)
print("Current ends at:", currentinput_end)
print("The window of time we see in ms:", time)
print("The spikes can be seen at time(s):", spikes)
print(f"Total number of spikes observed: {len(spikes)}")

plt.figure()
plt.title("Voltage Spike Graph")

plt.plot(times, voltages, label="Voltage")
plt.plot(times, currents, label="Current")

plt.axvline(currentinput_end,color="green",linestyle=":",label="Current Off")

for spike in spikes:
    plt.axvline(spike,color="purple",linestyle="--",alpha=0.5)

plt.xlabel("Time (ms)")
plt.ylabel("Voltage/Current")

plt.legend()
plt.show()

"""
answer to the question posed: 
After the current input ends, it looks the way it does (like exponential decay) because the voltage does not 
immediately return to the resting potential. This is because the membrane time constant 
tau (as tau gets bigger the voltage or dV gets smaller and vice versa) causes the voltage to change 
gradually rather than instantly. Which results in  the voltage decaying slowly back towards resting potential.
"""