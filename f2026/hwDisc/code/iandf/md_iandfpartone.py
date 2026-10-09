import numpy as np

k = 4.0      # spring constant (N/m)   

dt = 0.05    # time step (s)
t_max = 10   # total simulation time (s)
initt = 0.0  # initial time (s)
starttime = 1.0
stoptime = 6.0 
cap = 1.0 
res = 2.0 
threshold = 2.0
spikedisplay = 8.0
initv = 0.0 
voltage = initv 
injectioncurrent = 4.3
injectiontime = [starttime, stoptime]
iandftau = res*cap 
runningTime = [0.0]
time = np.arange(initt, t_max, dt)

voltage = np.zeros(len(time))
voltage[0] = initv

injectioncurrent = np.zeros(len(time))

for i in range(len(time)):
    if starttime <= time[i] <= stoptime:
        injectioncurrent[i] = 4.3


for i in range(len(time)-1):
    dVdt = (1/iandftau)*(res * injectioncurrent[i] - k*voltage[i])
    voltage[i+1] = voltage[i] + dVdt * dt

    if voltage[i+1] >= threshold:
        voltage[i] = spikedisplay
        voltage[i+1] = initv

print(dVdt)