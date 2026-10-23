#E325D - potassium channel defect

import matplotlib.pyplot as plt
from math import exp

# time settings
dt = 0.01
startTime = 0
endTime = 100
time_list = [startTime]

volt = 0.0
current = 0 

#tracking lists
volt_list = [volt]
time_list = [0]



# reverse potentials
ena = 115   # sodium
ek = -12    # potassium
el = 10.6   # leak

# conductance
gna = 120
gk = 36
gl = 0.3

#mutation parameters
gamma_tau = 0.08
gamma_alpha = 6
gamma_beta = 6 
delta_V = 64


def an(V):
	return (0.01 * (10 - V)) / (exp((10 - V) / 10 ) - 1)

def am(V):
	return (0.1 * (25 - V)) / (exp((25 - V) / 10) -1)

def ah(V):
	return 0.07 * exp(-V / 20)

def bn(V):
	return 0.125 * exp(-V / 80)

def bm(V):
	return 4 * exp(-V / 18)

def bh(V):
	return 1 / (exp((30 - V) / 10) + 1)



m = am(volt) / (am(volt) + bm(volt))
n = an(volt) / (an(volt) + bn(volt))
h = ah(volt) / (ah(volt) + bh(volt))



#Time loop

t = startTime
while t < endTime:


	#x-dot rates
	mdot = am (volt) * (1 - m) - bm (volt) * m
	hdot = ah (volt) * (1 - h) - bh (volt) * h

	# Gating variable Euler update
	m = m + (mdot * dt)
	h = h + (hdot * dt)

	#stimulus current pulse
	if 10.0 <= t <= 20.0:
		current = 10.0
	else:
		current = 0.0
	
	#disease formula
	dndt = gamma_tau * (
    		gamma_alpha * an(volt - delta_V) * (1.0 - n)
    		- gamma_beta * bn(volt - delta_V) * n
	)

	#update n
	n = n + dndt * dt

	#massive formula
	dvdt = current - (
		(gna * (m ** 3) * h * (volt - ena)) 
		+ (gk * (n ** 4) * (volt - ek))
		+ (gl * (volt - el))
	)

	# Voltage Euler update
	volt = volt + (dvdt * dt)

	#save history
	time_list.append(t)
	volt_list.append(volt)

	#step time forward
	t = t + dt

#Plotting voltage over time
plt.figure(figsize=(10, 4))
plt.plot(time_list, volt_list, label = 'E325D', color = "red")
plt.xlabel('Time')
plt.ylabel('Voltage')
plt.title('Diseased E325D')
plt.legend()
plt.grid(True)
plt.show()