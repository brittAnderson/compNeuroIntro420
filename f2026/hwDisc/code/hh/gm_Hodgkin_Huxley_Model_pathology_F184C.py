import matplotlib.pyplot as plt
import math 

#Simulation parameters and neuron dynamics
def get_derivatives(v,m,n,h,i):
    an = (0.01 * (10 - (v - dv))) / (math.exp((10 - (v - dv)) / 10) - 1) # dv is added in the 'n' functions since need as an(v-dv)
    am = (0.1 * (25 -v)) / (math.exp((25 - v) / 10) - 1)
    ah = 0.07 * math.exp(-v / 20)
    bn = 0.125 * math.exp(-(v - dv) / 80)
    bm = 4 * math.exp(-v / 18)
    bh = 1 / (math.exp((30 - v) / 10) + 1)

    mdot = am * (1 - m) - bm * m 
    hdot = ah * (1 - h) - bh * h
    ndot = yt * (ya * an * (1 - n) - yb * bn * n)

    dvdt = (1/cm) * (i-(gna * m**3 * h * (v - ena) + yk * gk * n**4 * (v - ek) + gl * (v - el)))
    return dvdt, mdot, ndot, hdot

# Pathology variables for F184C
ya = 1.0 # Modulates activation
yb = 1.0 # Modulates deactivation
yt = 1.0 # Modulates delayed rectifier channel over constant time
yk = 1.0 # Modulates max channel conductance
dv = 0 # Modulates voltage sensitivity

# simulation Variables
t = 0.0 #time start
v = 0.0 #initial voltage
v_int = 0.0 #resting potential
i = 40.0  #input current
t_end = 50.0 #time end
s = 0.01 #step size

#Constants 
cm = 1.0
ena = 115
gna = 120
ek = -12
gk = 36
el = 10.6
gl = 0.3


t_list = []
v_list = []

# Simulation loop
while t <= t_end:
    t_list.append(t)
    v_list.append(v)

    dvdt, mdot, ndot, hdot = get_derivatives(v,m,n,h,i)
    
    v += dvdt * s
    m += mdot * s
    n += ndot * s
    h += hdot * s
    t += s


plt.figure(figsize=(10,5))
plt.plot(t_list, v_list, color='b', linewidth=2, label='Action Potential')

plt.axhline(0, color='gray', linestyle='--', alpha=0.7, label='Resting Potential')
plt.title('Hodgkin-Huxley Model', fontsize=14)
plt.xlabel('Time (ms)', fontsize=12)
plt.ylabel('Voltage (mV)', fontsize=12)
plt.grid(True, linestyle=':', alpha=0.6)
plt.legend(loc='upper right')

plt.show()
