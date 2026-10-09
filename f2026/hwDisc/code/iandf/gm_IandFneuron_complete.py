import matplotlib.pyplot as plt

def get_derivatives(v):
    dvdt = ((1/f) * ((r * i) - v))
    return dvdt

t = 0.0 #time start
f = 1.0  #tau
v = 0.0 #initial voltage
v_int = 0.0 #resting potential
i_start = 1 #time current starts
i_end = 5 #time current ends
i = 4.0  #input current
max_v = 3 #threshold
r = 1.0 #resistance
t_end = 15.0 #time end
h = 0.1 #step size

t_list = []
v_list = []

while t <= t_end: #tells when current starts and ends
    if t >= i_end: #when current is shutoff, the voltage decays exponentially. I believe this is because it is modelled by a differential equation, and physically I believe as voltage is lost from the neuron, more and more of the ion channels will close causing more voltage to be lost in an exponential feedforward loop.
        i = 0
    if t >= i_start: #I cannot figure out how to fix this to be 0 before current injection, so I have left it as i
        i = i

    if v >= max_v: #causes spike and reset
        v = 8
        t_list.append(t)
        v_list.append(v)
        v = v_int

    t_list.append(t)
    v_list.append(v)
    

    dvdt = get_derivatives(v)
    v = v + dvdt * h
    t += h

if v >= max_v:
    v=0

plt.figure(figsize=(10,5))
plt.plot(t_list, v_list, color='b', linewidth=2, label='Action Potential')

plt.axhline(0, color='gray', linestyle='--', alpha=0.7, label='Resting Potential')
plt.title('Integrate and Fire Neuron', fontsize=14)
plt.xlabel('Time', fontsize=12)
plt.ylabel('Voltage', fontsize=12)
plt.grid(True, linestyle=':', alpha=0.6)
plt.legend(loc='upper right')

plt.show()