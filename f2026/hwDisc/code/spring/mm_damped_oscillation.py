import numpy as np
import matplotlib.pyplot as plt

P = 2.0 # Spring constant
k = 0.1 # Damping constant

t_step = 0.005 # time step interval is smaller to compute more values 
t_end = 100

time = np.arange(0, t_end + t_step, t_step)

## Make arrays to store step values over time, go Euler!

s = np.zeros(len(time)) 
v = np.zeros(len(time))

s[0] = 10.0 # Initial position 
v[0] = 0.0 # Initial velocity 

for i in range(len(time) - 1):
    ds_dt = v[i]
    d2s_dt2 = -P * s[i] - k * v[i]

    s[i + 1] = s[i] + ds_dt * t_step 
    v[i + 1] = v[i] + d2s_dt2 * t_step 

plt.plot(time, s, label = "Position s(t) - Displacement", color = 'mediumvioletred')
# plt.plot(time, v, label = "Velocity v(t) - Speed", color = 'lightpink')

plt.title("Position of a Damped Spring over Time")
plt.xlabel("Time")

plt.legend()
plt.grid(True)
plt.show()