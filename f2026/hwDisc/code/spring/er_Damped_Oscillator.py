# Dependencies
import numpy as np
import matplotlib.pyplot as plt

# Initial variables
t = np.linspace(0, 20, 2000) # Time array
p = 3.0  # Spring constant
x = [0] # Initial position
v = [1] # Initial velocity
b = 0.5 # Damping coefficient

# Solve Spring DE
for i in range(len(t)-1):
    dxdt = v[i]
    dvdt = -p * x[i] - b * v[i]  # Damping term added
    x.append(x[i] + dxdt * t[-1]/len(t))
    v.append(v[i] + dvdt * t[-1]/len(t))

# Plot solution
plt.plot(t, x)
plt.title('Spring Motion Differential Equation Plot')
plt.xlabel('Time')
plt.ylabel('Position')
plt.show()