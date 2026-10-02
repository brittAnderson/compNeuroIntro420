import matplotlib.pyplot as plt

def get_derivatives(x, v):
    dxdt = v
    dvdt = -1.0 * x
    return dxdt,dvdt

t = 0.0
x = 2.0
v = 0.0
h = 0.1
t_end = 100.0

t_list = []
x_list = []

while t <= t_end:
    t_list.append(t)
    x_list.append(x)

    dxdt, dvdt = get_derivatives(x, v)

    v = v + h * dvdt
    x = x + h * v
    t += h

plt.figure(figsize=(10,5))
plt.plot(t_list, x_list, color='b', linewidth=2, label='Spring Position')

plt.axhline(0, color='gray', linestyle='--', alpha=0.7, label='Resting Point')
plt.title('Simple spring', fontsize=14)
plt.xlabel('Time', fontsize=12)
plt.ylabel('Position', fontsize=12)
plt.grid(True, linestyle=':', alpha=0.6)
plt.legend(loc='upper right')

plt.show()