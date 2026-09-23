import matplotlib.pyplot as plt
import numpy as np

x1 = np.linspace(-4, -0.01, 200)
x2 = np.linspace(0.01, 4, 200)

fig = plt.figure(figsize=(10, 5))
plt.plot(x1, np.exp(1/x1), color='C0')
plt.plot(x2, np.exp(1/x2), color='C0')
plt.ylim(0, 4)
plt.axvline(0, color='gray', linestyle='--', alpha=0.5)
plt.show()