import numpy as np
import matplotlib.pyplot as plt

n = 5
polynomial_order = n-1

points = np.random.rand(n,2)

coeff = np.polyfit(x=points[:,0], y= points[:,1], deg = polynomial_order)
eps = 0.001
x = np.linspace(np.min(points[:,0])-eps, np.max(points[:,0])+eps, 1000)
y_estimated  = np.polyval(p = coeff, x = x)

plt.plot(x, y_estimated)
plt.plot(points[:,0], points[:,1], marker='.', linestyle = 'None', markersize = 4, color = 'red')
plt.show()

# print(coeff)