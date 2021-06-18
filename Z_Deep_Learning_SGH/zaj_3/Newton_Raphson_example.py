import matplotlib.pyplot as plt
import numpy as np

def fx(function_formula, x):
    return eval(function_formula)

def estimate_derivate(function_formula, point):
    eps = 0.0000001
    x_lower, x_upper = point - eps, point + eps
    y_lower = fx(function_formula,  x_lower)
    y_upper = fx(function_formula, x_upper)
    return (y_upper-y_lower)/(x_upper-x_lower)


function_formula = '((x-2+np.sin(x-2))*(x+30))**2'
x_values = np.linspace(-14,14,1000)
y_falues = fx(function_formula, x_values)


x =13
y = fx(function_formula, x)
plt.plot(x_values, y_falues)
plt.plot(x, y, marker = 'o', linestyle = 'None')
plt.pause(0.5)

max_iter = 100
iter = 0
while (np.abs(y)>0.000000001) and (iter < max_iter):
    iter += 1
    y_prim = estimate_derivate(function_formula, x)
    x_new = x - y / y_prim
    y_new = fx(function_formula, x_new)

    plt.plot(x_new, y_new, marker = 'o', linestyle = 'None')

    x = x_new
    y = y_new
    plt.pause(0.5)

# plt.show()
print(f'wynik x = {x_new}, f(x) = {y_new}, w {iter} iteracjach')

plt.plot(x_values, y_falues)
plt.show()

