import numpy as np

def softmax(x):
    return np.exp(x)/np.sum(np.exp(x),axis = 1, keepdims=True)


z = np.array([
    [1,1.5,1.4,-2],
    [1.6,1.5,1.4,-2]]
)

# q [0.36351847 0.32892511 0.29762375 0.00993268]]
# p   0          0           1         0


print(f"z = {z}")
print('='*10)
p = softmax(z)
print(f"softmax(x) = {p}")
print(f"sumy prawdopodobienstw = {np.sum(p, axis = 1)}")


