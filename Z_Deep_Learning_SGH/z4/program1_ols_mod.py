import numpy as np
import pandas as pd

data=np.array(pd.read_csv('data.csv',header=None))
X = np.concatenate((np.ones((data.shape[0],1)),data[:,:2]), axis=1)
y = data[:,2]

# alpha_ols = np.matmul(np.matmul(np.linalg.inv(np.matmul(X.T,X)), X.T),y)
alpha_ols = np.linalg.solve(np.matmul(X.T,X), np.matmul(X.T,y))
alpha = np.random.randn(X.shape[1])
alpha_old = np.inf

learning_rate = 0.01
it = 0
while np.linalg.norm(alpha_old - alpha)>0.0000000001:
    it += 1
    alpha_old = alpha
    gradient_alfa = (- 2 * np.matmul(X.T,y)) + 2 * np.matmul(np.matmul(X.T,X), alpha)
    alpha = alpha - learning_rate * gradient_alfa

print(alpha_ols)
print(alpha)
print(f'in {it} iteration')