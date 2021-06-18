import numpy as np
import pandas as pd

data=np.array(pd.read_csv('data.csv',header=None))
X = np.concatenate((np.ones((data.shape[0],1)),data[:,:2]), axis=1)
y = data[:,2]

# alfa_ols = np.matmul(np.matmul(np.linalg.inv(np.matmul(X.T,X)), X.T),y)
alpha_ols = np.linalg.solve(np.matmul(X.T,X), np.matmul(X.T,y))
alpha = np.random.randn(X.shape[1])

learning_rate = 0.01
for i in range(1000):
    gradient_alpha = (- 2 * np.matmul(X.T,y)) + 2 * np.matmul(np.matmul(X.T,X), alpha)
    alpha = alpha - learning_rate * gradient_alpha

print(alpha_ols)
print(alpha)