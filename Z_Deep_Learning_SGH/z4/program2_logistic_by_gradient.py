import numpy as np
import pandas as pd
import statsmodels.api as sm

data=np.array(pd.read_csv('data.csv',header=None))
X = np.concatenate((np.ones((data.shape[0],1)),data[:,:2]), axis=1)
y = data[:,2]
glm_binom = sm.GLM(y, X, family=sm.families.Binomial())
res = glm_binom.fit()

alfa = np.random.randn(X.shape[1])
learning_rate = 0.01
for i in range(10000):
    p = (np.exp(np.dot(X, alfa)) / (1 + np.exp(np.dot(X, alfa))))
    gradient_alfa = -np.matmul(X.T, y-p)
    alfa = alfa - learning_rate * gradient_alfa

print(res.params)
print(alfa)