import pandas as pd
import numpy as np
import statsmodels.api as sm
from sklearn import linear_model

data=pd.read_csv('dane.csv',header=None) #loading the data


import numpy as np
def regresja_logistyczna(x,y):
    X = np.asmatrix(np.concatenate((np.ones((x.shape[0], 1)), x), axis=1))
    w = np.asmatrix(np.zeros((X.shape[1],1)))
    y = np.asmatrix(y[:, np.newaxis])
    w_old = w+np.inf
    while np.linalg.norm(w-w_old)>0.00001:
        w_old = w
        p=np.exp(X*w)/(1+np.exp(X*w))
        V=np.asmatrix(np.diag(np.array(np.multiply(p,(1-p)))[:,0]))
        w = w+np.linalg.solve(X.T*V*X, X.T*(y-p))
    return w

w = regresja_logistyczna(data.iloc[:,:2],data.iloc[:,2])
print(w.T)


X=data.iloc[:,:2].to_numpy()
X = np.concatenate(( np.ones((X.shape[0], 1)), X), axis=1)
y=data.iloc[:,2].to_numpy()
glm_binom = sm.GLM(y, X, family=sm.families.Binomial())
res = glm_binom.fit()
print(res.params)


X=data.iloc[:,:2].to_numpy()
X = np.concatenate(( np.ones((X.shape[0], 1)), X), axis=1)
y=data.iloc[:,2].to_numpy()
logreg = linear_model.LogisticRegression(C=1e100)
logreg.fit(X, y)
print(logreg.coef_)
