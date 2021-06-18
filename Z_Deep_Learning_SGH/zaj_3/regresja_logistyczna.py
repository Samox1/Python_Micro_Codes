import pandas as pd
import numpy as np
import statsmodels.api as sm
from sklearn import linear_model

data=pd.read_csv('dane.csv',header=None) #wczytanie danych


def regresja_logistyczna(x,y):
    X = np.concatenate((np.ones((x.shape[0], 1)), x), axis=1)
    w = np.zeros((X.shape[1]))
    w_old = w+np.inf
    while np.linalg.norm(w-w_old)>0.00001:
        w_old = w
        p=(np.exp(np.dot(X,w))/(1+np.exp(np.dot(X,w))))
        V=np.diag(p*(1-p))
        w = w+np.linalg.solve(np.dot(np.dot(X.T,V),X), np.dot(X.T,(y-p)))
    return w


w = regresja_logistyczna(data.iloc[:,:2],data.iloc[:,2])
print('nasze rozwiazanie:')
print(w)
print('-'*10)
X=data.iloc[:,:2].to_numpy()
X = np.concatenate(( np.ones((X.shape[0], 1)), X), axis=1)
y=data.iloc[:,2].to_numpy()
glm_binom = sm.GLM(y, X, family=sm.families.Binomial())
res = glm_binom.fit()
print("stats models:")
print(res.params)

print(" scikit-learn:")
logreg = linear_model.LogisticRegression(C=1e100)
logreg.fit(X, y)
b = logreg.coef_[0].copy()
b[0] *= 2
print(b)



print('zestawienie wartosci prawdopodobienstw (nasze_rozwiazanie, statsmodels, scklarn) oraz wektora y')
y_est_nasze_rozwiazanie = 1 / (1 + np.exp(-(np.dot(X, w))))
y_est_sm=res.predict(X)
y_est_sklearn=logreg.predict_proba(X)[:,1]

print('-'*10)
print('jaka DataFrame:')
wyniki = pd.DataFrame(data = np.stack([y_est_nasze_rozwiazanie, y_est_sm, y_est_sklearn, y], axis = 1),columns = ['hm lr', 'statsmodels', 'scklarn', 'y'])
print(wyniki)
# print(np.stack([y_est_nasze_rozwiazanie, y_est_sm, y_est_sklearn, y], axis = 1))
