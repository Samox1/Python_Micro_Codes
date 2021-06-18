import numpy as np
from scipy import stats
import pandas as pd


df = pd.read_csv('data_iris.csv', sep = ';')
X = np.array(df)


X_train = X[:, :-1]
y_train = X[:, -1]

def knn_classifier(X_train, y_train, X_test, k):
    X_tensor = np.expand_dims(X_test, axis=1) - np.expand_dims(X_train, axis = 0)
    D = np.sqrt(np.sum(np.power(X_tensor, 2), axis=2))
    nr = np.argsort(D, axis = 1)
    return stats.mode(y_train[nr][:,:k], axis = 1)[0][:,0]

y_train_est = knn_classifier(X_train, y_train, X_train, k = 1)
acc = np.mean(y_train == y_train_est)
print(acc)

