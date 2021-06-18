import numpy as np
import pandas as pd
from scipy import stats

df = pd.read_csv('data_iris.csv', sep =';')
X = np.array(df)

nr = np.arange(X.shape[0])
np.random.shuffle(nr)

training_ratio = 0.7
X = X[nr]
nr_split = np.round(X.shape[0]*training_ratio,0).astype(np.int)

X_train = X[:nr_split,:-1]
y_train = X[:nr_split,-1]
X_test = X[nr_split:,:-1]
y_test = X[nr_split:,-1]


def knn_classification(X_train, y_train, X_test, k):
    X_train1 = np.expand_dims(X_train, axis=0)
    X_test1 = np.expand_dims(X_test, axis=1)
    X_tensor = X_test1 - X_train1

    D = np.sqrt(np.sum(np.power(X_tensor, 2), axis=2))
    nr = np.argsort(D, axis=1)
    return stats.mode(y_train[nr][:, :k], axis=1)[0][:, 0]

y_test_est = knn_classification(X_train, y_train, X_test, k=3)
acc = np.mean(y_test==y_test_est)
print(acc)

# y_test_est = knn_classification(X_train, y_train, X_train, k=1)
# acc = np.mean(y_train==y_test_est)
# print(acc)