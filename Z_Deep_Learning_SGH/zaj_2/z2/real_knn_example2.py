import numpy as np
import pandas as pd
from scipy import stats
# from sklearn.model_selection import train_test_split
from sklearn.model_selection import KFold
from sklearn.preprocessing import MinMaxScaler
from sklearn.pipeline import Pipeline
from sklearn.neighbors import KNeighborsClassifier

def knn_classification(X_train, y_train, X_test, k):
    X_train1 = np.expand_dims(X_train, axis=0)
    X_test1 = np.expand_dims(X_test, axis=1)
    X_tensor = X_test1 - X_train1
    D = np.sqrt(np.sum(np.power(X_tensor, 2), axis=2))
    nr = np.argsort(D, axis=1)
    return stats.mode(y_train[nr][:, :k], axis=1)[0][:, 0]


df = pd.read_csv('data_iris.csv', sep =';')
X = np.array(df)


# X_train, X_test, y_train, y_test = train_test_split(X[:,:-1], X[:,-1], test_size=0.33)

ACC = []

kf = KFold(n_splits=10, random_state=None, shuffle=True)
for train_index, test_index in kf.split(X):
    X_train, X_test, y_train, y_test = X[train_index, :-1], X[test_index,:-1], X[train_index, -1], X[test_index,-1]
    scaler = MinMaxScaler()
    knn = KNeighborsClassifier(n_neighbors=3, algorithm='brute')
    pipe = Pipeline([('scaler1', scaler), ('knn', knn)])
    pipe.fit(X=X_train, y=y_train)

    # X_train = scaler.fit_transform(X_train)
    # X_test = scaler.transform(X_test)
    # y_test_est = knn_classification(X_train, y_train, X_test, k=3)

    y_test_est = pipe.predict(X = X_test)
    acc = np.mean(y_test==y_test_est)
    ACC.append(acc)

ACC = np.mean(np.array(ACC))
print(ACC)