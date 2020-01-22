import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import matplotlib
from sklearn.tree import DecisionTreeClassifier
from sklearn.tree import plot_tree
from sklearn.metrics import accuracy_score
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import roc_curve, auc
from sklearn.model_selection import cross_val_score
from sklearn.neighbors import KNeighborsClassifier
from sklearn.neighbors import KNeighborsRegressor

Fak_u = pd.read_table('D:/Programming/Cpp_VSCode/Python_Micro_Codes/K-means-Python/fak_u.txt', sep=";", header=None)
Fak_test = pd.read_table('D:/Programming/Cpp_VSCode/Python_Micro_Codes/K-means-Python/fak_wal.txt', sep=";", header=None)

# print(Fak_u)
# print(Fak_test)

### Workin FOR for KNN Regressor
# for x in range(1,120):
#     kNN = KNeighborsRegressor(n_neighbors = x, p=2, n_jobs= -1)
#     kNN.fit(Fak_u.iloc[:,2:], Fak_u.iloc[:,1])
#     scores = kNN.predict(Fak_test.iloc[:,2:])
#     wynik = sum(abs(Fak_test.iloc[:,1] - scores)) / len(scores)
#     print(x, " = " ,wynik)

for x in range(1,120):
    kNN = KNeighborsRegressor(n_neighbors = x, p=2, n_jobs= -1)
    kNN.fit(Fak_u.iloc[:,2:], Fak_u.iloc[:,1])
    scores = kNN.predict(Fak_u.iloc[:,2:])
    wynik = sum(abs(Fak_u.iloc[:,1] - scores)) / len(scores)
    print(x, " = " ,wynik)

# kNN = KNeighborsRegressor(n_neighbors = 42, p=2, n_jobs= -1)
# kNN.fit(Fak_u.iloc[:,2:], Fak_u.iloc[:,1])
# scores = kNN.predict(Fak_test.iloc[:,2:])
# scores_u = kNN.predict(Fak_u.iloc[:,2:])

# do_pliku_u = pd.concat([Fak_u.iloc[:,:2], pd.DataFrame(scores_u)], axis=1, ignore_index=True)
# print(do_pliku_u)
# do_pliku_u.to_csv("Predict_U_id-outU-predict.txt", sep=';', header=None)

# do_pliku_test = pd.concat([Fak_test.iloc[:,:2], pd.DataFrame(scores)], axis=1, ignore_index=True)
# print(do_pliku_test)
# do_pliku_test.to_csv("Predict_Test_id-outTest-predict.txt", sep=';', header=None)

# wynik = sum(abs(Fak_test.iloc[:,1] - scores)) / len(scores)
# print(42, " - Test = " ,wynik)
# wynik_u = sum(abs(Fak_u.iloc[:,1] - scores_u)) / len(scores_u)
# print(42, " - U = " ,wynik_u)
