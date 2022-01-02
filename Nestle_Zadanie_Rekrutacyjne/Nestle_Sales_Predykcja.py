### Nestle - Zadanie rekrutacyjne - Grudzien 2021
### Predykcja na danych - regresja
### Szymon Baczynski

import pandas as pd
import data.table
import numpy as np
import xgboost as xgb
from sklearn import cross_validation




# 1. Wczytanie danych i przeglad
# Pliki treningowe: X_train.csv && Y_train.csv
# Pliki testowe: X_test.csv && Y_test.csv

X_train = pd.read_csv("X_train.csv", delimiter= ";")
X_train.head()
X_train.isnull().values.any()
X_train.isnull().sum()
