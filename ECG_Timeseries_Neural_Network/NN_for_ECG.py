# ---------------------------------------------
# ECG Heartbeat Categorization Dataset
# https://www.kaggle.com/shayanfazeli/heartbeat
#
# Arrhythmia Dataset
# Classes: ['N': 0, 'S': 1, 'V': 2, 'F': 3, 'Q': 4]
# Pliki: mitbih_train.csv & mitbih_test.csv
# ---------------------------------------------


import numpy as np
import pandas as pd
import tensorflow as tf
import matplotlib.pyplot as plt

train_raw = pd.read_csv("mitbih_train.csv", header=None)
print(train_raw.head())
plt.hist(train_raw.iloc[:,-1], bins=[-0.5, 0.5, 1.5, 2.5, 3.5, 4.5])
plt.show()

test_raw = pd.read_csv("mitbih_test.csv", header=None)
print(test_raw.head())
plt.hist(test_raw.iloc[:,-1], bins=[-0.5, 0.5, 1.5, 2.5, 3.5, 4.5])
plt.show()