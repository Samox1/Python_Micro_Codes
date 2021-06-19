# ---------------------------------------------
# ECG Heartbeat Categorization Dataset
# https://www.kaggle.com/shayanfazeli/heartbeat
#
# Arrhythmia Dataset
# Klasy: ['N': 0, 'S': 1, 'V': 2, 'F': 3, 'Q': 4]
# Pliki: mitbih_train.csv & mitbih_test.csv
# ---------------------------------------------

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import tensorflow as tf
from tensorflow import keras
from tensorflow.python.keras.engine.input_layer import Input
from tensorflow.python.ops.gen_array_ops import shape


### --- Import danych treningowych i testowych --- ###

# Dane Treningowe
train_raw = pd.read_csv("mitbih_train.csv", header=None)
print(train_raw.head())

print("Rozklad klas w danych treningowych")
train_class_spread = train_raw.pivot_table(index = [187], aggfunc ='size')
print(train_class_spread)
plt.title("Rozklad klas w danych treningowych")
plt.hist(train_raw.iloc[:,-1], bins=[-0.5, 0.5, 1.5, 2.5, 3.5, 4.5])
plt.show()


# Dane Testowe
test_raw = pd.read_csv("mitbih_test.csv", header=None)
print(test_raw.head())

print("Rozklad klas w danych testowych")
test_class_spread = test_raw.pivot_table(index = [187], aggfunc ='size')
print(test_class_spread)
plt.title("Rozklad klas w danych testowych")
plt.hist(test_raw.iloc[:,-1], bins=[-0.5, 0.5, 1.5, 2.5, 3.5, 4.5])
plt.show()


# Ze wzgledu na rozklad klas w danych treningowych, nalezy zbalansowac ilosc klas:
# 
# Klasa: 0 = 72471
# Klasa: 1 =  2223
# Klasa: 2 =  5788
# Klasa: 3 =   641
# Klasa: 4 =  6431
#
# - metoda upsamplingu i downsamplingu - ujednolicenie danych na poziomie ...

# Poki uzyte zostana wszystkie dane!!! 




def conv_unit(unit, input_layer):
    s = '_' + str(unit)
    layer = keras.layers.Conv1D(name='Conv1' + s, filters=32, kernel_size=5, strides=1, padding='same', activation='relu')(input_layer)
    layer = keras.layers.Conv1D(name='Conv2' + s, filters=32, kernel_size=5, strides=1, padding='same', activation=None)(layer )
    layer = keras.layers.Add(name='ResidualSum' + s)([layer, input_layer])
    layer = keras.layers.Activation("relu", name='Act_relu' + s)(layer)
    layer = keras.layers.MaxPooling1D(name='MaxPool' + s, pool_size=5, strides=2)(layer)
    return layer


# def cnn_model():
# 
#     current_layer = keras.layers.Conv1D(filters=32, kernel_size=5, strides=1)
# 
#     for i in range(5):
#         current_layer = conv_unit(i + 1, current_layer)
# 
#     current_layer = keras.layers.Dense(32, name='FC1', activation='relu')(current_layer)
#     output_layer = keras.layers.Dense(5, name='Output', activation='softmax')(current_layer)
#     
#     return output_layer

def get_uncompiled_model():

    inputs = keras.Input(shape=(187,), name="digits")
    current_layer = keras.layers.Conv1D(filters=32, kernel_size=5, strides=1)(inputs)

    for i in range(5):
        current_layer = conv_unit(i + 1, current_layer)

    current_layer = keras.layers.Dense(32, name='FC1', activation='relu')(current_layer)
    output_layer = keras.layers.Dense(5, name='Output', activation='softmax')(current_layer)

    model = keras.Model(inputs=inputs, outputs=output_layer)

    return model


def get_compiled_model():
    model = get_uncompiled_model()
    model.compile(
        optimizer='adam', 
        loss=tf.losses.CategoricalCrossentropy(from_logits=True),
        metrics=['accuracy']
    )
    return model


class ConvPart(tf.keras.Model):

  def __init__(self):
    super(ConvPart, self).__init__()

    self.conv_1 = keras.layers.Conv1D(filters=32, kernel_size=5, strides=1, padding='same', activation='relu')
    self.conv_2 = keras.layers.Conv1D(filters=32, kernel_size=5, strides=1, padding='same', activation=None)
    self.add_input = keras.layers.Add()
    self.relu = keras.layers.ReLU()
    self.pool = keras.layers.MaxPooling1D(pool_size=5, strides=2)

  def call(self, input_layer):

    layer = self.conv_1(input_layer)
    layer = self.conv_2(layer)
    layer = self.add_input([layer, input_layer])
    layer = self.relu(layer)
    layer = self.pool(layer)
    return layer



class CnnModel(tf.keras.Model):

  def __init__(self):
    super(CnnModel, self).__init__()

    self.conv_input = keras.layers.Conv1D(filters=32, kernel_size=5, strides=1)
    self.conv_loop = ConvPart()
    self.fc_1 = keras.layers.Dense(32, activation='relu')
    self.fc_2 = keras.layers.Dense(5, activation='softmax')

  def call(self, x_input):
    
    layer = self.conv_input(x_input)
    layer = self.conv_loop(layer)
    layer = self.conv_loop(layer)
    layer = self.conv_loop(layer)
    layer = self.conv_loop(layer)
    layer = self.conv_loop(layer)
    layer = self.fc_1(layer)
    layer = self.fc_2(layer)

    return layer



# input_layer = tf.feature_column.input_layer(features, params['feature_columns'])
# input_layer = tf.expand_dims(input_layer, -1)

model = CnnModel()

model.compile(optimizer='adam', 
              loss=tf.losses.CategoricalCrossentropy(from_logits=True),
              metrics=['accuracy'])

history = model.fit(
    train_raw.iloc[:,-1:], 
    epochs=10, 
    steps_per_epoch=500,
    validation_data=test_raw.iloc[:,-1:], 
    validation_steps=2
)

history.history