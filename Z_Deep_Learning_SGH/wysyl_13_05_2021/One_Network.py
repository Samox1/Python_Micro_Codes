import tensorflow as tf
from tensorflow.keras.models import Model
from tensorflow.keras import layers, losses
import numpy as np
from utils import softmax


class One_Network(Model):
    def __init__(self, factivation, **kwargs):
        super().__init__(**kwargs)
        self.conv1 = layers.Conv2D(32, (3, 3), activation=factivation, padding='same')
        self.pool1 = layers.MaxPool2D(pool_size=(3, 3), strides=2, padding='valid')

        self.conv2 = layers.Conv2D(32, (3, 3), activation=factivation, padding='same')
        self.pool2 = layers.MaxPool2D(pool_size=(3, 3), strides=2, padding='valid')

        self.conv3 = layers.Conv2D(64, (3, 3), activation=factivation, padding='same')
        self.pool3 = layers.MaxPool2D(pool_size=(3, 3), strides=2, padding='valid')

        self.flat = layers.Flatten()

        self.dense1 = layers.Dense(units=64, activation=factivation)

        self.dense2 = layers.Dense(units=2, activation=factivation)

    def call(self, x):
        x = self.conv1(x)
        x = self.pool1(x)

        x = self.conv2(x)
        x = self.pool2(x)

        x = self.conv3(x)
        x = self.pool3(x)

        x = self.flat(x)

        x = self.dense1(x)

        x = self.dense2(x)
        return x


    def predict(self, X):
        y_test_est_continous = softmax(self.call(X).numpy())
        return y_test_est_continous
