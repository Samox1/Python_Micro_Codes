# https://projector.tensorflow.org/
import io
import numpy as np
import tensorflow as tf
import tensorflow_addons as tfa
import tensorflow_datasets as tfds
from sklearn.neighbors import KNeighborsClassifier
from sklearn.preprocessing import MinMaxScaler

def softmax(x):
    """Compute softmax values for each sets of scores in x."""
    e_x = np.exp(x - np.max(x, axis=1, keepdims=True))
    return e_x / np.sum(e_x, axis = 1, keepdims=True)

def _normalize_img(img, label):
    img = tf.cast(img, tf.float32) / 255.
    return (img, label)

train_dataset, test_dataset = tfds.load(name="mnist", split=['train', 'test'], as_supervised=True)

y_test_true = np.array([labels for _, labels in tfds.as_numpy(test_dataset)])
y_train_true = np.array([labels for _, labels in tfds.as_numpy(train_dataset)])

train_dataset = train_dataset.batch(32)
train_dataset = train_dataset.map(_normalize_img)

test_dataset = test_dataset.batch(32)
test_dataset = test_dataset.map(_normalize_img)

class CNN(tf.keras.Model):
    def __init__(self, **kwargs):
        super().__init__(**kwargs) #obsluguje standardowe argumenty (np. name)
        self.conv1 = tf.keras.layers.Conv2D(filters=64, kernel_size=2, padding='same', activation='relu')
        self.pool1 = tf.keras.layers.MaxPooling2D(pool_size=2)
        self.dropout1 = tf.keras.layers.Dropout(0.3)
        self.conv2 = tf.keras.layers.Conv2D(filters=32, kernel_size=2, padding='same', activation='relu')
        self.pool2 = tf.keras.layers.MaxPooling2D(pool_size=2)
        self.dropout2 = tf.keras.layers.Dropout(0.3)
        self.fc = tf.keras.layers.Flatten()
        self.dense1 = tf.keras.layers.Dense(256, activation='relu')
        self.dense2 = tf.keras.layers.Dense(10, activation=None)

    def call(self, x, training=None, mask=None):
        x = self.conv1(x)
        x = self.pool1(x)
        x = self.dropout1(x)
        x = self.conv2(x)
        x = self.pool2(x)
        x = self.dropout2 (x)
        x = self.fc(x)
        x = self.dense1(x)
        x = self.dense2(x)
        return x

model = CNN()

# Compile the model
model.compile(
    optimizer=tf.keras.optimizers.Adam(0.001),
    loss= tf.keras.losses.SparseCategoricalCrossentropy(from_logits=True))

# Train the network
history = model.fit(
    train_dataset.shuffle(1024),
    epochs=5)

# Evaluate the network
results_train = model.predict(train_dataset)
results_test = model.predict(test_dataset)

y_test_pred_cross_entropy = np.argmax(results_test, axis = 1)
# y_test_pred_cross_entropy_continous =  softmax(results_test)
# print(y_test_pred_cross_entropy_continous)
print(f"ACC_cross_entropy = {np.mean(y_test_true==y_test_pred_cross_entropy)}")

# ACC = 0.9891
