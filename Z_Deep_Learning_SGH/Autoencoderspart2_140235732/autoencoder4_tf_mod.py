import matplotlib.pyplot as plt
import tensorflow as tf

from tensorflow.keras import layers, losses
from tensorflow.keras.datasets import fashion_mnist
from tensorflow.keras.models import Model
from read_data import read_data
import numpy as np
from sklearn.model_selection import train_test_split

X_all, y_all = read_data(directory='D:/kas/dane_brodawki', height=28, width=28)
X_all = X_all.astype('float32') / 255.

# nr = np.arange(X_all.shape[0])
# np.random.shuffle(nr)
# test_ratio = 0.2
# nr_stop = np.round(len(nr)*test_ratio).astype(int)
# X_train = X_all[nr[nr_stop:]]
# y_train = y_all[nr[nr_stop:]]
# X_test = X_all[nr[:nr_stop]]
# y_test = y_all[nr[:nr_stop]]


X_train, X_test, y_train, y_test = train_test_split(X_all, y_all, test_size=0.2, random_state=42)

noise_factor = 0.2
X_train_noisy = X_train + noise_factor * tf.random.normal(shape=X_train.shape)
X_test_noisy = X_test + noise_factor * tf.random.normal(shape=X_test.shape)

X_train_noisy = tf.clip_by_value(X_train_noisy, clip_value_min=0., clip_value_max=1.)
X_test_noisy = tf.clip_by_value(X_test_noisy, clip_value_min=0., clip_value_max=1.)


class Autoencoder(Model):
    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self.conv1 = layers.Conv2D(16, (3, 3), activation='relu', padding='same', strides=2)
        self.conv2 = layers.Conv2D(8, (3, 3), activation='relu', padding='same', strides=2)

        self.conv1T = layers.Conv2DTranspose(8, kernel_size=3, strides=2, activation='relu', padding='same')
        self.conv2T = layers.Conv2DTranspose(16, kernel_size=3, strides=2, activation='relu', padding='same')
        self.conv3 = layers.Conv2D(3, kernel_size=(3, 3), activation='sigmoid', padding='same')

    def call(self, x):
        x = self.conv1(x)
        x = self.conv2(x)
        x = self.conv1T(x)
        x = self.conv2T(x)
        x = self.conv3(x)
        return x

def loss(model, noisy_original, original):
  reconstruction_error = tf.reduce_mean(tf.square(tf.subtract(model(noisy_original), original)))
  return reconstruction_error


def train(loss, model, opt, noisy_original, original):
  with tf.GradientTape() as tape:
    gradients = tape.gradient(loss(model, noisy_original, original), model.trainable_variables)
    gradient_variables = zip(gradients, model.trainable_variables)
    opt.apply_gradients(gradient_variables)

autoencoder = Autoencoder()



opt = tf.optimizers.Adam(learning_rate=0.001)

batch_size = 16
epochs = 200

training_dataset = tf.data.Dataset.from_tensor_slices((X_train, X_train_noisy)).batch(batch_size).shuffle(buffer_size=128, reshuffle_each_iteration=True)


for epoch in range(epochs):
    for step, (batch_features, batch_features_noisy) in enumerate(training_dataset):
        train(loss, autoencoder, opt, batch_features_noisy, batch_features)
        loss_values = loss(autoencoder, batch_features_noisy, batch_features)

    print(f"epoch = {epoch}, loss = {loss_values}")


decoded_imgs = autoencoder(X_test_noisy).numpy()




n = 10
plt.figure(figsize=(20, 4))
for i in range(n):
    # display original
    ax = plt.subplot(3, n, i + 1)
    plt.title("original")
    plt.imshow(tf.squeeze(X_test[i]))
    plt.gray()
    ax.get_xaxis().set_visible(False)
    ax.get_yaxis().set_visible(False)

    # display original + noise
    ax = plt.subplot(3, n, i + n+ 1)
    plt.title("original + noise")
    plt.imshow(tf.squeeze(X_test_noisy[i]))
    plt.gray()
    ax.get_xaxis().set_visible(False)
    ax.get_yaxis().set_visible(False)

    # display reconstruction
    bx = plt.subplot(3, n, i + 2 * n + 1)
    plt.title("reconstructed")
    plt.imshow(tf.squeeze(decoded_imgs[i]))
    plt.gray()
    bx.get_xaxis().set_visible(False)
    bx.get_yaxis().set_visible(False)
plt.show()