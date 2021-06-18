import numpy as np
import tensorflow as tf
import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import MinMaxScaler
import os
import pickle

def make_dir_if_not_exist(path):
    if not os.path.exists(path):
        os.makedirs(path)
    return path

tf.compat.v1.disable_eager_execution()

model_path = make_dir_if_not_exist('model/')

np.random.seed(seed=10)

file = 'data_iris.csv'
df = pd.read_csv(file, sep = ';')
scaler = MinMaxScaler()

X = np.array(df.iloc[:,:-1])
y = np.array(df.iloc[:,-1])-1


X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=0)
X_train = scaler.fit_transform(X_train)
# scaler.fit(X_train)
# X_train = scaler.transform(X_train)

with open(model_path + 'scaler.pickle', 'wb') as f:
    pickle.dump(scaler, f)

np.save('X_test',X_test)
np.save('y_test',y_test)

X_test = scaler.transform(X_test)

n_input = X_train.shape[1]

learning_rate = 0.01
training_epochs = 1000
n_hidden_1 = 3
n_hidden_2 = 2

batch_len = 17
no_of_batches = np.linspace(0, X_train.shape[0], int(np.round(X_train.shape[0] / batch_len) + 1)).astype(np.int)


num_classes = len(np.unique(y_train))
y_train_one_hot = np.eye(num_classes)[y_train]

X_pl = tf.compat.v1.placeholder(tf.float32, [None, n_input], name = 'X')
y_pl = tf.compat.v1.placeholder(tf.float32, [None, num_classes])

scale = 100.0
W1 = tf.Variable(tf.compat.v1.random.normal([n_input, n_hidden_1])/scale)
b1 = tf.Variable(tf.compat.v1.random.normal([n_hidden_1])/scale)

W2 = tf.Variable(tf.compat.v1.random.normal([n_hidden_1, n_hidden_2])/scale)
b2 = tf.Variable(tf.compat.v1.random.normal([n_hidden_2])/scale)

W3 = tf.Variable(tf.compat.v1.random.normal([n_hidden_2, num_classes])/scale)
b3 = tf.Variable(tf.compat.v1.random.normal([num_classes])/scale)


def multilayer_percetron(x):
    layer1 = tf.sigmoid(tf.add(tf.matmul(x, W1), b1))
    # layer1 = tf.sigmoid(tf.matmul(x, W1) + b1)
    layer2 = tf.sigmoid(tf.add(tf.matmul(layer1, W2), b2))
    out_layer = tf.add(tf.matmul(layer2, W3), b3) # logits
    return out_layer


logits = multilayer_percetron(X_pl)
loss_op = tf.reduce_mean(tf.nn.softmax_cross_entropy_with_logits(logits=logits, labels=y_pl))
optimizer = tf.compat.v1.train.AdamOptimizer(learning_rate=learning_rate)
training_op = optimizer.minimize(loss_op)
init = tf.compat.v1.global_variables_initializer()

y_est_cont = tf.nn.softmax(logits, name = 'y_est_cont')
y_est_disc = tf.argmax(y_est_cont, axis= 1, name = 'y_est_disc')
# y_est_disc = tf.argmax(logits, axis= 1, name = 'y_est_disc')

nr = np.arange(X_train.shape[0])

with tf.compat.v1.Session() as sess:
    sess.run(init)
    saver = tf.compat.v1.train.Saver()
    for epoch in range(training_epochs):
        np.random.shuffle(nr)
        for i in range(len(no_of_batches) - 1):
            batch_begin = no_of_batches[i]
            batch_end = no_of_batches[i + 1]
            # input(f"epoka = {epoch}, batch = {i}, batch_begin = {batch_begin}, batch_end = {batch_end}")

            X_batch = X_train[nr[batch_begin:batch_end]]
            y_batch = y_train_one_hot[nr[batch_begin:batch_end]]

            _, loss_ = sess.run([training_op,loss_op], feed_dict={X_pl: X_batch, y_pl:y_batch})
            # print(f"epoch {epoch}, batch {i}, loss = {loss_}")
        y_est_disc_ = sess.run(y_est_disc, feed_dict={X_pl: X_test})
        acc_test = np.mean(y_est_disc_==y_test)
        print(f"epoch {epoch}, ACC of test data = {acc_test}")

    y_est_disc_, y_est_cont_ = sess.run([y_est_disc, y_est_cont], feed_dict={X_pl: X_test})
    print(f"after training on {epoch} epochs:")
    print("continuous estimations:")
    print(y_est_cont_)
    print(f" ACC of testing data = {np.mean(y_est_disc_==y_test)}")
    saver.save(sess, model_path + 'model_mlp')