import numpy as np
import pandas as pd
import tensorflow as tf

tf.compat.v1.disable_eager_execution()

data=np.array(pd.read_csv('data.csv',header=None))
X = np.concatenate((np.ones((data.shape[0],1)),data[:,:2]), axis=1)
y = data[:,2]
print(y.shape)

X_pl = tf.compat.v1.placeholder(dtype=tf.float32, shape=[None, X.shape[1]])
y_pl = tf.compat.v1.placeholder(dtype=tf.float32, shape=[None])
weights = tf.Variable(tf.compat.v1.random_normal([X.shape[1]])/100.0)
y_est = tf.nn.sigmoid(tf.reduce_sum(tf.multiply(X_pl,weights), axis=1)) #[-0.90111554  0.58614016  0.43306494]
# y_est = 1 / (1 + tf.exp(-tf.reduce_sum(tf.multiply(X_pl,weights), axis=1))) #[-0.9011158  0.5861403  0.4330649]


def my_foal_function(y_true, y_pred):
    y_pred = tf.minimum(tf.maximum(y_pred,1E-10),1.0-1E-107)
    z1 = tf.multiply(y_true ,tf.compat.v1.log(y_pred)) +  tf.multiply(1.0-y_true , tf.compat.v1.log(1.0-y_pred))
    z2 = -tf.reduce_sum(z1)
    # z2 = tf.compat.v1.Print(z2,[tf.shape(z1[:2])])
    return z2

loss = my_foal_function(y_pl, y_est)
optimizer = tf.compat.v1.train.AdamOptimizer(learning_rate=0.01)


train = optimizer.minimize(loss)
init = tf.compat.v1.global_variables_initializer()
training_epochs = 1000

weights_old = np.ones(X.shape[1])*np.inf
weights_ = - weights_old
epoch = 0


with tf.compat.v1.Session() as sess:
    sess.run(init)
    # for epoch in range(training_epochs):
    while np.linalg.norm(weights_old-weights_)>0.0000001:
        epoch += 1
        weights_old = weights_

        _, loss_, weights_ = sess.run([train, loss, weights], feed_dict={X_pl: X,
                                                  y_pl: y})
        print(f"iter {epoch+1}, loss = {loss_}, weitghts = {weights_}")

    weights_,loss_, y_est_ = sess.run([weights, loss, y_est], feed_dict={X_pl: X, y_pl: y})



print(weights_)
print("_"*10)
print(loss_)