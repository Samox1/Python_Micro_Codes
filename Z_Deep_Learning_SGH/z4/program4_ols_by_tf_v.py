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
weights = tf.Variable(tf.compat.v1.random_normal([X.shape[1]]))
y_est = tf.reduce_sum(tf.multiply(X_pl,weights), axis=1)
# y_est = tf.reduce_sum(X_pl * weights, axis=1)

def my_goal_function(y_true, y_pred):
    e_ = tf.add(y_true,-y_pred)
    y = tf.reduce_mean(tf.compat.v1.multiply(e_,e_))
    return y

loss = my_goal_function(y_pl, y_est)
# loss = tf.losses.mean_squared_error(labels = y_pl, predictions=y_est)
# optimizer = tf.train.RMSPropOptimizer(learning_rate=0.01)
optimizer = tf.compat.v1.train.AdamOptimizer(learning_rate=0.01)

train = optimizer.minimize(loss)
init = tf.compat.v1.global_variables_initializer()
training_epochs = 1000
with tf.compat.v1.Session() as sess:
    sess.run(init)
    for epoch in range(training_epochs):
        _, loss_ = sess.run([train, loss], feed_dict={X_pl: X,
                                                  y_pl: y})
    print(f"epoch = {epoch+1}, loss = {loss_}")
    weights_, loss_ = sess.run([weights, loss], feed_dict={X_pl: X, y_pl: y})


print(weights_)
print(loss_)
print("_"*10)

