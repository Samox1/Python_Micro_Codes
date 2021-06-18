import numpy as np
import tensorflow as tf
import pickle

tf.compat.v1.disable_eager_execution()
model_path = 'model/'
prefix_model = 'model_mlp'

with open(model_path + 'scaler.pickle', 'rb') as f:
    scaler = pickle.load(f)

X_test = np.load('X_test.npy')
y_test = np.load('y_test.npy')

X_test = scaler.transform(X_test)

with tf.compat.v1.Session() as sess:
    saver = tf.compat.v1.train.import_meta_graph(model_path + prefix_model + '.meta')
    saver.restore(sess, model_path + prefix_model)
    graph = tf.compat.v1.get_default_graph()

    X_pl = graph.get_tensor_by_name('X:0')
    y_est_cont = graph.get_tensor_by_name('y_est_cont:0')
    y_est_disc = graph.get_tensor_by_name('y_est_disc:0')

    y_est_disc_, y_est_cont_ = sess.run([y_est_disc, y_est_cont], feed_dict = {X_pl: X_test})

ACC_test = np.mean(y_est_disc_==y_test)
print(f" ACC of testing data = {ACC_test}")

# print('='* 100)
# input(f"y_est_cont_ = {y_est_cont_}")
# input(f"y_est_disc_ = {y_est_disc_}")
# input(f"y_test = {y_test}")