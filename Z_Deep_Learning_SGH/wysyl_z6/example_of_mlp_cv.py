import numpy as np
import tensorflow as tf
import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import MinMaxScaler
from sklearn.model_selection import KFold
from sklearn.metrics import confusion_matrix

tf.compat.v1.disable_eager_execution()

np.random.seed(seed=10)

def network_architecture(num_classes, n_input, n_hidden_1 = 3, n_hidden_2 = 2):
    X_pl = tf.compat.v1.placeholder(tf.float32, [None, n_input], name='X')
    y_pl = tf.compat.v1.placeholder(tf.float32, [None, num_classes])

    scale = 100.0
    W1 = tf.Variable(tf.compat.v1.random.normal([n_input, n_hidden_1]) / scale)
    b1 = tf.Variable(tf.compat.v1.random.normal([n_hidden_1]) / scale)

    W2 = tf.Variable(tf.compat.v1.random.normal([n_hidden_1, n_hidden_2]) / scale)
    b2 = tf.Variable(tf.compat.v1.random.normal([n_hidden_2]) / scale)

    W3 = tf.Variable(tf.compat.v1.random.normal([n_hidden_2, num_classes]) / scale)
    b3 = tf.Variable(tf.compat.v1.random.normal([num_classes]) / scale)

    layer1 = tf.sigmoid(tf.add(tf.matmul(X_pl, W1), b1))
    layer2 = tf.sigmoid(tf.add(tf.matmul(layer1, W2), b2))
    logits = tf.add(tf.matmul(layer2, W3), b3) # logits

    return X_pl, y_pl, logits

def run_experiment(X_train, y_train, X_test, y_test, learning_rate = 0.01, training_epochs = 1000, batch_len = 17):

    n_input = X_train.shape[1]
    scaler = MinMaxScaler()
    X_train = scaler.fit_transform(X_train)
    X_test = scaler.transform(X_test)

    no_of_barches = np.linspace(0, X_train.shape[0], int(np.round(X_train.shape[0] / batch_len) + 1)).astype(np.int)

    num_classes = len(np.unique(y_train))
    y_train_one_hot = np.eye(num_classes)[y_train]

    X_pl, y_pl, logits = network_architecture(num_classes, n_input)
    loss_op = tf.reduce_mean(tf.nn.softmax_cross_entropy_with_logits(logits=logits, labels=y_pl))
    optimizer = tf.compat.v1.train.AdamOptimizer(learning_rate=learning_rate)
    training_op = optimizer.minimize(loss_op)
    init = tf.compat.v1.global_variables_initializer()
    y_est_cont = tf.nn.softmax(logits, name = 'y_est_cont')
    y_est_disc = tf.argmax(y_est_cont, axis= 1, name = 'y_est_disc')
    nr = np.arange(X_train.shape[0])
    with tf.compat.v1.Session() as sess:
        sess.run(init)
        for epoch in range(training_epochs):
            np.random.shuffle(nr)
            for i in range(len(no_of_barches)-1):
                batch_begin = no_of_barches[i]
                batch_end = no_of_barches[i+1]

                X_batch = X_train[nr[batch_begin:batch_end]]
                y_batch = y_train_one_hot[nr[batch_begin:batch_end]]

                _, loss_ = sess.run([training_op,loss_op], feed_dict={X_pl: X_batch, y_pl:y_batch})
        y_est_disc_ = sess.run(y_est_disc, feed_dict={X_pl: X_test})

    ConfusionMatrix = confusion_matrix(y_true = y_test, y_pred = y_est_disc_)
    ACC = np.mean(y_est_disc_==y_test)
    return ACC, ConfusionMatrix


file = 'data_iris.csv'
df = pd.read_csv(file, sep = ';')
X = np.array(df.iloc[:,:-1])
y = np.array(df.iloc[:,-1])-1

ACC_list = []
ConfusionMatrix_avg = 0
n = 10
kf = KFold(n_splits=n, shuffle=True)
for i, (train, test) in enumerate(kf.split(X)):
    X_train, X_test = X[train], X[test]
    y_train, y_test = y[train], y[test]
    ACC, ConfusionMatrix = run_experiment(X_train, y_train, X_test, y_test)
    ACC_list.append(ACC)
    ConfusionMatrix_avg += ConfusionMatrix
    print(f"fold {i+1} ACC = {ACC}")

ConfusionMatrix_avg = ConfusionMatrix_avg/n
print(f"ACC_list = {ACC_list}, mean = {np.mean(np.array(ACC_list))}")
print(f"average Confusion Matrix = \n {ConfusionMatrix_avg}")