import tensorflow as tf
from tensorflow.keras import losses
from read_data import read_data
from sklearn.model_selection import RepeatedKFold
from Random_Network import Random_Network
import numpy as np
from One_Network import One_Network
import pandas as pd
from sklearn.metrics import roc_auc_score

import os
def mk_directory(directory):
    if not os.path.exists(directory):
        os.makedirs(directory)
    return directory

tf.random.set_seed(1234)
np.random.seed(1234)

data_path = 'D:/kas/dane_brodawki'
results_path = 'D:/Zajecia/Lato_2020_2021/Deep Learning/z6/results/'


k_iteration = 101

X_all, y_all = read_data(directory=data_path, height=28, width=28)

nr = np.arange(X_all.shape[0])
np.random.shuffle(nr)
X_all, y_all = X_all[nr], y_all[nr]


X_all = (X_all - 127.5) / 127.5  # Normalize the images to [-1, 1]
n_splits=10
n_repeats=10
rkf = RepeatedKFold(n_splits=n_splits, n_repeats=n_repeats, random_state=2652124)
it_repeat = 1
it_fold = 0

factivation = tf.keras.activations.relu


dir_to_save = mk_directory(results_path)

batch_size = 16
for train_index, test_index in rkf.split(X_all):
    it_fold +=1

    dir_to_save_rf = dir_to_save + f"/repeat{it_repeat}_fold{it_fold}/"
    if not os.path.isdir(dir_to_save_rf):
        dir_to_save_rf = mk_directory(directory=dir_to_save_rf)
        X_train, y_train, X_test, y_test = X_all[train_index], y_all[train_index], X_all[test_index], y_all[test_index]

        one_network = One_Network(factivation=factivation)

        def loss(model, X, y):
            cce = losses.CategoricalCrossentropy(from_logits=True)
            return cce(y_true=y, y_pred=model(X))

        def train(loss, model, opt, X, y):
            with tf.GradientTape() as tape:
                gradients = tape.gradient(loss(model, X, y), model.trainable_variables)
                gradient_variables = zip(gradients, model.trainable_variables)
                opt.apply_gradients(gradient_variables)

        opt_one = tf.optimizers.Adam(learning_rate=0.001)

        train_batch_size = 16
        train_no_of_batches = np.round(np.linspace(0, X_train.shape[0], int(np.round(X_train.shape[0] / train_batch_size,0)) + 1),0).astype(np.int)
        nr = np.arange(X_train.shape[0])

        AUC_one, ACC_one = [], []

        for epoch in range(1000):
            dir_to_save_epoch = mk_directory(dir_to_save_rf + f"epoch{epoch}/")
            np.random.shuffle(nr)

            for batch in range(len(train_no_of_batches) - 1):
                pocz = train_no_of_batches[batch]
                kon = train_no_of_batches[batch + 1]

                train(loss, one_network, opt_one, X_train[nr[pocz:kon]], y_train[nr[pocz:kon]])

            y_test_est_continous_one = one_network.predict(np.concatenate((X_train,X_test)))

            y_train_est_continous_one = y_test_est_continous_one[:X_train.shape[0]]
            y_test_est_continous_one = y_test_est_continous_one[-X_test.shape[0]:]


            y_test_est_discrete_one = np.argmax(y_test_est_continous_one, axis=1)

            df_train_one = pd.DataFrame(data=np.concatenate((y_train_est_continous_one, y_train), axis=1),
                                        columns=['y_pred0', 'y_pred1', 'y_true0', 'y_true1'])
            df_test_one = pd.DataFrame(data=np.concatenate((y_test_est_continous_one, y_test), axis = 1),
                                   columns = ['y_pred0', 'y_pred1','y_true0','y_true1'])

            df_train_one.to_csv(dir_to_save_epoch + f"df_train_one_fold{it_fold}_repeat{it_repeat}.csv", index=False)
            df_test_one.to_csv(dir_to_save_epoch + f"df_test_one_fold{it_fold}_repeat{it_repeat}.csv", index=False)


            ACC_one_ = np.mean(y_test_est_discrete_one==y_test[:,1])
            ACC_one.append(ACC_one_)

            AUC_one_ = roc_auc_score(y_true=y_test[:,1], y_score=y_test_est_continous_one[:,1])
            AUC_one.append(AUC_one_)

            np.save(dir_to_save_rf + '/AUC_one', np.array(AUC_one))

            print(f"repeat {it_repeat}, fold = {it_fold}, epoch = {epoch}:")
            print(f"ACC_one_ = {ACC_one_}, mean ACC_one = {np.mean(np.array(ACC_one))}")
            print(f"AUC_one_ = {AUC_one_}, mean AUC_one = {np.mean(np.array(AUC_one))}")
            print('')
    else:
        print(f"directory {dir_to_save_rf} already exists")

    if it_fold==n_splits:
        it_fold = 0
        it_repeat += 1

