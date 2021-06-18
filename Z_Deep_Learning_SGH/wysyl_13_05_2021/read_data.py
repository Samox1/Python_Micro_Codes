import os
import numpy as np
import matplotlib.pyplot as plt
from PIL import Image


from os import walk
def list_dir(directory):
    for (dirpath, directors, files) in walk(directory):
        break
    return directors, files

def read_data(directory, height, width):
    direstories_, _ = list_dir(directory)
    X_data = []
    Y_label=[]
    for i,_ in enumerate(direstories_):
        print(directory + '/' + direstories_[i])
        _, files = list_dir(directory + '/' + direstories_[i])
        for j,_ in enumerate(files):
            file=os.path.join(directory + '/' + direstories_[i], files[j])
            img = plt.imread(file)
            img = Image.fromarray(img)
            img = np.array(img.resize((height, width), Image.BICUBIC))

            X_data.append(img)
            Y_label.append(direstories_[i])

    Y_label = np.array(Y_label)
    unique_classe = np.unique(Y_label)
    number_of_classes = len(unique_classe)
    Y_label= np.array([Y_label == unique_classe[i] for i in range(number_of_classes)]).astype(np.int).T

    return (np.array(X_data,dtype=np.float32), np.array(Y_label).astype(np.float32))



