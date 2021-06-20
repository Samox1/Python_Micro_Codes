import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import tensorflow as tf
from tensorflow import keras
from keras.utils.np_utils import to_categorical
from sklearn.metrics import classification_report, confusion_matrix

# --- ECG Heartbeat Categorization Dataset --- #
# https://www.kaggle.com/shayanfazeli/heartbeat
# Pliki: mitbih_train.csv & mitbih_test.csv
#
# > Number of Samples: 109446
# > Number of Categories: 5
# > Sampling Frequency: 125Hz
# > Data Source: Physionet's MIT-BIH Arrhythmia Dataset
# > Classes: ['N': 0, 'S': 1, 'V': 2, 'F': 3, 'Q': 4]
# 
# N: Non Ectopic beats (Normal beats)
# S: Supraventrical ectopic beats
# V: Ventricular ectopic beats
# F: Fusion beats
# Q: Unknown beats
#
#
# Rozklad klas w danych treningowych:
# Klasa: 0 = 72471
# Klasa: 1 =  2223
# Klasa: 2 =  5788
# Klasa: 3 =   641
# Klasa: 4 =  6431


### Dane Treningowe
train_raw = pd.read_csv("mitbih_train.csv", header=None)
print(train_raw.head())
print(train_raw.describe())

print("*** Rozklad klas w danych treningowych ***")
train_raw[187] = train_raw[187].astype(int)
train_class_spread = train_raw.pivot_table(index = [187], aggfunc ='size')
print(train_class_spread)
sns.catplot(x = 187, kind = 'count', data = train_raw)
plt.show()

# Pokazanie przykladowych przebiegow czasowych dla kazdej klasy
x = range(1,188)
y0 = train_raw.loc[train_raw[187] == 0]
y0 = y0.iloc[1,:-1]
plt.plot(x,y0, label = "Klasa 0")
y1 = train_raw.loc[train_raw[187] == 1]
y1 = y1.iloc[1,:-1]
plt.plot(x,y1, label = "Klasa 1")
y2 = train_raw.loc[train_raw[187] == 2]
y2 = y2.iloc[1,:-1]
plt.plot(x,y2, label = "Klasa 2")
y3 = train_raw.loc[train_raw[187] == 3]
y3 = y3.iloc[1,:-1]
plt.plot(x,y3, label = "Klasa 3")
y4 = train_raw.loc[train_raw[187] == 4]
y4 = y4.iloc[1,:-1]
plt.plot(x,y4, label = "Klasa 4")
plt.legend()
plt.show()

### Dane Testowe
test_raw = pd.read_csv("mitbih_test.csv", header=None)
print(test_raw.head())
print(test_raw.describe())

print("*** Rozklad klas w danych testowych ***")
test_raw[187] = test_raw[187].astype(int)
test_class_spread = test_raw.pivot_table(index = [187], aggfunc ='size')
print(test_class_spread)
sns.catplot(x = 187, kind = 'count', data = test_raw)
plt.show()

### Dostosowanie danych wejsciowych:
# Dane X musza miec wymiar: (87554, 187, 1)
# Dane Y musza miec wymiar: (87554, 5)

X_train = np.array(train_raw.iloc[:,:-1].values)
Y_train = to_categorical(train_raw[187].values)

X_test = np.array(test_raw.iloc[:,:-1].values)
Y_test = to_categorical(test_raw[187].values)

print(X_train)
print(Y_train)

X_train = X_train.reshape(len(X_train),X_train.shape[1],1)  
X_test = X_test.reshape(len(X_test),X_test.shape[1],1)

print(X_train.shape)
print(Y_train.shape)



### --- Model sieci CNN z artykulu --- ###
# "ECG Heartbeat Classification: A Deep Transferable Representation"
# Mohammad Kachuee, Shayan Fazeli, Majid Sarrafzadeh

### Model wyglada nastepujaco:
#
# -> Input
# > Convolution (1D, kernels=32, kernel_size=5)
# ->> Loop x5:
# >> 1) Convolution (1D, kernels=32, kernel_size=5)
# >> 2) ReLU
# >> 3) Convolution (1D, kernels=32, kernel_size=5)
# >> 4) Add (Residual Skip) - Dodawanie: start bloku + po Conv z (3)
# >> 5) ReLU
# >> 6) MaxPool (size=5, stride=2)
# > Fully Connected (neurons=32)
# > ReLU
# > Fully Connected (neurons=32)
# > Softmax 
# -> Output


### Blok konwolucji (splotu), dodawania i maxpoolingu - pozniej wykorzystane w petli
def conv_unit(unit, input_layer):
    s = '_' + str(unit)
    layer = keras.layers.Conv1D(name='Conv1' + s, filters=32, kernel_size=5, strides=1, padding='same', activation='relu')(input_layer) # Konwolucja 1D
    layer = keras.layers.Conv1D(name='Conv2' + s, filters=32, kernel_size=5, strides=1, padding='same', activation=None)(layer )        # Konwolucja 1D
    layer = keras.layers.Add(name='ResidualSum' + s)([layer, input_layer])               # Sumowanie
    layer = keras.layers.Activation("relu", name='Act_relu' + s)(layer)                  # Aktywacja - ReLU
    layer = keras.layers.MaxPooling1D(name='MaxPool' + s, pool_size=5, strides=2)(layer) # Maxpool - maksymalne wartosci z kolejnych 5 wartosci
    
    return layer


### Funkcja tworzenia modelu na wzor CNN z artykulu
def get_uncompiled_model():

    inputs = keras.layers.Input(shape=(187,1), name="Inputo") # Warstwa wejsciowa o odpowiednich wymiarach
    current_layer = keras.layers.Conv1D(name='Conv1D_1', filters=32, kernel_size=5, strides=1, padding='same', input_shape=(187,1))(inputs) # Konwolucja 1D

    for i in range(5):
        current_layer = conv_unit(i + 1, current_layer)     # Blok konwolucji

    current_layer = keras.layers.Flatten()(current_layer)   # Przekształcenie macierzy 2D na 1D
    
    current_layer = keras.layers.Dense(32, name='FC1', activation='relu')(current_layer)     # Warstwa neuronow - FC (1)
    current_layer = keras.layers.Dense(32, name='FC2')(current_layer)                        # Warstwa neuronow - FC (2)
    output_layer = keras.layers.Dense(5, name='Output', activation='softmax')(current_layer) # Warstwa wyliczajaca prawdopodobienstwo klasy

    model = keras.Model(inputs=inputs, outputs=output_layer) # Zbieranie warstw w jeden obiekt modelu
    
    return model


### Kompilacja modelu sieci neuronowej
def get_compiled_model():
    model = get_uncompiled_model()                                  # Zwrocenie modelu sieci neuronowej
    model.compile(optimizer='adam', loss='categorical_crossentropy', metrics=['accuracy'])  # Kompilacja modelu

    return model


### Tworzenie modelu i opcje monitorowania nauczania sieci
model = get_compiled_model()

### OPCJONALNIE = Fragment do uczenia sieci z dodatkowymi parametrami - monitorowanie zbioru testowego, zapisywanie najlepszego modelu, wczesniejsze stopowanie nauczania przy braku postepow i zapisywanie modelu do pliku
# file_path = "baseline_cnn_mitbih.h5"
# checkpoint = ModelCheckpoint(file_path, monitor='val_accuracy', verbose=1, save_best_only=True, mode='max')
# early = EarlyStopping(monitor="val_accuracy", mode="max", patience=10, verbose=1)
# redonplat = ReduceLROnPlateau(monitor="val_accuracy", mode="max", patience=3, verbose=2)
# callbacks_list = [checkpoint, early, redonplat]  # early


### Prezentacja modelu
model.summary()

# Petla pokazujaca wymiar danych wejsciowych i wyjsciowych dla kazdej warstwy sieci:
# for layer in model.layers:
#     print(str(layer.name) + " : " + str(layer.input_shape) + " >> X >> " + str(layer.output_shape))


### Nauczanie modelu na danych treningowych
history = model.fit(X_train, Y_train, epochs = 5, validation_data=(X_test, Y_test))
# history = model.fit(X_train, Y_train, epochs = 5, callbacks=callbacks_list, validation_data=(X_test, Y_test))  # OPCJONALNIE = Uczenie modelu z parametrami do zapisu najlepszego modelu i zapisanie go jako pliku


# Przygotowanie prezentacji historii nauczania sieci
pd.DataFrame(history.history)
pd.DataFrame(history.history)[['accuracy', 'val_accuracy']].plot()
pd.DataFrame(history.history)[['loss', 'val_loss']].plot()

### Predykcja na danych testowych / walidacyjnych
predict = model.predict(X_test)

# Zamiana prawdopodobienstwa na klasy (rozklad prawdopodobienstwa na wektor klas)
yhat = np.argmax(predict, axis = 1)

# Macierz pomylek / bledu, predykcja vs oryginalne klasy 
confusion_matrix(np.argmax(Y_test, axis = 1), yhat)

# Prezentacja macierzy pomylek
plt.figure(figsize=(8,6))
sns.heatmap(confusion_matrix(np.argmax(Y_test, axis =1), yhat), annot = True, fmt = '0.0f', cmap= 'RdPu')
plt.show()

# Raport pokazujacy metryki klasyfikacji dla zbioru testowego:
print(classification_report(np.argmax(Y_test, axis=1), yhat))