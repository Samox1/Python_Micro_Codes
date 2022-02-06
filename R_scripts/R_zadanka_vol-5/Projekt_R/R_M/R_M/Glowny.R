source("funkcje.R")

print("******************************************************")
print("*** Wczytywanie danych do klasyfikacji binarnej - Breast Cancer Wisconsin (Diagnostic)")         # http://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Wisconsin+%28Diagnostic%29
binarna <- read.csv("breast-cancer-wisconsin.data", header = FALSE) 
binarna[,11] <- as.factor(binarna[,11])                                                                 # Klasy zapisane jako wartosci numeryczne -> "2 for benign, 4 for malignant" (z notki)
binarna_Y <- colnames(binarna)[11]
binarna_X <- colnames(binarna)[-11]
print(paste0("Czy dane do klasyfikacji wieloklasowej maja warstosci NA: ", any(is.na(binarna))))   
print("Podsumowanie danych w kazdej kolumnie:")
print(summary(binarna))


print("******************************************************")
print("** Wczytywanie danych do klasyfikacji wieloklasowej - Yeast")                                    # Dane od prowadzacego -> https://archive.ics.uci.edu/ml/datasets/Yeast
wieloklasowa <- read.csv("yeast.data", header = FALSE, sep = ";")[,-1]                                  # Dane wedlug notki maja 8 kolumn numerycznych i 1 z klasami docelowymi -> kolumna nr 1 to nic nie mowiaca nazwa
wieloklasowa_Y <- colnames(wieloklasowa)[9]
wieloklasowa_X <- colnames(wieloklasowa)[-9]
print(paste0("Czy dane do klasyfikacji wieloklasowej maja warstosci NA: ", any(is.na(wieloklasowa))))   # Brak wartosci NA - dane mialy specyficzny separator, ktory zostal zamieniony na srednik + upewniono sie ze nie ma zadnych problemow z separatorem
print("Podsumowanie danych w kazdej kolumnie:")
print(summary(wieloklasowa))
print("Rozklad klas w danych: ")
print(sort(summary(wieloklasowa[,9])))


print("******************************************************")
print("*** Wczytywanie danych do regresji - Computer Hardware")                                         # https://archive.ics.uci.edu/ml/datasets/Computer+Hardware
regresja <- read.csv("machine.data", header = FALSE)[,-10]                                              # Ostatnia kolumna wedlug notki to estymacja wydajnosci (kolumny nr 9) z jakiegos artykulu
regresja_Y <- colnames(regresja)[9]
regresja_X <- colnames(regresja)[-9]
print(paste0("Czy dane do klasyfikacji wieloklasowej maja warstosci NA: ", any(is.na(regresja))))   
print("Podsumowanie danych w kazdej kolumnie:")
print(summary(regresja))




# Sieci Neuronowe



# Drzewa Decyzyjne



# KNN



# Wykresy AUC, Jakosc / MSE, MAE, MAPE od parametrow 



# Kroswalidacja na modelach z bibliotek R



