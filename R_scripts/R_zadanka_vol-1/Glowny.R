rm(list=ls())

library(data.tree)
library(rpart.plot)
library(tidyverse)
library(pROC)

source("funkcje.R")


# parTune <- expand.grid(depth=c(3:4), minobs=c(2:3), type=c('Gini', 'Entropy'), cf=0.2)
# parTune1 <- expand.grid(k=c(1:2), depth=c(3:4), minobs=c(2:3), type=c('Gini', 'Entropy'), cf=0.2)
# # parTune1[parTune1[,colnames(parTune)] == parTune[3,]]
# 
# kappa <- CrossValidTune(dane, X, Y, kFold = 2, parTune, algorytm="KNN", seed = 123)


### DANE ###

# Klasyfikacja Binarna      = 
# Klasyfikacja Wieloklasowa = 
# Regresja                  = https://archive.ics.uci.edu/ml/datasets/Abalone


dane_reg <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data", header = FALSE)

reg_kolumny <- colnames(dane_reg)               
dane_reg_X <- reg_kolumny[-9]
dane_reg_Y <- reg_kolumny[9]


### KNN ###

# ramka$k[id_modele], ramka$XminNew[id_modele], ramka$XmaxNew[id_modele]

parTune_KNN_bin <- expand.grid(k=4)

KNN_bin_CrossValid <- CrossValidTune(dane_bin, dane_bin_X, dane_bin_Y, kFold = 1, parTune_KNN_bin, algorytm="KNN", seed = 123)


dane <- iris
nazwy_kolumn <- colnames(dane)
X <- nazwy_kolumn[-5]
Y <- nazwy_kolumn[5]


parTune_KNN_multi <- expand.grid(k=4)

KNN_multi_CrossValid <- CrossValidTune(dane, X, Y, kFold = 1, parTune_KNN_multi, algorytm="KNN", seed = 123)





