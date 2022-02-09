source('Funkcje.R')


  ##  DANE  ##

dane_bin <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data",header=FALSE, sep = ",")
dane_bin <- dane_bin[,-1]    
dane_bin_X <- colnames(dane_bin)[-1]
dane_bin_Y <- colnames(dane_bin)[1]
dane_bin[,1] <- as.factor(dane_bin[,1])
print(anyNA(dane_bin))
print(summary(dane_bin))


dane_multi <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data", header = FALSE, sep = ",")
dane_multi[,1] <- as.character(dane_multi[,1])
dane_multi[,2] <- as.character(dane_multi[,2])
dane_multi[,3] <- as.character(dane_multi[,3])
dane_multi[,4] <- as.character(dane_multi[,4])
dane_multi[,5] <- as.character(dane_multi[,5])
dane_multi[,6] <- as.character(dane_multi[,6])
dane_multi <- przeksztalcenie_multi(dane_multi)
# dane_multi[,1] <- as.factor(dane_multi[,1])
# dane_multi[,2] <- as.factor(dane_multi[,2])
# dane_multi[,3] <- as.factor(dane_multi[,3])
# dane_multi[,4] <- as.factor(dane_multi[,4])
# dane_multi[,5] <- as.factor(dane_multi[,5])
# dane_multi[,6] <- as.factor(dane_multi[,6])
dane_multi[,1] <- as.numeric(dane_multi[,1])
dane_multi[,2] <- as.numeric(dane_multi[,2])
dane_multi[,3] <- as.numeric(dane_multi[,3])
dane_multi[,4] <- as.numeric(dane_multi[,4])
dane_multi[,5] <- as.numeric(dane_multi[,5])
dane_multi[,6] <- as.numeric(dane_multi[,6])
dane_multi[,7] <- as.factor(dane_multi[,7])
dane_multi_X <- colnames(dane_multi)[-7]
dane_multi_Y <- colnames(dane_multi)[7]
print(anyNA(dane_multi))
print(summary(dane_multi))


dane_reg <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/forest-fires/forestfires.csv", header = TRUE, sep = ",")
dane_reg[,3] <- as.character(dane_reg[,3])
dane_reg[,4] <- as.character(dane_reg[,4])
dane_reg <- przeksztalcenie_reg(dane_reg)
dane_reg[,3] <- as.numeric(dane_reg[,3])
dane_reg[,4] <- as.numeric(dane_reg[,4])
dane_reg[,13] <- as.numeric(dane_reg[,13])            
dane_reg_X <- colnames(dane_reg)[-13]
dane_reg_Y <- colnames(dane_reg)[13]
print(anyNA(dane_reg))
print(summary(dane_reg))



  ##  Kroswalidacja wlasnych algorytmow  ##

### KNN ###
parTune_KNN_bin <- expand.grid(k=c(2,4,6,8,10))                                                                                     # COS JEST NIE TAK Z --> KNN - BIN
KNN_bin_CV <- CrossValidTune(dane_bin, dane_bin_X, dane_bin_Y, kFold = 8, parTune_KNN_bin, algorytm="KNN", seed = 264)

parTune_KNN_multi <- expand.grid(k=c(2,4,6,8,10))  
KNN_multi_CV <- CrossValidTune(dane_multi, dane_multi_X, dane_multi_Y, kFold = 8, parTune_KNN_multi, algorytm="KNN", seed = 264)

parTune_KNN_reg <- expand.grid(k=c(2,4,6,8,10))
KNN_reg_CV <- CrossValidTune(dane_reg, dane_reg_X, dane_reg_Y, kFold = 8, parTune_KNN_reg, algorytm="KNN", seed = 264)


### TREE ###
parTune_Tree_bin <- expand.grid(depth=c(3,5,7), minobs=c(2,4,6), type=c('Entropy', 'Gini'), overfit = c('none', 'prune'), cf=c(0.15))
Tree_bin_CV <- CrossValidTune(dane_bin, dane_bin_X, dane_bin_Y, kFold = 8, parTune_Tree_bin, algorytm="Tree", seed = 264)

parTune_Tree_multi <- expand.grid(depth=c(3,5,7), minobs=c(2,4,6), type=c('Entropy', 'Gini'), overfit = c('none', 'prune'), cf=c(0.15))
Tree_multi_CV <- CrossValidTune(dane_multi, dane_multi_X, dane_multi_Y, kFold = 8, parTune_Tree_multi, algorytm="Tree", seed = 264)

parTune_Tree_reg <- expand.grid(depth=c(3,5,7), minobs=c(2,4,6), type=c('SS'), overfit = c('none'), cf=c(0.15))
Tree_reg_CV <- CrossValidTune(dane_reg, dane_reg_X, dane_reg_Y, kFold = 8, parTune_Tree_reg, algorytm="Tree", seed = 264)


### NN ###
parTune_NN_bin <- expand.grid(h=list(c(2,2), c(3,3), c(5,5), c(6,6)), lr = c(0.001), iter = c(50000, 150000))
NN_bin_CV <- CrossValidTune(dane_bin, dane_bin_X, dane_bin_Y, kFold = 8, parTune_NN_bin, algorytm="NN", seed = 264)

parTune_NN_multi <- expand.grid(h=list(c(2,2), c(3,3), c(5,5), c(6,6)), lr = c(0.001), iter = c(20000, 150000))
NN_multi_CV <- CrossValidTune(dane_multi, dane_multi_X, dane_multi_Y, kFold = 8, parTune_NN_multi, algorytm="NN", seed = 264)

parTune_NN_reg <- expand.grid(h=list(c(2,2), c(3,3), c(5,5), c(6,6)), lr = c(0.001), iter = c(20000, 150000))
NN_reg_CV <- CrossValidTune(dane_reg, dane_reg_X, dane_reg_Y, kFold = 8, parTune_NN_reg, algorytm="NN", seed = 264)


  ##  Wybor najlepszego modelu dla funkcji wlasnych  ##

### KNN ###

### TREE ###

### NN ###


  ##  Kroswalidacja wbudowanych algorytmow  ##

### KNN ###

### TREE ###

### NN ###


  ##  Wybor najlepszego modelu dla funkcji wbudowanych  ##


### KNN ###

### TREE ###

### NN ###


  ##  Porownanie wynikow z wlasnych funckji z wynikami funkcji wbudowanych  ##

  ##  Wplyw hiper-parametrow na jakosc opracowanych modeli  ##



