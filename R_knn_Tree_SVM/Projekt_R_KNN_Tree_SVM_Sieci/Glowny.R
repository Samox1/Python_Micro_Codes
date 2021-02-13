#setwd("C:\\Users\\Dell\\Desktop\\Projekt_PG")

source("funkcje.R")

#dane do klasyfikacji binarnej
df_bin <- read.csv("caesarian.csv",header=T, sep=",")
df_bin[,6] = as.factor(df_bin[,6])
X_nazwy_bin = colnames(df_bin)[1:5]

#X_bin = df_bin[,1:5]
class(df_bin)
class(df_bin[,6])


#dane do klasyfikacji wieloklasowej
df_multi <- read.csv("balance.csv",header=T, sep=",")

X_multi = df_multi[,2:5]
Y_multi = ifelse(df_multi[,1] == "B", 1, df_multi[,1])
Y_multi = ifelse(Y_multi == "L", 2, Y_multi)
Y_multi = ifelse(Y_multi == "R", 3, Y_multi)

df_multi = as.data.frame(cbind(X_multi, Y_multi))
df_multi[,5] = as.factor(df_multi[,5])
X_nazwy_multi = colnames(df_multi)[1:4]

class(df_multi)
class(df_multi[,5])

#dane do regresji
df_reg <- read.csv("servo.csv",header=T, sep=",")

motor = ifelse(df_reg[,1] == "A", 1, df_reg[,1])
motor = ifelse(motor == "B", 2, motor)
motor = ifelse(motor == "C", 3, motor)
motor = ifelse(motor == "D", 4, motor)
motor = ifelse(motor == "E", 5, motor)
motor = as.integer(motor)

screw = ifelse(df_reg[,2] == "A", 1, df_reg[,2])
screw = ifelse(screw == "B", 2, screw)
screw = ifelse(screw == "C", 3, screw)
screw = ifelse(screw == "D", 4, screw)
screw = ifelse(screw == "E", 5, screw)
screw = as.integer(screw)

df_reg = drop(df_reg[,3:5])

df_reg = as.data.frame(cbind(motor, screw, df_reg))
X_nazwy_reg = colnames(df_reg)[1:4]

class(df_reg)
class(df_reg[,5])

#X_reg = df_reg[,1:4]
#Y_reg = df_reg[,5]

######## Drzewa decyzyjne ########

Tree <- function(Y, X, data, type, depth, minobs, overfit, cf)
  
#binarna
Drzewko_bin <- Tree("Caesarian", X_nazwy_bin, data=df_bin, type='Gini', depth=2, minobs=2, overfit='none', cf=0.001)

#wieloklasowa
Drzewko_multi <- Tree("Y_multi", X_nazwy_multi, data=df_multi, type='Gini', depth=6, minobs=2, overfit='none', cf=0.001)


#regresja - 
Drzewko_reg <- Tree("class", X_nazwy_reg, data=df_reg, type='SS', depth=9, minobs=2, overfit='none', cf=0.001)



######## K najbliższych sąsiadów ########

KNNmodel <- KNNtrain( X1, y_tar, k = 5, 0,1 )
KNNpred(KNNmodel, X)
#regresja
X1 <- df_reg[,1:4]
X <- df_reg[,1:4]
y_tar <- df_reg[,5]
#binarna
X1 <- df_bin[,1:5]
X <- df_bin[,1:5]
y_tar <- df_bin[,6]
#wieloklasowa
X1 <- df_multi[,1:4]
X <- df_multi[,1:4]
y_tar <- df_multi[,5]

######## Maszyna wektorow nosnych ######## - tylko klasyfikacja binarna
#svm_grid = expand.grid(C=1:10, lr=c(0.0001, 0.001, 0.01, 0.1))
#svm_crossval_results = CrossValidTune(dane_binary_scaled, Xnames_binary, 'Y', method='svm', k=5, param_grid=svm_grid)





