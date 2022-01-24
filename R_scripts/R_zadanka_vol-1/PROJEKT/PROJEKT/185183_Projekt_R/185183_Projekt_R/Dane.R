
# Dane do klasyfikacji wieloklasowej

f_1 <- file("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data", open="r")
dane_1 <- read.table(f_1, sep=",", header=F)

colnames(dane_1) <- c('Sex', 'Length', 'Diameter', 'Height', 'Whole weight', 'Shucked weight', 'Viscera weight', 'Shell weight', 'Rings')
dane_1

# Dane do regresji

f_2 <- file("https://archive.ics.uci.edu/ml/machine-learning-databases/concrete/slump/slump_test.data", open="r")
dane_2 <- read.table(f_2, sep=",", header=T)

colnames(dane_2) <- c('Number', 'Cement', 'Slag', 'Fly ash', 'Water', 'SP', 'Coarse Aggr', 'Fine Aggr', 'Slump', 'Flow', 'Compressive Strength')
dane_2 <- dane_2[,-c(1)]

for (i in names(dane_2)){
  dane_2[[i]] <- as.numeric(gsub(",", ".", dane_2[[i]]))
}

# Dane do klasyfikacji binarnej
f_3 <-  file("https://archive.ics.uci.edu/ml/machine-learning-databases/00472/caesarian.csv.arff", open="r")
dane_3 <- read.table(f_3, sep = '',header=F)