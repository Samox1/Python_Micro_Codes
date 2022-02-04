  ##  DANE  ##

dane_bin <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data",header=FALSE, sep = ",")
dane_bin <- dane_bin[,-1]    
dane_bin_X <- colnames(dane_bin)[-1]
dane_bin_Y <- colnames(dane_bin)[1]
dane_bin[,1] <- as.factor(dane_bin[,1])

dane_multi <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data", header = FALSE, sep = ",")
przeksztalcenie_multi<-function(dane){
  for (i in 1:ncol(dane)) {
    if(i==1){
      for (j in 1:nrow(dane)) {
        if(dane[j,1]=="vhigh"){dane[j,1]<- 4} 
        else if(dane[j,1]=="high"){dane[j,1]<- 3} 
        else if(dane[j,1]=="med"){dane[j,1]<- 2} 
        else if(dane[j,1]=="low"){dane[j,1]<- 1}
      }
    }
    else if(i==2){
      for (j in 1:nrow(dane)) {
        if(dane[j,2]=="vhigh"){dane[j,2]<- 4} 
        else if(dane[j,2]=="high"){dane[j,2]<- 3} 
        else if(dane[j,2]=="med"){dane[j,2]<- 2} 
        else if(dane[j,2]=="low"){dane[j,2]<- 1}
      }
    }
    else if(i==3){
      for (j in 1:nrow(dane)) {
        if(dane[j,3]=="5more"){dane[j,3]<- 5} 
        }
    }
    else if(i==4){
      for (j in 1:nrow(dane)) {
        if(dane[j,4]=="more"){dane[j,4]<- 5} 
      }
    }
    else if(i==5){
      for (j in 1:nrow(dane)) {
        if(dane[j,5]=="small"){dane[j,5]<- 1} 
        else if(dane[j,5]=="med"){dane[j,5]<- 2} 
        else if(dane[j,5]=="big"){dane[j,5]<- 3} 
      }
    }
    else if(i==6){
      for (j in 1:nrow(dane)) {
        if(dane[j,6]=="low"){dane[j,6]<- 1} 
        else if(dane[j,6]=="med"){dane[j,6]<- 2} 
        else if(dane[j,6]=="high"){dane[j,6]<- 3} 
      }
    }
  }
  return(dane)
}
dane_multi<-przeksztalcenie_multi(dane_multi)
dane_multi[,1] <- as.numeric(dane_multi[,1])
dane_multi[,2] <- as.numeric(dane_multi[,2])
dane_multi[,3] <- as.numeric(dane_multi[,3])
dane_multi[,4] <- as.numeric(dane_multi[,4])
dane_multi[,5] <- as.numeric(dane_multi[,5])
dane_multi[,6] <- as.numeric(dane_multi[,6])
dane_multi[,7] <- as.factor(dane_multi[,7])
dane_multi[,7] <- as.ordered(dane_multi[,7])
dane_multi_X <- colnames(dane_multi)[-7]
dane_multi_Y <- colnames(dane_multi)[7]
view(dane_multi)

dane_reg <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/forest-fires/forestfires.csv", header = TRUE, sep = ",")
przeksztalcenie_reg<-function(dane){
  for (i in 1:ncol(dane)) {
    if(i==3){
      for (j in 1:nrow(dane)) {
        if(dane[j,3]=="jan"){dane[j,3]<- 1} 
        else if(dane[j,3]=="feb"){dane[j,3]<- 2} 
        else if(dane[j,3]=="mar"){dane[j,3]<- 3} 
        else if(dane[j,3]=="apr"){dane[j,3]<- 4} 
        else if(dane[j,3]=="may"){dane[j,3]<- 5} 
        else if(dane[j,3]=="jun"){dane[j,3]<- 6} 
        else if(dane[j,3]=="jul"){dane[j,3]<- 7} 
        else if(dane[j,3]=="aug"){dane[j,3]<- 8} 
        else if(dane[j,3]=="sep"){dane[j,3]<- 9} 
        else if(dane[j,3]=="oct"){dane[j,3]<- 10} 
        else if(dane[j,3]=="nov"){dane[j,3]<- 11} 
        else if(dane[j,3]=="dec"){dane[j,3]<- 12} 
      }
    }
    if(i==4){
      for (j in 1:nrow(dane)) {
        if(dane[j,4]=="mon"){dane[j,4]<- 1} 
        else if(dane[j,4]=="tue"){dane[j,4]<- 2}
        else if(dane[j,4]=="wed"){dane[j,4]<- 3}
        else if(dane[j,4]=="thu"){dane[j,4]<- 4}
        else if(dane[j,4]=="fri"){dane[j,4]<- 5}
        else if(dane[j,4]=="sat"){dane[j,4]<- 6}
        else if(dane[j,4]=="sun"){dane[j,4]<- 7}
      }
    }
  }
  return(dane)
}
dane_reg<-przeksztalcenie_reg(dane_reg)
dane_reg[,3]<-as.numeric(dane_reg[,3])
dane_reg[,4]<-as.numeric(dane_reg[,4])
dane_reg[,13] <- as.numeric(dane_reg[,13])            
dane_reg_X <- colnames(dane_reg)[-13]
dane_reg_Y <- colnames(dane_reg)[13]

  ##  Kroswalidacja wlasnych algorytmow  ##
### KNN ###

### TREE ###

### NN ###

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



