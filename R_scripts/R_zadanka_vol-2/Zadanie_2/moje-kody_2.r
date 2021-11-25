# Zadanie 0:

set.seed(555)
m <- as.data.frame(matrix(NA, 1000, 1001))
colnames(m)[1] <- "Y"
colnames(m)[2:ncol(m)] <- c(paste0("x_",1:(ncol(m)-1)))

set.seed(555)
m$Y <- sample(1:100, 1000, replace = TRUE) 

set.seed(555)
m[,2:ncol(m)] <- apply(m[,2:ncol(m)], 2, function(x) m$Y + rnorm(1000))


# Zadanie 1:

library(parallel)
library(foreach)
ModelParallel <- function(dane, Ynazwa, XnazwyList, Nrdzeni, metoda="lapply"){
  
  p <- lapply(XnazwyList, function(var) paste(var, collapse= "+"))
  if(metoda == "lapply"){
    z <- mclapply(p, function(x) lm(as.formula(paste0(Ynazwa,"~" ,x)), data = dane))
  } else if(metoda == "for"){
    cl<-makeCluster(Nrdzeni) 
    registerDoSNOW(cl)
    z <- foreach(i=1:length(XnazwyList)) %dopar% {
      lm(as.formula(paste0(Ynazwa,"~" ,p[[i]])), data = dane)
    } 
    
  }else{
    stop("Podaj wlasciwa nazwe metody (lapply lub for)")
  }
  
  df<- data.frame(var = unlist(p))
  df$coef <- lapply(z, function(x) x$coef)
  
  return(df)
}


ModelParallel(m, "Y", list("x_1", c("x_1", "x_2")),lapply)


# Zadanie 2:

mse <-function(y_tar, y_hat){
  return(mean(y_tar-y_hat)^2)
}

mae <- function(y_tar, y_hat){
  return(mean(abs(y_tar - y_hat)))
}

mape <- function(y_tar, y_hat){
  return(mean(abs((y_tar-y_hat)/y_tar)) * 100)
}


library(pROC)
auc_wart<-function(y_tar, y_hat){
  y_hat2 <- as.numeric(y_hat)
  roc_obj<- roc(y_tar, y_hat2)
  a <-as.numeric(roc_obj$auc)
  return(a)
}

conf_matrix <- function(y_tar, y_hat, odciecie = 1){
  
  if(odciecie != 1)
  {
    y_hat <- ifelse(y_hat>odciecie, 1,0)
  }
  y_tar <- factor(y_tar, levels=c(0,1))
  y_hat <- factor(y_hat, levels=c(0,1))
  return(t(table(y_hat, y_tar)))
}

sensitivity <- function(table){
   
  return(table[1,1]/(table[1,1]+table[1,2]))
}

specificity <- function(table){
  
  return(table[2,2]/(table[2,2]+table[2,1]))
}


accuracy <- function(table){
  return((table[1,1]+table[1,2])/(table[1,1]+table[2,2]+table[1,2]+table[2,1]))
}

youden_index  <- function(a,b){
   
  return(a+b-1)
}


ModelOcena <- function(y_tar, y_hat){
  if(is.numeric(y_tar)==TRUE){
    
    mae_model <- mae(y_tar, y_hat)
    mse_model <-  mae(y_tar, y_hat)
    mape_model <- mape(y_tar, y_hat)
    wynik <- c(mae_model, mse_model, mape_model)
    names(wynik) <- c("MAE", "MSE", "MAPE")
    return(wynik)
  }else if(is.factor(y_tar)==TRUE){
    #auc, macierz_klasyfikacji, czulosc, specyficznosc, jakosc
    auc_model <- auc_wart(y_tar, y_hat)
    conf_matrix_model <- conf_matrix(y_tar, y_hat)
    sensitivity_model <- sensitivity(conf_matrix_model)
    specificity_model <- specificity(conf_matrix_model)
    accuracy_model <- accuracy(conf_matrix_model)
    youden_index_model <- youden_index(sensitivity_model, specificity_model)
    
    Mat <- conf_matrix_model
    J <- youden_index_model
    Miary <- c(auc_model, sensitivity_model, specificity_model, accuracy_model)
    names(Miary) <- c("AUC","CZULOSC", "SPECYFICZNOSC", "JAKOSC")
    
    wynik <- list(Mat, J, Miary)
    names(wynik)<- c("MAT", "J", "MIARY")
    return(wynik)
  }else{
    stop("Podaj wektor y_tar dla regresji lub klasyfikacji")
  }
}

ModelOcena(c(1,11,3), c(1,12,5))
ModelOcena(as.factor(c(1,1,0,1)), as.factor(c(1,0,1,1)))


#Zadanie 3:

CrossValidTune <- function(dane, kFold, parTune, seed){
  set.seed(seed)
  k = nrow(dane)
  wektor = c()
  lista = list()
  
  for (j in 1:kFold){
    indxT <- sample( x = 1:k, size = round((1-1/kFold) * k), replace = F )
    s_indxT = sort(indxT)
    p <- 1
    while (p<=length(s_indxT)){
      for (i in 1:k){
        if (s_indxT[p] == i){
          wektor[i] == 1
          p = p+1
        }
        else{
          wektor[i] == 2
        }
      }
    }
    
    lista[[j]] = c(wektor)
  }
  
  ramka_r <- data.frame( kFold_par =c(1,2,1,2))
  for (i in 1:(length(parTune))){
    ramka_r[i+1] = parTune[i]
  }
  
  if(is.numeric(y_tar)){
    f_reg <- ramka_r
    f_reg['MAEt'] = 0
    f_reg['MSEt'] = 0
    f_reg['MAPEt'] = 0
    f_reg['MAEw'] = 0
    f_reg['MSEw'] = 0
    f_reg['MAPEw'] = 0
    return(f_reg)
  }
  else if(is.factor(y_tar)){
    f_klas <- ramka_r
    f_klas['AUCT'] = 0
    f_klas['Czulosc'] = 0
    f_klas['Specyficznosc'] = 0
    f_klas['Jakosc?'] = 0
    f_klas['AUCW'] = 0
    f_klas['Specyficznosc'] = 0
    f_klas['MAPEW'] = 0
    f_klas['Jakosc'] = 0
    return(f_klas)
  }
}

