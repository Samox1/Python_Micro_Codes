library(pROC)
library(ggplot2)
library(StatMatch)
library(dplyr)
library(data.tree)
library(caret)
library(microbenchmark)
library(rpart)
library(caTools)
library(nnet)


MAE <- function(y_tar, y_hat){
  return(mean(abs(y_tar - y_hat)))
}

MSE <- function(y_tar, y_hat){
  return(mean((y_tar - y_hat)^2))
}

MAPE <- function(y_tar, y_hat){
  return( mean(abs((y_tar - y_hat)/y_tar) ))
}

Jakosc <- function(y_tar, y_hat, t){
  y_hat_klasy <- ifelse(y_hat <= t, 0, 1)
  return(sum(y_tar == y_hat_klasy) / length(y_tar))
}

Czulosc <- function(y_tar, y_hat, t){
  y_hat_klasy <- ifelse(y_hat <= t, 0, 1)
  return(sum(y_tar == 1 & y_hat_klasy == 1) / sum(y_tar == 1))
}

Specyficznosc <- function(y_tar, y_hat, t){
  y_hat_klasy <- ifelse(y_hat <= t, 0, 1)
  return(sum(y_tar == 0 & y_hat_klasy == 0) / sum(y_tar == 0))
}


Mat <- function(y_tar, y_hat, t){
  Mat <- table(y_tar, y_hat = ifelse(y_hat <= t, 0, 1))
  return(Mat)
}

Trafnosc_new <- function(y_tar, y_hat){
  return( sum(y_tar == y_hat) / length(y_tar) )
}

Czulosc_new <- function(y_tar, y_hat){
  return( sum(y_tar == 1 & y_hat == 1) / sum(y_tar == 1) )
}

Specyficznosc_new <- function(y_tar, y_hat){
  return( sum(y_tar == 0 & y_hat == 0) / sum(y_tar == 0) )
}


J <- function(y_tar, y_hat) {
  t <- seq(0.01, 1, 0.01)
  J <- vector(length = length(t))
  for (i in 1:length(t)){
    J[i] <- Czulosc(y_tar, y_hat, t[i]) + Specyficznosc(y_tar, y_hat, t[i]) - 1
  }
  return(data.frame(t, J))
}

#J_new <- function(y_tar, y_hat) {
#  d <- data.table(y_tar, y_hat)[order(-y_hat)]
#  tpr <- d[, cumsum(y_tar==1)/sum(y_tar==1)]
#  fpr <- d[, cumsum(y_tar==0)/sum(y_tar==0)]
#  return(max(tpr - fpr))
#}

#plotROC <- function(y_tar, y_hat) {
#  d <- data.table(y_tar, y_hat)[order(-y_hat)]
#  tpr <- d[, cumsum(y_tar==1)/sum(y_tar==1)]
#  fpr <- d[, cumsum(y_tar==0)/sum(y_tar==0)]
#  plot(fpr, tpr, type='l', las=1)
#  abline(a=0,b=1)
#}

AUC <- function(y_tar, y_hat){
  roc_obj <- roc(y_tar, y_hat, quiet = TRUE)
  TPR=rev(roc_obj$sensitivities)
  FPR=rev(1 - roc_obj$specificities)
  dFPR = c(diff(FPR), 0)        
  dTPR = c(diff(TPR), 0)
  auc <- sum(TPR * dFPR) + sum(dTPR * dFPR)/2
  return(auc)
}


ModelOcena <- function(y_tar, y_hat){
  if(is.numeric(y_tar)){
    regresja <- c("MAE" = MAE(y_tar, y_hat), "MSE" = MSE(y_tar, y_hat), "MAPE" = MAPE(y_tar, y_hat))
    return(regresja)
  }
  else if(is.factor(y_tar)){
    wyniki <- J(y_tar, y_hat)
    J <- max(wyniki$J)
    t <- wyniki[wyniki$J == J,][1,1]
    Mat <- Mat(y_tar, y_hat, t)
    miary <- c( "AUC" = AUC(y_tar, y_hat), "Czulosc" = Czulosc(y_tar, y_hat, t), "Specyficznosc" = Specyficznosc(y_tar, y_hat, t), "Jakosc" = Jakosc(y_tar, y_hat, t))
    klasyfikacja <- list(Mat, J(y_tar, y_hat), miary, t)
    return(klasyfikacja)
  }
  else{
    c("Niepoprawne dane!")
  }
}


ModelOcena_Jakosc <- function(y_tar, y_hat){
  return (c("Jakosc" = sum( diag( table( y_tar, y_hat ) ) ) / length(y_tar) ))
}


######## K najbliższych sąsiadów ########

KNNtrain <- function(X, y_tar, k, XminNew, XmaxNew){
  if(sum(is.na(X)) == 0 & sum(is.na(y_tar)) == 0 & k>0 & (is.data.frame(X) | is.matrix(X))){
    nazwy <- vector()
    nazwy_kol <- colnames(X)
    minOrg <- vector()
    maxOrg <- vector()
    minmaxNew <- c(XminNew, XmaxNew)
    for (i in 1:ncol(X)) {
      if(is.numeric(X[,i])){
        nazwy <- append(nazwy, nazwy_kol[i])
        minOrg <- append(minOrg, min(X[,i]))
        maxOrg <- append(maxOrg, max(X[,i]))
        X[,i] <- ( (X[,i] - min(X[,i])) / (max(X[,i]) - min(X[,i])) ) * (XmaxNew - XminNew) + XminNew
      }
    }
    names(minOrg) <- nazwy
    names(maxOrg) <- nazwy
    attr(X,"minOrg") <- minOrg
    attr(X,"maxOrg") <- maxOrg
    attr(X,"minmaxNew") <- minmaxNew
    
    knn <- list()
    knn[["X"]] <- X
    knn[["y"]] <- y_tar
    knn[["k"]] <- k
    return( knn )
  }
  else{
    print("Niepoprawne dane!")
  }  
}

#skala ilorazowa - odl.euklidesowa
ilorazowa <- function(x_i, x_n){
  return(sqrt(sum( (x_i - x_n)^2 )))
}

#skala porządkowa
porzadkowa <- function(x_i, x_n){
  odl <- matrix(0, nrow(x_i), nrow(x_n))
  moc <- vector()
  for (i in 1:ncol(x_i)){
    moc[i]<-length(unique(x_i[,i]))
  }
  for (i in 1:nrow(x_i)){
    for (j in 1:nrow(x_n)) {
      for (k in 1:ncol(x_i)){
        odl[i,j] <- odl[i,j] + abs(x_i[i,k] - x_n[j,k])/(moc[k] - 1)
      }
    }
  }
  return (odl)
}

#skala nominalna
nominalna <- function(x_i, x_n){
  return ( sum(x_i != x_n) / (length(x_i)) )
}

#skala mieszana - odl.Gowera
mieszana <- function(x_i, x_n){
  odl <- matrix(0, nrow(x_i), nrow(x_n))
    for (i in 1:nrow(x_i)){
    for (j in 1:nrow(x_n)) {
      licznik <- 0 
      for (k in 1:length(x_i)) {
        if (is.numeric(x_i[,k])){
          licznik <- licznik + abs(x_i[i,k] - x_n[j,k]) / (max(x_i[,k]) - min(x_i[,k]))
        }
        else if (is.ordered(x_i[,k])){
          temp_i <- as.integer(x_i[,k])
          temp_n <- as.integer(x_n[,k])
          z_i <- (temp_i -1)/(max(temp_i)-1)
          z_n <- (temp_n - 1) / (max(temp_n)-1)
          z_i_i <- z_i[i]
          z_j_n <- z_n[j]
          licznik <- licznik + abs(z_i_i - z_j_n) / (max(z_i) - min(z_i))
        }
        else if (is.factor(x_i[,k])){
          temp_x_i <- as.character(x_i[,k])
          temp_x_n <- as.character(x_n[,k])
          licznik <- licznik + sum(temp_x_i[i] == temp_x_n[j])
        }
      }
      odl[i,j] <- licznik / length(x_i)
    }
    
  }
  return (odl)
}

KNNpred <- function(KNNmodel, X){
  if ( sum(is.na(X)) == 0 & all(colnames(KNNmodel$X) == colnames(X)) ){
    min_org <- attr(KNNmodel$X, "minOrg")
    max_org <- attr(KNNmodel$X, "maxOrg")
    min_new <- attr(KNNmodel$X, "minmaxNew")[1]
    max_new <- attr(KNNmodel$X, "minmaxNew")[2]
    zmienne_norm <- rownames(as.data.frame(min_org))
    
    for (name in zmienne_norm) {
      X[name] <- ( (X[name] - min_org[name]) / (max_org[name] - min_org[name]) ) * ( max_new - min_new ) + min_new
    }  
    
    odl <- gower.dist(KNNmodel$X, X)
    odl <- as.data.frame(odl)

    n <- nrow(X)
    
    if (is.numeric(KNNmodel$y)){
      pred <- vector()
      for (i in 1:n) {
        kNaj <- order( odl[,i] )
        kNaj <- kNaj[1:KNNmodel$k]
        y_hat <- mean( KNNmodel$y[ kNaj ] )
        pred[ i ] <- y_hat
      }
    }
    
    else if(is.factor(KNNmodel$y)){
      pred <- as.data.frame(matrix(nrow = nrow(X), ncol = length(unique(KNNmodel$y))+1))
      for (i in 1:n) {
        kNaj <- order( odl[,i])
        kNaj <- kNaj[1:KNNmodel$k]
        if (length(unique(KNNmodel$y))== 2){
          names(pred) <- c('P', 'N', 'Klasa')
          pozytywna <- sum(KNNmodel$y[kNaj] == 1) / KNNmodel$k
          negatywna <- sum(KNNmodel$y[kNaj] == 0) / KNNmodel$k
          pred_klasa <- ifelse(pozytywna > 0.5, 'P', 'N')
          pred[i, 1] <- pozytywna
          pred[i, 2] <- negatywna
          pred[i, 3] <- pred_klasa
        }
        else if (length(unique(KNNmodel$y)) > 2){
          etykiety <- sort(unique(KNNmodel$y))
          names(pred) <- etykiety
          names(pred)[length(unique(KNNmodel$y))+1] <- 'Klasa'
          for (j in 1:length(etykiety)){
            pozytywna <- sum(KNNmodel$y[kNaj] == as.character(etykiety[j])) / KNNmodel$k
            pred[i, j] <- pozytywna
          }
          pred_index <- which.max(pred[i,])
          pred_klasa <- etykiety[pred_index]
          pred[i,'Klasa'] <- as.character(pred_klasa)
        }
        
      }
    }
    return( pred )
  }
  
  else{
    print("Niepoprawne dane!")
  }
}





######## Drzewa decyzyjne ########

StopIfNot <- function(Y, X, data, type, depth, minobs, overfit, cf) {
  
  if (!is.data.frame(data)) {
    mes <<- "Zbior nie jest ramka danych"
    return(FALSE)
  } else if (!all(c(Y,X) %in% names(data))) {
    mes <<- "Zmienne nie wystepuja w ramce danych"
    return(FALSE)
  } else if (any(is.na(data[,c(Y,X)]))) {
    mes <<- "Wystepuja braki danych"
    return(FALSE)
  } else if (depth<=0 | minobs<=0) {
    mes <<- "Ujemne parametry depth/minobs"
    return(FALSE)
  } else if (!(type %in% c("Gini", "Entropy", "SS"))) {
    mes <<- "Bledny typ"
    return(FALSE)
  } else if (!(overfit %in% c("none", "prune"))) {
    mes <<- "Bledny argument overfit"
    return(FALSE)
  } else if (cf<=0 | cf>0.5) {
    mes <<- "Bledny argument cf"
    return(FALSE)
  } else if (type=="SS" & is.factor(data[,Y]) | type=="Gini" & is.numeric(data[,Y]) | 
             type=="Entropy" & is.numeric(data[,Y])) {
    mes <<- "Niedozwolona kombinacja parametrow"
    return(FALSE)
  } else {
    return(TRUE)
  }
  
}

Prob <- function(y){
  
  res <- unname(table(y))
  res <- res/sum(res)
  return(res)
  
}

Gini <- function(prob){
  
  res <- prob^2
  res <- 1-sum(res)
  return(res)
  
}

Entropy <- function(prob){
  
  res <- prob*log2(prob)
  res[prob == 0] <- 0
  res <- -sum(res)
  return(res)
  
}

SS <- function(y){
  
  res <- (y-mean(y))^2
  res <- sum(res)
  return(res)
  
}

AssignInitialMeasures <- function(tree, Y, data, type, depth){
  
  tree$Depth <- 0
  
  if (type=="Gini") {
    tree$inf <- Gini(Prob(data[,Y]))
  } else if (type=="Entropy") {
    tree$inf <- Entropy(Prob(data[,Y]))
  } else {
    tree$inf <- SS(data[,Y])
  }
}

AssignInfo <- function(tree, Y, X, data, type, depth, minobs, overfit, cf) {
  
  tree$Y <- data[,Y]
  tree$X <- data[,X]
  tree$data <- data
  tree$type <- type
  tree$Depth <- depth
  tree$minobs <- minobs
  tree$overfit <- overfit
  tree$cf <- cf
  
}

SpliNum <- function(Y, x, parentVal, splits, minobs, type) {
  
  n <- length(x)
  res <- data.frame(matrix(0, length(splits), 6))
  colnames(res) <- c("InfGain", "lVal", "rVal", "point", "ln", "rn")
  
  for (i in 1:length(splits)) {
    partition <- x <= splits[i] 
    ln <- sum(partition)
    rn <- n - ln
    
    lVal <- 
      if (type=="Gini") {
        Gini(Prob(Y[partition]))
      } else if (type=="Entropy") {
        Entropy(Prob(Y[partition]))
      } else {
        SS(Y[partition])
      }   
    
    rVal <-         
      if (type=="Gini") {
        Gini(Prob(Y[!partition]))
      } else if (type=="Entropy") {
        Entropy(Prob(Y[!partition]))
      } else {
        SS(Y[!partition])
      }  
    
    InfGain <- parentVal - (lVal * ln / n  + rVal * rn / n)
    
    res[i, "InfGain"] <- InfGain
    res[i, "lVal"] <- lVal
    res[i, "rVal"] <- rVal
    res[i, "point"] <- ifelse(is.numeric(splits[i]), splits[i], as.character(splits[i]))
    res[i, "ln"] <- ln
    res[i, "rn"] <- rn
  }
  
  return(res)
  
}
SplitVar <- function(Y, x, parentVal, minobs, type) {
  
  s <- unique(x)
  if (length(x) == 1) {
    splits <- s
    
  } else{
    splits <- head(sort(s),-1)
    
  }
  
  res <- SpliNum(Y, x, parentVal, splits, minobs, type)
  
  incl <- res$ln >= minobs & res$rn >= minobs & res$InfGain > 0
  res <- res[incl, , drop = F]
  
  best <- which.max(res$InfGain)
  
  res <- res[best, , drop = F]
  
  return(res)
  
}
FindBestSplit <- function(Y, X, data, parentVal, type, minobs) {
  
  if (is.numeric(data[,Y])) { 
    for (zm in X) {
      if (!is.numeric(data[,zm]) & !is.ordered(data[,zm])) { 
        a <- tapply(data[,Y], data[,zm], mean)
        a <- sort(a)
        data[,zm] <- factor(data[,zm], levels = names(a), ordered = TRUE)
      }
    }
    
  } else { 
    for (zm in X) {
      if (!is.numeric(data[,zm]) & !is.ordered(data[,zm])) { 
        posCat <- 1 
        temp <- data[data[,Y]==posCat,] 
        a <- prop.table(table(temp[,zm]))
        a <- sort(a)
        data[,zm] <- factor(data[,zm], levels = names(a), ordered = TRUE)
      }
    }
  }
  
  res <- sapply(X, function(i) {
    
    SplitVar(data[, Y], data[, i], parentVal, minobs, type)
    
  }, simplify = F)
  
  res <- do.call("rbind", res)
  
  best <- which.max(res$InfGain)
  res <- res[best, , drop = F]
  
  return(res)
  
}

BuildTree <- function(node, Y, X, data, depth, type, minobs) {
  node$Count <- nrow(data)
  node$Prob <- Prob(data[, Y])
  
  bestSplit <- FindBestSplit(Y, X, data, node$inf, type, minobs)
  
  ifStop <- nrow(bestSplit) == 0
  
  if (node$Depth == depth |
      ifStop | all(node$Prob %in% c(0, 1))) {
    node$Leaf <- "*"
    return(node)
    
  } else{
    split_indx <- data[, rownames(bestSplit)] <= bestSplit$point
    child_frame <- split(data, split_indx)
    
    name <- sprintf("%s <= %s", rownames(bestSplit), bestSplit$point)
    child_l <- node$AddChild(name)
    child_l$value <- split_indx
    child_l$Depth <- node$Depth + 1
    child_l$inf <- bestSplit$lVal
    
    BuildTree(child_l, Y, X, child_frame[[1]], depth, type, minobs)
    
    name <- sprintf("%s >  %s", rownames(bestSplit), bestSplit$point)
    child_r <- node$AddChild(name)
    child_r$value <- split_indx
    child_r$Depth <- node$Depth + 1
    child_r$inf <- bestSplit$rVal
    
    BuildTree(child_r, Y, X, child_frame[[2]], depth, type, minobs)
    
  }
  
}

Tree <- function(Y, X, data, type, depth, minobs, overfit, cf){
  
  if (!StopIfNot(Y, X, data, type, depth, minobs, overfit, cf)) stop(mes, call. = FALSE)
  
  tree <- Node$new("Root")
  
  AssignInitialMeasures(tree, Y, data, type, depth)
  
  BuildTree(tree, Y, X, data, depth, type, minobs)
  
  AssignInfo(tree, Y, X, data, type, depth, minobs, overfit, cf)
  
  return(tree)
  
}

######## Maszyna wektorow nosnych ########

Decision <- function( X, theta, theta0 ){
  X %*% t( theta ) + theta0
}

Margin <- function( X, y, theta, theta0 ){
  y * Decision( X, theta, theta0 )
}

Cost <- function( margin, theta, C ){
  (1/2) * theta %*% t(theta) + C * sum( pmax( 0, 1 - margin ) )
}

trainSVM <- function( X, y, C = 1, lr = 0.001, maxiter = 500 ){
  d <- nrow(X)
  p <- ncol(X)
  theta <- matrix( runif(p), nrow = 1 )
  theta0 <- 0
  loss <- double(maxiter)
  for( i in 1:maxiter ){
    margin <- Margin( X, y, theta, theta0 )
    loss[i] <- Cost( margin, theta, C )
    miss_indx <- which( margin < 1 )
    d_theta <- theta - C * y[miss_indx] %*% X[miss_indx,]
    theta <- theta - lr * d_theta
    d_theta0 <- - C * sum( y[miss_indx] )
    theta0 <- theta0 - lr * d_theta0
  }
  supp_vect <- which( Margin( X, y, theta, theta0 ) <= 1 )
  return( list( SP = supp_vect, Theta = theta, Theta0 = theta0 ) )
}

predSVM <- function( X, theta, theta0 ){
  sign( Decision( X, theta, theta0 ) )
}



######## Sieci neuronowe ########

sigmoid <- function( x ){
  return( 1 / (1 + exp( -x ) ) )
}
dsigmoid <- function( x ){
  return( x * (1 - x) )
}
ReLu <- function( x ){
  return( ifelse( x <= 0, 0, x ) )
}
dReLu <- function( x ){
  return( ifelse( x <= 0, 0, 1 ) )
}
lossSS <- function( y_tar, y_hat ){
  return( 1/2 * sum( ( y_tar - y_hat )^2 ) )
}
SoftMax <- function(x){
  exp( x ) / sum( exp( x ) )
}
MinMax <- function( x ){
  return( ( x - min(x) ) / ( max(x) - min(x) ) )
}  
MinMaxOdwrot <- function( x, y_min, y_max ){
  return(  x * (y_max - y_min) + y_min )
} 


### NOWE SIECI ###

### wprzod <- function( X, W, f_aktywacji, typ ){
###   H <- list()
###   H[[1]] <- cbind( matrix( 1, nrow = nrow(X) ), f_aktywacji( X %*% W[[1]] )  )
###   for (i in 2:(length(W)-1)){
###     H[[i]] <- cbind( matrix( 1, nrow = nrow(X) ), f_aktywacji( H[[i-1]] %*% W[[i]] )  )
###     
###   }
###   
###   if (typ == "wieloklasowa") {
###     y_hat <- matrix( t( apply( H[[length(W)-1]] %*% W[[length(W)]], 1, SoftMax ) ), nrow = nrow(X) ) # klasyfikacja wieloklasowa
###   } 
###   else if (typ == "regresja") {
###     y_hat <- H[[length(W)-1]] %*% W[[length(W)]] # regresja
###   }
###   else if (typ == "binarna") {
###     y_hat <- f_aktywacji( H[[length(W)-1]] %*% W[[length(W)]] )
###   }
###   nazwy_H<- paste0( "H", 1:(length(W)-1))
###   names(H) <- nazwy_H
###   # print(y_hat)
###   return( list( y_hat = y_hat, H = H) )
### }
### 
### wstecz <- function( X, y_tar, y_hat, W, H, lr, df_aktywacji, typ ){
###   if (typ == "binarna") {
###     dy_hat <- (y_tar - y_hat) * df_aktywacji( y_hat )#(y_hat-y_tar) 
###   }
###   else if (typ == "wieloklasowa") {
###     dy_hat <- (y_tar - y_hat) / nrow( X ) # klasyfikacja wieloklasowa
###   }
###   else if (typ == "regresja") {
###     dy_hat <- (y_tar - y_hat) # regresja
###   }
###   dW <- list() 
###   dH <- list()
###   dW[[length(W)]] <- t(H[[length(H)]]) %*% dy_hat
###   
###   dH[[length(H)]] <- dy_hat %*% t(W[[length(W)]]) * df_aktywacji( H[[length(H)]] )
###   dW[[length(W)-1]] <- t(H[[length(H)-1]]) %*% dH[[length(H)]][,-1]
###   i=length(W)-2
###   while (i>1){
###     dH[[i]] <- dH[[i+1]][,-1] %*% t(W[[i+1]]) * df_aktywacji( H[[i]] )
###     dW[[i]] <- t(H[[i-1]]) %*% dH[[i]][,-1]
###     i=i-1 
###   }
###   
###   dH[[1]]<- dH[[2]][,-1] %*% t(W[[2]]) * df_aktywacji( H[[1]] )
###   dW[[1]] <- t(X) %*% dH[[1]][,-1]
###   for (k in 1:length(W)){
###     W[[k]] <- W[[k]] + lr * dW[[k]]
###   }
###   return( W )
### } 
### 
### trainNN <- function( x, y_tar, h = c(5,5), lr = 0.001, iter = 1000, seed = 123, f_aktywacji, df_aktywacji, typ){
###   set.seed( seed )
###   X <- cbind( rep( 1, nrow(x) ), x )
###   # W1, .... W_liczba_warst_ukrytych+1
###   W <- list()
###   
###   W[[1]] <- matrix( runif( ncol(X) * h[1], -1, 1 ), nrow = ncol(X) )
###   for (k in 2:length(h)){
###     W[[k]] <- matrix( runif( (h[k-1]+1) * h[k], -1, 1 ), nrow = h[k-1] + 1 )
###   }
###   W[[length(h)+1]] <- matrix( runif( (h[length(h)]+1) * ncol(y_tar), -1, 1 ), nrow = h[length(h)] + 1 )
###   
###   nazwy<- paste0( "W", 1:(length(h)+1))
###   names(W) <- nazwy
###   
###   error <- double( iter )
###   for( i in 1:iter ){
###     sygnalwprzod <- wprzod( X, W, f_aktywacji, typ )# W = list( W1, W2, W3 )
###     sygnalwtyl <- wstecz( X, y_tar, y_hat = sygnalwprzod$y_hat, W, H = sygnalwprzod$H, lr, df_aktywacji, typ )
###     for (l in 1:(length(h)+1)){
###       W[[l]] <- sygnalwtyl[[l]]
###     }
###     cat( paste0( "\rIteracja: ", i ) )
###     error[i] <- lossSS( y_tar, sygnalwprzod$y_hat )
###   }
###   xwartosci <- seq( 1, iter, length = 1000 )
###   print( qplot( xwartosci, error[xwartosci], geom = "line", main = "Error", xlab = "Iteracje" ) )
###   return( list( y_hat = sygnalwprzod$y_hat, W = W ) )
### }
### 
### predNN <- function( xnew, nn, f_aktywacji, typ ){
###   
###   xnew <- cbind( rep( 1, nrow(xnew) ), xnew )
###   HP <- list()
###   HP[[1]] <- cbind( matrix( 1, nrow = nrow(xnew) ), f_aktywacji( xnew %*% nn$W[[1]] )  )
###   for (i in 2:(length(nn$W)-1)){
###     HP[[i]] <- cbind( matrix( 1, nrow = nrow(xnew) ), f_aktywacji( HP[[i-1]] %*% nn$W[[i]] )  )
###     
###   } 
###   if (typ == "wieloklasowa") {
###     y_hat <- matrix( t( apply( HP[[length(nn$W)-1]] %*% nn$W[[length(nn$W)]], 1, SoftMax ) ), nrow = nrow(xnew) ) # klasyfikacja wieloklasowa
###   } 
###   else if (typ == "regresja") {
###     y_hat <- HP[[length(nn$W)-1]] %*% nn$W[[length(nn$W)]] # regresja
###   }
###   else if (typ == "binarna") {
###     y_hat <- f_aktywacji( HP[[length(nn$W)-1]] %*% nn$W[[length(nn$W)]] )
###   }
###   
###   return( y_hat )
### }

### NOWE SIECI ###



### --- OLD NN --- ###

wprzod_old <- function( X, W1, W2, W3, typ ){
  h1 <- cbind( matrix( 1, nrow = nrow(X) ), sigmoid( X %*% W1 )  )
  h2 <- cbind( matrix( 1, nrow = nrow(X) ), sigmoid( h1 %*% W2 )  )
  if (typ =="binarna"){
    y_hat <- sigmoid( h2 %*% W3 ) # klasyfikacja binarna
  }
  else if (typ == "wieloklasowa"){
    y_hat <- matrix( t( apply( h2 %*% W3, 1, SoftMax ) ), nrow = nrow(X) ) # klasyfikacja wieloklasowa
  } 
  else if (typ == "regresja"){
    y_hat <- h2 %*% W3 # regresja
  }
  return( list( y_hat = y_hat, H1 = h1, H2 = h2 ) )
}
wstecz_old <- function( X, y_tar, y_hat, W1, W2, W3, H1, H2, lr, typ ){
  if (typ =="binarna"){
    dy_hat <- (y_tar - y_hat) * dsigmoid( y_hat ) # klasyfikacja binarna
  }
  else if (typ == "wieloklasowa"){
    dy_hat <- (y_tar - y_hat) / nrow( X ) # klasyfikacja wieloklasowa
  } 
  else if (typ == "regresja"){
    dy_hat <- (y_tar - y_hat) # regresja
  }
  dW3 <- t(H2) %*% dy_hat
  dH2<- dy_hat %*% t(W3) * dsigmoid( H2 )
  dW2 <- t(H1) %*% dH2[,-1]
  dH1<- dH2[,-1] %*% t(W2) * dsigmoid( H1 )
  dW1 <- t(X) %*% dH1[,-1]
  W1 <- W1 + lr * dW1
  W2 <- W2 + lr * dW2
  W3 <- W3 + lr * dW3
  return( list( W1 = W1, W2 = W2, W3 = W3 ) )
}

trainNN_old <- function( x, y_tar, h = c(5,5), lr = 0.01, iter = 10000, seed = 123, typ = "binarna" ){
  set.seed( seed )
  X <- cbind( rep( 1, nrow(x) ), x )
  
  h = unlist(h, use.names = FALSE)
  
  W1 <- matrix( runif( ncol(X) * h[1], -1, 1 ), nrow = ncol(X) )
  W2 <- matrix( runif( (h[1]+1) * h[2], -1, 1 ), nrow = h[1] + 1 )
  W3 <- matrix( runif( (h[2]+1) * ncol(y_tar), -1, 1 ), nrow = h[2] + 1 )

  error <- double( iter )

  for( i in 1:iter ){
    sygnalwprzod <- wprzod_old( X, W1, W2, W3, typ=typ )
    sygnalwtyl <- wstecz_old( X, y_tar, y_hat = sygnalwprzod$y_hat, W1, W2, W3, H1 = sygnalwprzod$H1, H2 = sygnalwprzod$H2, lr, typ=typ )
    W1 <- sygnalwtyl$W1
    W2 <- sygnalwtyl$W2
    W3 <- sygnalwtyl$W3
    cat( paste0( "\rIteracja: ", i , " / ", iter) )

    error[i] <- lossSS( y_tar, sygnalwprzod$y_hat )
  }
  xwartosci <- seq( 1, iter, length = 1000 )
  print( qplot( xwartosci, error[xwartosci], geom = "line", main = "Error", xlab = "Iteracje" ) )
  return( list( y_hat = sygnalwprzod$y_hat, W1 = W1, W2 = W2, W3 = W3 ) )
}

predNN_old <- function( xnew, nn, typ = "binarna" ){
  xnew <- cbind( rep( 1, nrow(xnew) ), xnew )
  h1 <- cbind( matrix( 1, nrow = nrow(xnew) ), sigmoid( xnew %*% nn$W1 )  )
  h2 <- cbind( matrix( 1, nrow = nrow(xnew) ), sigmoid( h1 %*% nn$W2 )  )
  if (typ =="binarna"){
    y_hat <- sigmoid( h2 %*% nn$W3 ) # klasyfikacja binarna
  }
  else if (typ == "wieloklasowa"){
    y_hat <- matrix( t( apply( h2 %*% nn$W3, 1, SoftMax ) ), nrow = nrow(xnew) ) # klasyfikacja wieloklasowa
  } 
  else if (typ == "regresja"){
    y_hat <- h2 %*% nn$W3 # regresja
  }
  return( y_hat )
}


### --- OLD NN --- ###


######## Kroswalidacja ########

CrossValidTune <- function(dane, X, y, kFold = 10, parTune, seed = 123, algorytm="KNN"){
  
  set.seed(seed)
  
  if(is.numeric(dane[,y])){
    typ = "reg"
  }
  else if(is.factor(dane[,y])){
    if (length(unique(dane[,y])) == 2){
      typ = "bin"
    }
    else if(length(unique(dane[,y])) > 2){
      typ = "multi"
    }
  }
  
  cat("\n")
  print("*** ---------- ***")
  print(paste("Algorytm: ", algorytm))
  print(paste("Typ algorytmu: ",typ))
  print("*** ---------- ***")
  cat("\n")
  
  if(typ=="reg"){
    
    mae_train = vector()
    mse_train = vector()
    mape_train = vector()
    
    mae_test = vector()
    mse_test = vector()
    mape_test = vector()
  }
  else if(typ=="bin"){
    
    czulosc_train <- vector()
    specyficznosc_train  <- vector()
    jakosc_train  <- vector()
    
    czulosc_test <- vector()
    specyficznosc_test  <- vector()
    jakosc_test <- vector()
  }
  else if(typ=="multi"){
    jakosc_train <- vector()
    jakosc_test <- vector()
  }
  
  for (i in 1:nrow(parTune)){
    hiper <- parTune[i,]
    
    if(typ=="reg"){
      
      MAE_Train = vector()
      MSE_Train = vector()
      MAPE_Train = vector()
      
      MAE_Test = vector()
      MSE_Test = vector()
      MAPE_Test = vector()
    }
    else if(typ=="bin"){
      
      Czulosc_Train <- vector()
      Specyficznosc_Train  <- vector()
      Jakosc_Train  <- vector()
      
      Czulosc_Test <- vector()
      Specyficznosc_Test  <- vector()
      Jakosc_Test <- vector()
    }
    else if(typ=="multi"){
      Jakosc_Train <- vector()
      Jakosc_Test <- vector()
    }
    
    
  
    for (j in 1:kFold){
      indxTest <- sample( 1:nrow(dane), size = 1/kFold * nrow(dane), replace = F )
      dane_Test <- dane[indxTest,] 
      dane_Train <- dane[-indxTest,]
    
      if(algorytm=="KNN"){                                ### --- ### --- KNN --- ### --- ### 
        
        KNNmodel <- KNNtrain( X=dane_Train[,X], y_tar=dane_Train[,y], k = hiper, XminNew=0, XmaxNew=1 )
        KNNpred_Train <- KNNpred(KNNmodel, X=dane_Train[,X])
        KNNpred_Test <- KNNpred(KNNmodel, X=dane_Test[,X])
        
        if(typ=="reg")
        {
          Train_ocena = ModelOcena(dane_Train[,y], KNNpred_Train)
          Test_ocena = ModelOcena(dane_Test[,y], KNNpred_Test)
          
          MAE_Train = append(MAE_Train, Train_ocena["MAE"])
          MSE_Train = append(MSE_Train, Train_ocena["MSE"])
          MAPE_Train = append(MAPE_Train, Train_ocena["MAPE"])
          
          MAE_Test = append(MAE_Test, Test_ocena["MAE"])
          MSE_Test = append(MSE_Test, Test_ocena["MSE"])
          MAPE_Test = append(MAPE_Test, Test_ocena["MAPE"])
        }
        else if(typ=="bin")
        {
          Train_ocena = ModelOcena(dane_Train[, y], as.numeric(KNNpred_Train[,1]))
          Test_ocena = ModelOcena(dane_Test[, y], as.numeric(KNNpred_Test[,1]))
          
          Train_miary = Train_ocena[[3]]
          Test_miary = Test_ocena[[3]]
          
          Czulosc_Train <- append(Czulosc_Train, Train_miary["Czulosc"])
          Specyficznosc_Train  <- append(Specyficznosc_Train, Train_miary["Specyficznosc"])
          Jakosc_Train  <- append(Jakosc_Train, Train_miary["Jakosc"])
          
          Czulosc_Test <- append(Czulosc_Test, Test_miary["Czulosc"])
          Specyficznosc_Test  <- append(Specyficznosc_Test, Test_miary["Specyficznosc"])
          Jakosc_Test <- append(Jakosc_Test, Test_miary["Jakosc"])
        }
        else if(typ=="multi")
        {
          Jakosc_Train <- append(Jakosc_Train, Trafnosc_new(dane_Train[,y], KNNpred_Train$Klasa))
          Jakosc_Test <- append(Jakosc_Test, Trafnosc_new(dane_Test[,y], KNNpred_Test$Klasa))
        }
        
      }else if(algorytm=="drzewa"){                     ### --- ### --- DRZEWA --- ### --- ### 

        print("Brak mozliwosci zrobienia predykcji na Drzewach!")
        
      }else if(algorytm=="SVM"){                        ### --- ### --- SVM --- ### --- ###

        if(typ=="bin"){
          dane_Test <- dane[indxTest,]
          dane_Train <- dane[-indxTest,]

          X_train = as.matrix(sapply(dane_Train[,X],MinMax))
          X_test = as.matrix(sapply(dane_Test[,X],MinMax))
          y_train = ifelse(dane_Train[,y] == 0, -1, 1)
          y_test = ifelse(dane_Test[,y] == 0, -1, 1)

          SVM_model <- trainSVM(X_train, y_train, C=hiper$C, lr = hiper$lr, maxiter = hiper$maxiter)
          SVM_pred_Train <- predSVM(X_train, SVM_model$Theta, SVM_model$Theta0)
          SVM_pred_Test <- predSVM(X_test, SVM_model$Theta, SVM_model$Theta0)
          
          y_train = ifelse(y_train == -1, 0, 1)
          y_test = ifelse(y_test == -1, 0, 1)
          SVM_pred_Train = ifelse(SVM_pred_Train == -1, 0, 1)
          SVM_pred_Test = ifelse(SVM_pred_Test == -1, 0, 1)

          czulosc_train_svm = Czulosc_new(y_train, SVM_pred_Train)
          czulosc_test_svm = Czulosc_new(y_test, SVM_pred_Test)
          specyficznosc_train_svm = Czulosc_new(y_train, SVM_pred_Train)
          specyficznosc_test_svm = Czulosc_new(y_test, SVM_pred_Test)
          jakosc_train_svm = Trafnosc_new(y_train, SVM_pred_Train)
          jakosc_test_svm = Trafnosc_new(y_test, SVM_pred_Test)
          
          Czulosc_Train <- append(Czulosc_Train, czulosc_train_svm)
          Specyficznosc_Train  <- append(Specyficznosc_Train, specyficznosc_train_svm)
          Jakosc_Train  <- append(Jakosc_Train, jakosc_train_svm)
          
          Czulosc_Test <- append(Czulosc_Test, czulosc_test_svm)
          Specyficznosc_Test  <- append(Specyficznosc_Test, specyficznosc_test_svm)
          Jakosc_Test <- append(Jakosc_Test, jakosc_test_svm)
        }
        else if(typ=="reg"){
          print("Brak opcji na Regresje!")
        }
        else if(typ=="multi"){
          print("Brak opcji na klasyfikację Wieloklasową!")
        }
       
      
      }else if(algorytm=="sieci"){                      ### --- ### --- Neural Network --- ### --- ###

        dane_Test <- dane[indxTest,]
        dane_Train <- dane[-indxTest,]

        X_Train = as.matrix(sapply(dane_Train[,X],MinMax))
        X_Test = as.matrix(sapply(dane_Test[,X],MinMax))
        
        
        if(typ=="bin"){

          Y_Train = as.matrix(as.numeric(dane_Train[,y]))
          Y_Test = as.matrix(as.numeric(dane_Test[,y]))

          NN_model_Bin <- trainNN_old( X_Train, Y_Train, h = hiper$h[1], lr = hiper$lr, iter = hiper$iter, seed = 123, typ = "binarna")
          NN_predict_Bin_Train <- predNN_old( X_Train, NN_model_Bin, typ = "binarna")
          NN_predict_Bin_Test <- predNN_old( X_Test, NN_model_Bin, typ = "binarna")

          Train_ocena = ModelOcena(dane_Train[, y], NN_predict_Bin_Train)
          Test_ocena = ModelOcena(dane_Test[, y], NN_predict_Bin_Test)
          Train_miary = Train_ocena[[3]]
          Test_miary = Test_ocena[[3]]
          
          Czulosc_Train <- append(Czulosc_Train, Train_miary["Czulosc"])
          Specyficznosc_Train  <- append(Specyficznosc_Train, Train_miary["Specyficznosc"])
          Jakosc_Train  <- append(Jakosc_Train, Train_miary["Jakosc"])
          
          Czulosc_Test <- append(Czulosc_Test, Test_miary["Czulosc"])
          Specyficznosc_Test  <- append(Specyficznosc_Test, Test_miary["Specyficznosc"])
          Jakosc_Test <- append(Jakosc_Test, Test_miary["Jakosc"])
        
        }else if(typ=="multi"){

          klasy = levels(dane[,y])
          Y_Train = model.matrix( ~ dane_Train[,y] - 1, dane_Train)
          Y_Test = model.matrix( ~ dane_Test[,y] - 1, dane_Test)

          NN_model_Multi <- trainNN_old( X_Train, Y_Train, h = hiper$h[1], lr = hiper$lr, iter = hiper$iter, seed = 123, typ = "wieloklasowa")
          NN_predict_Multi_Train <- predNN_old( X_Train, NN_model_Multi, typ = "wieloklasowa")
          NN_predict_Multi_Test <- predNN_old( X_Test, NN_model_Multi, typ = "wieloklasowa")

          NNpred_Train <- as.numeric(klasy[apply( NN_predict_Multi_Train, 1, which.max )])
          NNpred_Test <- as.numeric(klasy[apply( NN_predict_Multi_Train, 1, which.max )])
          
          Jakosc_Train <- append(Jakosc_Train, Trafnosc_new(NNpred_Train, dane_Train[,y]))
          Jakosc_Test <- append(Jakosc_Test, Trafnosc_new(NNpred_Test, dane_Test[,y]))
        
        }else if(typ=="reg"){

          Y_Train = as.matrix(MinMax(dane_Train[,y]))
          Y_Test = as.matrix(MinMax(dane_Test[,y]))
          Y_min = min(dane[,y])
          Y_max = max(dane[,y])

          NN_model_Reg <- trainNN_old( X_Train, Y_Train, h = hiper$h[1], lr = hiper$lr, iter = hiper$iter, seed = 123, typ = "regresja")
          NN_predict_Reg_Train <- predNN_old( X_Train, NN_model_Reg, typ = "regresja")
          NN_predict_Reg_Test <- predNN_old( X_Test, NN_model_Reg, typ = "regresja")

          NN_predict_Reg_Train_Scale <- MinMaxOdwrot(NN_predict_Reg_Train, Y_min, Y_max)
          NN_predict_Reg_Test_Scale <- MinMaxOdwrot(NN_predict_Reg_Test, Y_min, Y_max)
          
          Train_ocena = ModelOcena(dane_Train[,y], NN_predict_Reg_Train_Scale)
          Test_ocena = ModelOcena(dane_Test[,y], NN_predict_Reg_Test_Scale)
          
          MAE_Train = append(MAE_Train, Train_ocena["MAE"])
          MSE_Train = append(MSE_Train, Train_ocena["MSE"])
          MAPE_Train = append(MAPE_Train, Train_ocena["MAPE"])
          
          MAE_Test = append(MAE_Test, Test_ocena["MAE"])
          MSE_Test = append(MSE_Test, Test_ocena["MSE"])
          MAPE_Test = append(MAPE_Test, Test_ocena["MAPE"])
        }


      }
    
    }
      
    if(typ=="reg"){
      
      mae_train = append(mae_train, mean(MAE_Train))
      mse_train = append(mse_train, mean(MSE_Train))
      mape_train = append(mape_train, mean(MAPE_Train))
      
      mae_test = append(mae_test, mean(MAE_Test))
      mse_test = append(mse_test, mean(MSE_Test))
      mape_test = append(mape_test, mean(MAPE_Test))
    }
    else if(typ=="bin"){
      
      czulosc_train <- append(czulosc_train, mean(Czulosc_Train))
      specyficznosc_train  <- append(specyficznosc_train, mean(Specyficznosc_Train))
      jakosc_train  <- append(jakosc_train, mean(Jakosc_Train))
      
      czulosc_test <- append(czulosc_test, mean(Czulosc_Test))
      specyficznosc_test  <- append(specyficznosc_test, mean(Specyficznosc_Test))
      jakosc_test <- append(jakosc_test, mean(Jakosc_Test))
    }
    else if(typ=="multi"){
      jakosc_train <- append(jakosc_train, mean(Jakosc_Train))
      jakosc_test <- append(jakosc_test, mean(Jakosc_Test))
    }
  }#parTune
  
  if(typ=="reg"){
    wyniki <- data.frame(matrix(0, nrow(parTune), ncol=6))
    colnames(wyniki) = c("MAE_TRAIN", "MSE_TRAIN", "MAPE_TRAIN", "MAE_TEST", "MSE_TEST", "MAPE_TEST")
    wyniki[,1] = mae_train
    wyniki[,2] = mse_train
    wyniki[,3] = mape_train
    wyniki[,4] = mae_test
    wyniki[,5] = mse_test
    wyniki[,6] = mape_test
  }
  else if(typ=="bin"){
    wyniki <- data.frame(matrix(0, nrow(parTune), ncol=6))
    colnames(wyniki) = c("Czulosc_TRAIN", "Specyficznosc_TRAIN", "Jakosc_TRAIN", "Czulosc_TEST", "Specyficznosc_TEST", "Jakosc_TEST")
    # print(czulosc_train)
    wyniki[,1] = czulosc_train
    wyniki[,2] = specyficznosc_train
    wyniki[,3] = jakosc_train
    wyniki[,4] = czulosc_test
    wyniki[,5] = specyficznosc_test
    wyniki[,6] = jakosc_test
  }
  else if(typ=="multi"){
    wyniki <- data.frame(matrix(0, nrow(parTune), ncol=2))
    colnames(wyniki) = c("Jakosc_TRAIN", "Jakosc_TEST")
    wyniki[,1] = jakosc_train
    wyniki[,2] = jakosc_test
  }
  wyniki = cbind(parTune, wyniki)
  return( wyniki )
}#CrossValidTune











