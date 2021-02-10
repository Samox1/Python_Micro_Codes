### ------------------------ ###
### --- Drzewo Decyzyjne --- ###
### ------------------------ ###

library(data.tree)

Prob <- function( y ){
  res <- unname( table( y ) )
  res <- res / sum( res )
  return( res )
}


Entropy <- function( prob ){
  res <- prob * log2( prob )
  res[ prob == 0 ] <- 0
  res <- -sum( res )
  return( res )
}


SplitNum <- function(Y, x, parentVal, splits, minobs){
  n <- length(x)
  res <- data.frame(matrix(0,length(splits),6))
  colnames(res) <- c("InfGain","lVal","rVal","point","ln","rn")
  for(i in 1:length(splits)){
    partition <- x <= splits[i]
    ln <- sum(partition)
    rn <- n - ln
    
    if(any(c(ln,rn) < minobs, na.rm = TRUE)){
      res[i,] <- 0
    }else{
      lVal <- Entropy(Prob(Y[partition]))
      rVal <- Entropy(Prob(Y[!partition]))
      InfGain <- parentVal - (ln/n * lVal + rn/n * rVal)
      res[i,"InfGain"] <- InfGain
      res[i,"lVal"] <- lVal
      res[i,"rVal"] <- rVal
      res[i,"point"] <- splits[i]
      res[i,"ln"] <- ln
      res[i,"rn"] <- rn
    }
  }
  return(res)
}


SplitVar <- function(Y, x, parentVal, minobs){
  s <- unique(x)
  if(length(x) == 1){
    splits <- s
  }else{
    splits <- head(sort(s),-1)
  }
  res <- SplitNum(Y, x, parentVal, splits, minobs)
  incl <- res$ln >= minobs & res$rn >= minobs & res$InfGain > 0 
  res <- res[ incl, , drop = F ]
  best <- which.max( res$InfGain )
  
  res <- res[ best, , drop = F ]
  return( res )
}


FindBestSplit <- function( Y, Xnames, data, parentVal, minobs ){
  res <- sapply( Xnames, function( i ){
    SplitVar( Y = data[,Y] , x = data[,i], parentVal = parentVal, minobs = minobs )
  }, simplify = F )
  res <- do.call( rbind, res )
  best <- which.max( res$InfGain )
  res <- res[ best, , drop = F ]
  return( res )
}


BuildTree <- function( node, Y, Xnames, data, depth, minobs ){
  node$Count <- nrow( data )
  node$Prob <- Prob( data[,Y] )
  node$Class <- levels( data[,Y] )[ which.max(node$Prob) ]
  bestSplit <- FindBestSplit( Y, Xnames, data, node$Val, minobs )
  ifStop <- nrow( bestSplit ) == 0 
  if( node$Depth == depth | ifStop | all( node$Prob %in% c(0,1) ) ){
    node$Leaf <- "*"
    return( node )
  }
  splitIndx <- data[, rownames(bestSplit) ] <= bestSplit$point
  childFrame <- split( data, splitIndx )
  namel <- sprintf( "%s <= %s",  rownames(bestSplit), bestSplit$point )
  childL <- node$AddChild( namel )
  childL$Depth <- node$Depth + 1
  childL$Val <- bestSplit$lVal
  BuildTree( childL, Y, Xnames, childFrame[["TRUE"]], depth, minobs )
  namer <- sprintf( "%s >  %s",  rownames(bestSplit), bestSplit$point )
  childR <- node$AddChild( namer )
  childR$Depth <- node$Depth + 1
  childR$Val <- bestSplit$rVal
  BuildTree( childR, Y, Xnames, childFrame[["FALSE"]], depth, minobs )
}


Tree <- function( Y, Xnames, data, depth, minobs ){
  tree <- Node$new( "Root" )
  tree$Depth <- 0
  tree$Count <- nrow( data )
  tree$Val <- Entropy( Prob( data[,Y] ) )
  BuildTree( tree, Y, Xnames, data, depth, minobs )
  return( tree )
}



### --- knn --- ###

KNNtrain <- function(X, y_tar, k, XminNew, XmaxNew){
  if(all(!is.na(X)) & all(!is.na(y_tar)) & k>0 & (is.matrix(X) | is.data.frame(X))){
    if(is.matrix(X)){
      X_norm <- matrix(0,nrow(X),ncol(X))
    }
    else if(is.data.frame(X)){
      X_norm <- matrix(0,nrow(X),ncol(X))
      X_norm <- as.data.frame(X_norm)
    }
    minOrg <- c()
    maxOrg <- c()
    for (i in 1:ncol(X)) {
      if(is.numeric(X[,i])){
        for (j in 1:nrow(X)) {
          X_norm[j,i] <- ( (X[j,i] - min(X[,i])) / (max(X[,i]) - min(X[,i])) ) * (XmaxNew - XminNew) + XminNew
          minOrg[i] <- min(X[,i])
          maxOrg[i] <- max(X[,i])
        }
      }
      else if(is.factor(X[,i]) & is.ordered(X[,i]) | is.factor(X[,i])){
        X_norm[,i] <- X[,i]
        minOrg[i] <- NA
        maxOrg[i] <- NA
      }
      else{
        c("Niepoprawne dane")
      }
    }
    attr(X_norm,"minOrg") <- minOrg
    attr(X_norm,"maxOrg") <- maxOrg
    attr(X_norm,"minmaxNew") <- c("min"=XminNew,"max"=XmaxNew)
    
    knn <- list()
    knn[["X"]] <- X_norm
    knn[["y"]] <- y_tar
    knn[["k"]] <- k
    return( knn )
  }
  else{
    c("Niepoprawne dane")
  }  
}


KNNpred <- function(KNNmodel, X){
  if(all(!is.na(X)) & ncol(KNNmodel$X)==ncol(X)){
    if(is.matrix(X)){
      X_norm <- matrix(0,nrow(X),ncol(X))
    }
    if(is.data.frame(X)){
      X_norm <- matrix(0,nrow(X),ncol(X))
      X_norm <- as.data.frame(X_norm)
    }
    num <- 0
    fac_o <- 0
    fac <- 0
    for (i in 1:ncol(X)) {
      if(is.numeric(X[,i])){
        num <- num + 1
        for (j in 1:nrow(X)) {
          X_norm[j,i] <- ((X[j,i] - attr(KNNmodel$X,"minOrg")[i]) / 
                            (attr(KNNmodel$X,"maxOrg")[i] - attr(KNNmodel$X,"minOrg")[i]))  * 
            (attr(KNNmodel$X,"minmaxNew")["max"] - attr(KNNmodel$X,"minmaxNew")["min"]) + 
            attr(KNNmodel$X,"minmaxNew")["min"]
        }
      }
      else if(is.factor(X[,i]) & is.ordered(X[,i])){
        fac_o <- fac_o + 1
        X_norm[,i] <- X[,i]
      }
      else if(is.factor(X[,i])){
        fac <- fac + 1
        X_norm[,i] <- X[,i]
      }
    }
    odl <- matrix(0, nrow(KNNmodel$X), nrow(X_norm))
    if(num == ncol(X_norm)){ 
      for(i in 1:nrow(KNNmodel$X)){
        for(j in 1:nrow(X_norm)){
          odl[ i, j ] <- sqrt(sum( (KNNmodel$X[i,] - X_norm[j,])^2 ))
        }
      }
    }
    else if(fac_o == ncol(X_norm)){ 
      for(i in 1:nrow((KNNmodel$X))){
        for(j in 1:nrow(X_norm)){
          for (k in 1:ncol(X_norm)) {
            uniq <- length(unique(X_norm[,k]))
            odl[i, j] <- (sum( abs(as.numeric(KNNmodel$X[i,]) - as.numeric(X_norm[j,]))  / (uniq - 1)) )
          }
        }
      }
    }
    else if(fac == ncol(X_norm)){ 
      for(i in 1:nrow(KNNmodel$X)){
        for(j in 1:nrow(X_norm)){
          odl[i, j] <- ( (sum(KNNmodel$X[i,] != X_norm[j,])) / ncol(X_norm) )
          #odl[i, j] <- ( (sum(KNNmodel$X[i,] == X_norm[j,])) / ncol(X_norm) )
        }
      }
    }
    else{
      c("odległość Gowera")
    } 
    if(is.numeric(KNNmodel$y)){
      pred <- double(nrow(X_norm))
      for( i in 1:nrow(X_norm) ){
        kNaj <- order( odl[,i] )
        kNaj <- kNaj[1:KNNmodel$k]
        y_hat <- mean( KNNmodel$y[ kNaj ] )
        pred[ i ] <- y_hat
      }
      
    }
    else if(is.factor(KNNmodel$y)){
      pred <- c("klasyfikacja")
    }
    return(pred)  
  }
  else{
    c("Niepoprawne dane")
  }
}


# Zdefiniujmy λ i jako decyzję (θ T x i + θ 0 ), wtedy:
Decyzja <- function( X, theta, theta0 ){
  X %*% t( theta ) + theta0
}

# Jeśli (y i λ i ) > 1, to klasyfikator prawidłowo przewiduje znak
Margines <- function( X, y, theta, theta0 ){
  y * Decyzja( X, theta, theta0 )
}

# L = n X kθ 2 k + C max(0, 1 − y i (θ T x i + θ 0 ))
Koszt <- function( margines, theta, C ){
  ( 1/ 2 ) * theta %*% t(theta) + C * sum( pmax( 0, 1 - margines ) ) # apply( tabela, 1, max )
}

trainSVM <- function( X, y, C = 1, lr = 0.001, maxiter = 500 ){
  n <- nrow( X )
  p <- ncol( X )
  theta <- matrix( runif( p ), nrow = 1 ) 
  theta0 <- 0
  koszt <- double( maxiter )
  for( i in 1:maxiter ){
    margines <- Margines( X, y, theta, theta0 )
    koszt[i] <- Koszt( margines, theta, C )
    indxMiss <- which( margines < 1 )
    d_theta <- theta - C * y[ indxMiss ] %*% X[ indxMiss, ]
    d_theta0 <- - C * sum( y[ indxMiss ] )
    theta <- theta - lr * d_theta
    theta0 <- theta0 - lr * d_theta0
  }
  SuppVector_Niepoprawnie <- which( Margines( X, y, theta, theta0 ) <= 1 )
  return( list( Theta = theta, Theta0 = theta0, SupVecNiepop = SuppVector_Niepoprawnie ) )
}

predSVM <- function( X, theta, theta0 ){
  sign( Decyzja( X, theta, theta0 ) )
}





norm_minmax <- function(x){
  (x- min(x)) /(max(x)-min(x))
}

library("pROC")

MAE <- function(y_tar, y_hat){
  return(mean(abs(y_tar - y_hat)))
}

MSE <- function(y_tar, y_hat){
  return(mean((y_tar - y_hat)^2))
}

MAPE <- function(y_tar, y_hat){
  return( mean(abs((y_tar - y_hat)/y_tar) ))
}

AUC_MT <- function(y_tar, y_hat){
  roc_obj <- roc(y_tar, y_hat)
  TPR <- rev(roc_obj$sensitivities) #czu?o?? posortowana rosn?co
  FPR <- rev(1 - roc_obj$specificities) #1 - specyficzno?? posortowana rosn?co
  dFPR <- c(diff(FPR), 0) #r??nica pomi?dzy kolejnymi warto?ciami czu?o?ci
  dTPR <- c(diff(TPR), 0)
  AUC_wynik <- sum(TPR * dFPR) + sum(dTPR * dFPR)/2
  return(AUC_wynik)
}

J <- function(y_tar, y_hat){
  roc_obj <- roc(y_tar, y_hat) 
  TPR <- roc_obj$sensitivities #czu?o??
  FPR <- roc_obj$specificities #specyficzno??
  max_J <- 0
  for (i in 1:length(TPR)) {
    J <- (TPR[i] + FPR[i]-1)
    if(J > max_J){
      max_J=J
    }
  }
  return(max_J)
}


Czulosc <- function(Mat){
  return((Mat[1] / (Mat[1] + Mat[3])))
}
Specyficznosc <- function(Mat){
  return((Mat[4] / (Mat[4] + Mat[2])))
}

Jakosc <- function(Mat){
  return(((Mat[1] + Mat[4]) / (Mat[1] + Mat[2] + Mat[3] + Mat[4])))
}

ModelOcena <- function(y_tar, y_hat){
  if(is.numeric(y_tar)){
    regresja <- c("MAE" = MAE(y_tar, y_hat), "MSE" = MSE(y_tar, y_hat), "MAPE" = MAPE(y_tar, y_hat))
    return(regresja)
  }
  else if(is.factor(y_tar)){
    Mat <- table(y_tar, y_hat = ifelse(y_hat <= J(y_tar, y_hat), 0, 1))
    miary <- c( "AUC" = AUC_MT(y_tar, y_hat), "Sensitivity" = Czulosc(Mat), "Specificity" = Specyficznosc(Mat), "Accuracy" = Jakosc(Mat))
    klasyfikacja <- list(Mat, J(y_tar, y_hat), miary)
    return(miary)
  }
  else{
    c("Niepoprawne dane!")
  }
}


CM.large <- function(org.class, pred.class) {
  CM <- table(org.class, pred.class)
  sums <- apply(CM, 1, sum)
  
  ACC <- sum(diag(CM)) / sum(CM)
  
  TP <- CM[2,2]
  TN <- CM[1,1]
  #FN <- CM[1,2]
  #FP <- CM[2,1]
  
  #FPR <- TP / (TP+FN)      # Sensitivity
  #TPR <- TN / (TN+FP)      # Specificity
  
  TPR <- TP / sums[2]
  FPR <- 1 - TN / sums[1]
  
  return(c(Accuracy = round(ACC,4), TPR_Sensitivity = round(TPR, 4), FPR_Specificity = round(FPR, 4), TP = TP, TN = TN, row.names = NULL))
}


library(ROCit)

ModelOcena_Class <- function(y_pred, y_source){
  
  CM <- table(y_source, y_pred)
  TP <- CM[2,2]
  TN <- CM[1,1]
  
  rocit_model <- rocit(as.numeric(y_pred), y_source)
  TPR <- rocit_model[["TPR"]][2]
  FPR <- rocit_model[["FPR"]][2]
  AUC <- rocit_model[["AUC"]]
  Accuracy <- (TP+TN) / sum(CM)
    
  return( c("AUC" = AUC, "Sensitivity" = TPR, "1-Specificity" = FPR, "Accuracy" = Accuracy, "TP"=TP, "TN"=TN) )
}









