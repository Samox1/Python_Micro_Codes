### ------------------------ ###
### --- Drzewo Decyzyjne --- ###
### ------------------------ ###



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
      n <- nrow(X)
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
  roc_obj <- roc(y_tar, y_hat, quiet = TRUE)
  TPR <- rev(roc_obj$sensitivities) #czu?o?? posortowana rosn?co
  FPR <- rev(1 - roc_obj$specificities) #1 - specyficzno?? posortowana rosn?co
  dFPR <- c(diff(FPR), 0) #r??nica pomi?dzy kolejnymi warto?ciami czu?o?ci
  dTPR <- c(diff(TPR), 0)
  AUC_wynik <- sum(TPR * dFPR) + sum(dTPR * dFPR)/2
  return(AUC_wynik)
}

J <- function(y_tar, y_hat){
  roc_obj <- roc(y_tar, y_hat, quiet = TRUE) 
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




Krosswalidacja_param <- function(Dane, Dane_Y, Dane_Y_Y, k_folds=5, typ_danych="bin", model="tree", algorytm="R", tree_minsplit=1, tree_maxdepth=5, knn_k=5, svm_cost=100, svm_lr=0.001, svm_maxiter=500, seed=1234) {

  set.seed(seed)
  folds_list <- createFolds(Dane_Y, k = k_folds, list = TRUE, returnTrain = FALSE)
  
  
  if (typ_danych == "bin") {
    AUC <- vector()
    TPR <- vector()
    FPR <- vector()
    ACC <- vector()
  }
  
  if (typ_danych == "class") {
    ACC <- vector()
  }
  
  if (typ_danych == "reg") {
    MAE <- vector()
    MSE <- vector()
    MAPE <- vector()
  }
  

  for (folds_num in 1:k_folds) {
    print(folds_num)
    
    train <- Dane[-folds_list[[folds_num]],]
    test <- Dane[folds_list[[folds_num]],]
    train_Y <- Dane_Y_Y[-folds_list[[folds_num]]]
    test_Y <- Dane_Y_Y[folds_list[[folds_num]]]
  

    if (typ_danych == "bin") {

      if (model == "tree") {
        Drzewko_Bin_rpart = rpart( formula = Y_out ~ . , data = train, minsplit = tree_minsplit, maxdepth = tree_maxdepth)
        pred_Drzewko_Bin_rpart_class <- predict(Drzewko_Bin_rpart, newdata = test, type="class")
        pred_Drzewko_Bin_rpart <- predict(Drzewko_Bin_rpart, newdata = test, type="prob")[,2]
        Ocena <- ModelOcena(test$Y_out, pred_Drzewko_Bin_rpart)
        AUC <- append(AUC, Ocena[1])
        TPR <- append(TPR, Ocena[2])
        FPR <- append(FPR, Ocena[3])
        ACC <- append(ACC, Ocena[4])
      }

      if (model == "knn") {

        if (algorytm == "my") {
          knn_model_Bin <- KNNtrain(train[-5], train_Y, k=knn_k, 0, 1)
          pred_knn_Bin <- KNNpred(knn_model_Bin, test[-5])
          Ocena <- ModelOcena(test$Y_out, pred_knn_Bin)
          AUC <- append(AUC, Ocena[1])
          TPR <- append(TPR, Ocena[2])
          FPR <- append(FPR, Ocena[3])
          ACC <- append(ACC, Ocena[4])
        }

        if (algorytm == "R") {
          knn_model_Bin_caret <- knn3(Y_out ~ . , data = train, k=knn_k)
          pred_knn_model_Bin_caret <- predict(knn_model_Bin_caret, test, type="prob")[,2]
          Ocena <- ModelOcena(test$Y_out, pred_knn_model_Bin_caret)
          AUC <- append(AUC, Ocena[1])
          TPR <- append(TPR, Ocena[2])
          FPR <- append(FPR, Ocena[3])
          ACC <- append(ACC, Ocena[4])
        }
      }

      if (model == "svm") {

        if (algorytm == "my") {
          train_Y_class <- ifelse( train_Y == 0, -1, train_Y )
          SVM_model_Bin <- trainSVM( as.matrix(train[-5]), train_Y_class, C = svm_cost, lr = svm_lr, maxiter = svm_maxiter )
          pred_SVM_model_Bin <- predSVM( as.matrix(train[-5]), SVM_model_Bin$Theta, SVM_model_Bin$Theta0)
          Ocena <- ModelOcena(train$Y_out, as.vector(pred_SVM_model_Bin))
          AUC <- append(AUC, Ocena[1])
          TPR <- append(TPR, Ocena[2])
          FPR <- append(FPR, Ocena[3])
          ACC <- append(ACC, Ocena[4])
        }

        if (algorytm == "R") {
          SVM_model_Bin_e1071 <- svm(Y_out ~ . , data = train, probability = TRUE, cost = svm_cost)
          pred_SVM_model_Bin_e1071 <- predict(SVM_model_Bin_e1071, train, probability = TRUE)
          prob_SVM_model_Bin_e1071 <- attr(pred_SVM_model_Bin_e1071, "probabilities")[,1]
          Ocena <- ModelOcena(train$Y_out, prob_SVM_model_Bin_e1071)
          AUC <- append(AUC, Ocena[1])
          TPR <- append(TPR, Ocena[2])
          FPR <- append(FPR, Ocena[3])
          ACC <- append(ACC, Ocena[4])
        }
      }

    }

    if (typ_danych == "class") {
      
      if (model == "tree") {
        Drzewko_Class_rpart = rpart( formula = V35 ~. , data = train, minsplit = tree_minsplit, maxdepth = tree_maxdepth, method = "class")
        pred_Drzewko_Class_rpart_class <- predict(Drzewko_Class_rpart, newdata = test, type="class")
        ACC <- append(ACC, (length(test[test$V35 == pred_Drzewko_Class_rpart_class,35]) / length(test$V35)))
      }

      if (model == "knn") {

        if (algorytm == "my") {
          knn_model_Class <- KNNtrain(train[-35], train$V35, k=knn_k, 0, 1)
          pred_knn_Class <- KNNpred(knn_model_Class, test[-35])
          ACC <- append(ACC, (length(test[test$V35 == pred_knn_Class$Klasa,35]) / length(test$V35)))
        }

        if (algorytm == "R") {
          knn_model_Class_caret <- knn3(formula = V35 ~ . , data = train, k = knn_k)
          pred_knn_model_Class_caret <- predict(knn_model_Class_caret, test, type="class")
          ACC <- append(ACC, (length(test[test$V35 == pred_knn_model_Class_caret,35]) / length(test$V35)))
        }
      }
    }

    if (typ_danych == "reg") {
      
      if (model == "tree") {
        Drzewko_Reg_rpart = rpart( formula = Wytrzymalosc ~ . , data = train, method = "anova", minsplit = tree_minsplit, maxdepth = tree_maxdepth)
        pred_Drzewko_Reg_rpart <- as.numeric(predict(Drzewko_Reg_rpart, newdata = test))
        Ocena <- ModelOcena(train$Wytrzymalosc, pred_Drzewko_Reg_rpart)
        MAE <- append(MAE, Ocena[1])
        MSE <- append(MSE, Ocena[2])
        MAPE <- append(MAPE, Ocena[3])
      }

      if (model == "knn") {

        if (algorytm == "my") {
          knn_model_Reg <- KNNtrain(train.data[,-9], train$Wytrzymalosc, k=knn_k, 0, 1)
          pred_knn_Reg <- KNNpred(knn_model_Reg, test[,-9])
          Ocena <- ModelOcena(test$Wytrzymalosc, pred_knn_Reg)
          MAE <- append(MAE, Ocena[1])
          MSE <- append(MSE, Ocena[2])
          MAPE <- append(MAPE, Ocena[3])
        }

        if (algorytm == "R") {
          knn_model_Reg_caret <- knn3(Wytrzymalosc ~ . , data = train, k=knn_k)
          pred_knn_model_Reg_caret <- predict(knn_model_Reg_caret, test)
          Ocena <- ModelOcena(test$Wytrzymalosc, pred_knn_model_Reg_caret)
          MAE <- append(MAE, Ocena[1])
          MSE <- append(MSE, Ocena[2])
          MAPE <- append(MAPE, Ocena[3])
        }
      }
    }

  }
  
  if (typ_danych == "bin") {
    wynik_sredni <- c("AUC_mean" = mean(AUC), "TPR_mean" = mean(TPR), "FPR_mean" = mean(FPR), "ACC_mean" = mean(ACC))
  }
  
  if (typ_danych == "class") {
    wynik_sredni <- c("ACC_mean" = mean(ACC))
  }
  
  if (typ_danych == "reg") {
    wynik_sredni <- c("MAE_mean" = mean(MAE), "MSE_mean" = mean(MSE), "MAPE_mean" = mean(MAPE))
  }
  
  return(wynik_sredni)

}




