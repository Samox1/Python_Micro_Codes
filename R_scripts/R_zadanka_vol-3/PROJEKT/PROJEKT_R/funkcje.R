library(Rcpp)
library(dplyr)
library(data.tree)
library(caret)
library(rpart)
library(neuralnet)
library(tidyverse)
library(pROC)


set.seed(123)
#KNN

MinMax <- function( x, new_min = 0, new_max = 1 ){
  return( ( ( x - min(x) ) / ( max(x) - min(x) ) ) * ( new_max - new_min ) + new_min )
}

KNN_train <- function( X, y_tar, k = 5,XminNew,XmaxNew ){
  knn <- list()
  knn$X <- X
  knn$y <- y_tar
  knn$k <- k

  if(is.na(X)==FALSE  && is.na(y_tar)==FALSE && k>0 && (class(X)== "data.frame" || class(X)=="matrix")){
    Xnew <- X
    for( i in 1:ncol(Xnew)){
      Xnew[,i] <- MinMax(X[,i], new_min = XminNew, new_max = XmaxNew)
    }
    
    minimum <- c()
    maksimum <- c()
    for( i in 1:ncol(X)){
      minimum[i] <- min(X[,i])
      maksimum[i] <- max(X[,i])
    }
    
    attr(Xnew, "minOrg") <- minimum
    knn$minOrg <- attr(Xnew, "minOrg")
    
    attr(Xnew, "maxOrg") <- maksimum
    knn$maxOrg <- attr(Xnew, "maxOrg")
    
    minmaxnew <- c(XminNew, XmaxNew)
    attr(Xnew, "minmaxNew") <- Xnew

    knn$minmaxNew <- Xnew
    knn$min<-XminNew
    knn$max<-XmaxNew
    knn$X<-Xnew
  }
  return(knn )
}


c_gower_vec_matrix_dist = cppFunction(plugins = "openmp",
'NumericVector c_gower_vec_matrix_cat_dist(IntegerMatrix x, IntegerVector x2) 
{
  NumericVector res(x.nrow());
  #pragma omp parallel for 
  for(int i = 0; i < x.nrow(); i++) 
  {
    double dist = 0;
    for(int col = 0; col < x.ncol(); col++) 
    {
      dist = dist + (x2[col] == x(i, col)?0:1);
    }
    res[i] = dist/(double)x.ncol();
  }
  return res;
}')

knn_w_gower <- function(x,y,x2,k,verbose=TRUE) {
  x_m <- data.matrix(x)
  x2_m <- data.matrix(x2)
  
  res <- c()
  
  for(i in 1:nrow(x2)) 
  {
    dist <- c_gower_vec_matrix_dist(x_m, x2_m[i,])
    dist_order <- order(dist)
    kt <- table(y[dist_order[1:k]])
    res <- bind_rows(res, prop.table(kt))
  }
  return(res)
}


d_euklides <- function( x_i, x_n ){
  return( sqrt( sum( ( x_i - x_n )^2 ) ) )
}


KNN_pred <- function( model, Xnew ){
  nTrain <- nrow( model$X )
  nPred <- nrow( Xnew )
  odl <- matrix( 0, nTrain, nPred )
  for( i in 1:nTrain )
  {
    for( j in 1:nPred )
    {
      odl[i,j] <- d_euklides( model$X[i,], Xnew[j,] )
    }
    n<-0
    p <- 0
    z <- 0
    
    if(any(is.na(Xnew))==FALSE&& ncol(model$X)==ncol(Xnew))
    {
      Xnew1 <- Xnew
      for( i in 1:ncol(Xnew1))
      {
        if(is.numeric(Xnew1[,i]))
        {
          n<-n+1
          Xnew1[,i] <- MinMax(Xnew[,i], new_min = model$min, new_max = model$max)
        }
        else if(is.factor(X[,i]) & is.ordered(X[,i]))
        {
          p<- p + 1
          Xnew1[,i] <- X[,i] 
        }
        else if(is.factor(X[,i]))
        {
          fac <- fac + 1
          Xnew1[,i] <- X[,i]
        }
      }
      odl <- matrix(0, nrow(model$X), nrow(Xnew1))
      if(n == ncol(Xnew1)){ 
        for(i in 1:nrow(model$X)){
          for(j in 1:nrow(Xnew1)){
            odl[ i, j ] <- d_euklides( model$X[i,], Xnew1[j,] )
          }
        }
      }
      else if(p == ncol(Xnew1)){
        for(i in 1:nrow((model$X))){
          for(j in 1:nrow(Xnew1)){
            for (k in 1:ncol(Xnew1)) {
              q <- length(unique(Xnew1[,k]))
              odl[i, j] <- (sum( abs(as.numeric(model$X[i,]) - as.numeric(Xnew1[j,]))  / (q - 1)) )
            }
          }
        }
      }
      else if(z == ncol(Xnew1)){ 
        for(i in 1:nrow(model$X)){
          for(j in 1:nrow(Xnew1)){
            odl[i, j] <- ( (sum(model$X[i,] != Xnew1[j,])) / ncol(Xnew1) )
          }
        }
      }
      else {
        return(0)
      }
      if(is.numeric(model$y)){
        pred <- double( nPred )
        for( j in 1:nPred ){
          kNaj <- order( odl[,j] )
          # print(kNaj)
          kNaj <- kNaj[1:model$k]
          # print(kNaj)
          y_pred <- mean( model$y[kNaj] )
          # print( y_pred )
          pred[j] <- y_pred
        }
      }else if(is.factor(model$y)){
        pred <-knn_w_gower(model$X,model$y, Xnew1,model$k) %>% mutate('class'=names(.)[apply(.,1,which.max)])
      }
    }
    else return(0)
  return(pred)
  }
}





#Drzewa decyzyjne

StopIfNot <- function(Y, X, data, type, depth, minobs, overfit, cf){
  
  error_liczba <- 0
  
  if( !is.data.frame(data)){
    warning("Dane to nie ramka danych")
    error_liczba <- error_liczba + 1
  }
  
  if(all(Y %in% colnames(data)) == FALSE || all(X %in% colnames(data)) == FALSE ){
    warning("kolumny 'Y' lub 'X' nie wystepuja w tabeli")
    error_liczba <- error_liczba + 1 
  }
  else{
    
    if(any(is.na(data[,X])) || any(is.na(data[,Y]))){
      warning("Y lub X ma braki danych")
      error_liczba <- error_liczba + 1
    }
    
    if( ((is.factor(data[,Y]) && type=="SS") || (is.numeric(data[,Y]) && type=="Gini") || (is.numeric(data[,Y]) && type == "Entropy")) == TRUE){
      warning("kombinacja parametrow Y i type nie ma sensu")
      error_liczba <- error_liczba + 1
    }
  }
  
  if(depth <= 0 || minobs <= 0){
    warning("depth lub minobs nie jest wieksze od 0")
    error_liczba <- error_liczba + 1
  }
  
  if((type == "Gini" || type == "SS" || type=="Entropy") == FALSE){
    warning("type nie jest:  Gini,  Entropy , SS")
    error_liczba <- error_liczba + 1
  }
  
  if((overfit == "none" || overfit =="prune") == FALSE){
    warning("overfit nie jest 'none' lub  'prune'")
    error_liczba <- error_liczba + 1
  }
  
  if((cf >0 && cf <=0.5) == FALSE){
    warning("cf nie nalezy do przedzialu (0,0.5]")
    error_liczba <- error_liczba + 1
  }
  
  if(error_liczba > 0){
    print(paste0("Liczba problemow: ", error_liczba))
    return(FALSE)
  }
  else{
    return(TRUE)
  }
  
}


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

SS <- function(n,Y){
  res <- (Y-(1/n) * sum (Y))^2
  res <- sum (res)
  return(res)
}

Gini <- function(prob){
  res <- prob^2
  res <- sum(res)
  res <- 1-res
  return (res)
}


AssignInitialMeasures <- function(tree, Y, data, type, depth){
  tree$Depth <- depth
  wynik <- 0
  
  if (type=="Gini"){
    wynik = Gini(Prob(data[,Y]))
    tree$Val <- wynik
  }
  else if (type=='Entropy'){
    wynik = Entropy(Prob(data[,Y]))
    tree$Val <- wynik
  }
  else if (type=='SS'){
    wynik = SS(nrow(data[,Y]), data[,Y])
    tree$Val <- wynik
  }
  return (tree)
}


AssignInfo <- function(tree, Y, X, data, type, depth, minobs, overfit, cf){
  attr(tree, "Y") <- Y
  attr(tree, "X") <- X
  attr(tree, "data") <- data
  attr(tree, "type") <- type
  attr(tree, "depth") <- depth
  attr(tree, "minobs") <- minobs
  attr(tree, "overfit") <- overfit
  attr(tree, "cf") <- cf
  return(tree)
}


SplitNum <- function( Y, X, parentVal, splits, type, minobs ){
  n <- length(X)
  res <- data.frame(matrix(0, length(splits), 6))
  
  colnames( res ) <- c("InfGain","lVal","rVal","point","ln","rn")
  
  for( i in 1:length(splits)){
    
    partition <- X <= splits[i]
    ln <- sum(partition)
    rn <- n - ln
    
    if(any(10 < minobs)){
      res[i,] <- 0
    }else{
      if(type=="Entropy"){
        lVal <- Entropy(Prob(Y[partition]))
        rVal <- Entropy(Prob(Y[!partition]))
      }else if(type=="Gini"){
        lVal <- Gini(Prob(Y[partition]))
        rVal <- Gini(Prob(Y[!partition]))
      }else if(type=="SS"){
        lVal <- SS(nrow(Y[partition]),Y[partition])
        rVal <- SS(nrow(Y[!partition]),Y[!partition])
      }
      
      InfGain <- parentVal - ( ln/n * lVal + rn/n * rVal )
      
      res[i,"InfGain"] <- InfGain
      res[i,"lVal"] <- lVal
      res[i,"rVal"] <- rVal
      res[i,"point"] <- ifelse(is.numeric(splits[i]), splits[i], as.character(splits[i]))
      res[i,"ln"] <- ln
      res[i,"rn"] <- rn
    }
  }
  return( res )
}


SplitVar <- function( Y, X, parentVal, type, minobs ){
  
  s <- unique(X)
  
  if(length(X) == 1){
    splits <- s
  }else{
    splits <- head(sort(s), -1)
  }
  
  res <- SplitNum(Y, X, parentVal, splits, type, minobs )
  incl <- res$ln >= minobs & res$rn >= minobs & res$InfGain > 0 
  res <- res[incl, , drop = F]
  best <- which.max( res$InfGain )
  res <- res[best, , drop = F]
  return( res )
}


FindBestSplit <- function( Y, Xnames, data, parentVal, type, minobs ){
  
  if(is.numeric(data[,Y])){ 
    for(col_var in Xnames){
      if(!is.numeric(data[,col_var]) & !is.ordered(data[,col_var])){ 
        tmp <- tapply(data[,Y], data[,col_var], mean)                                  
        tmp <- sort(tmp)
        data[,col_var] <- factor(data[,col_var], levels = names(tmp), ordered = TRUE)
      }
    }
  }else{ 
    for(col_var in Xnames){
      if(!is.numeric(data[,col_var]) & !is.ordered(data[,col_var])){ 
        positive_class <- levels(data[,Y])[-1]                                          
        positive_rows <- data[data[,Y]==positive_class,] 
        tmp <- prop.table(table(positive_rows[,col_var]))
        tmp <- sort(tmp)
        data[,col_var] <- factor(data[,col_var], levels = names(tmp), ordered = TRUE)
      }
    }
  }
  
  res <- sapply( Xnames, function(i){
    SplitVar(Y = data[,Y] , X = data[,i], parentVal = parentVal, type = type, minobs = minobs)
  }, simplify = F)
  
  res <- do.call(rbind, res)
  best <- which.max(res$InfGain)
  res <- res[ best, , drop = F]
  
  return(res)
}


BuildTree <- function( node, Y, Xnames, data, type, depth, minobs ){
  node$Count <- nrow( data )
  node$Prob <- Prob( data[,Y] )
  node$Class <- levels( data[,Y] )[ which.max(node$Prob) ]
  
  bestSplit <- FindBestSplit( Y, Xnames, data, node$Val, type, minobs )
  
  ifStop <- nrow( bestSplit ) == 0
  if( node$Depth == depth | ifStop | all( node$Prob %in% c(0,1) ) ){
    node$Leaf <- "*"
    return( node )
  }
  
  splitIndx <- data[, rownames(bestSplit) ] <= bestSplit$point
  
  node$BestAtr <- rownames(bestSplit)
  node$SplitPoint <- bestSplit$point
  
  childFrame <- split( data, splitIndx )
  
  namel <- sprintf( "%s <= %s",  rownames(bestSplit), bestSplit$point )
  
  childL <- node$AddChild( namel )
  childL$Depth <- node$Depth + 1
  childL$Val <- bestSplit$lVal
  BuildTree( childL, Y, Xnames, childFrame[["TRUE"]], type, depth, minobs )
  
  namer <- sprintf( "%s >  %s",  rownames(bestSplit), bestSplit$point )
  
  childR <- node$AddChild( namer )
  childR$Depth <- node$Depth + 1
  childR$Val <- bestSplit$rVal
  BuildTree( childR, Y, Xnames, childFrame[["FALSE"]], type, depth, minobs )
}




Tree <- function(Y, X, data, type, depth, minobs, overfit, cf){
  
  if(StopIfNot (Y=Y, X=X, data=data, type=type, depth=depth, minobs=minobs, overfit=overfit, cf=cf) == FALSE){
    stop("Zle dane lub parametry wejsciowe!")
  }
  
  tree <- Node$new("Root")
  tree$Count <- nrow(data)
  
  AssignInfo(tree, Y=Y, X=X, data=data, type=type, depth=depth, minobs=minobs, overfit=overfit, cf=cf)
  
  AssignInitialMeasures(tree, Y=Y, data=data, type=type, depth=0)
  
  BuildTree(tree, Y, X, data, type, depth, minobs)
  
  if(overfit == 'prune')
  {
    PruneTree(tree, cf)
  }
  
  return(tree)
}


Predykcja <- function(tree, data)
{
  if(tree$isLeaf){
    return(c(tree$Prob, tree$Class))
  }
  
  if(data[,tree$BestAtr] <= tree$SplitPoint){
    return(Predykcja(tree$children[[1]], data))
  }else{
    return(Predykcja(tree$children[[2]], data))
  }
}


PredictTree<-function(tree, new_data)
{ 
  
  if(all(colnames(new_data) %in% attributes(tree)$X) == FALSE )
  {
    stop("Kolumny 'Y' lub 'X' nie wystepuja w tabeli")
  }
  
  for(i in colnames(new_data)){
    if(is.factor(new_data[,i]) || is.ordered(new_data[,i])){
      if(all(levels(new_data[,i]) %in% levels(attributes(tree)$data[,i])) == FALSE){
        stop("Nieznane poziomy w nowych danych")
      }
    }
  }
  
  
  if(is.numeric(attributes(tree)$data[,attributes(tree)$Y]))
  { 
    t<-mean(attributes(tree)$data[,attributes(tree)$Y])
    return(t)
  
  }else if(is.factor(attributes(tree)$data[,attributes(tree)$Y]))
  {
    Klasy_Nowe <- c()
    
    for(i in 1:nrow(new_data))
    {
      probability <- Predykcja(tree, new_data[i,])
      Klasy_Nowe <- rbind(Klasy_Nowe, probability)
    }
    
    Klasy_Nowe <- data.frame(Klasy_Nowe)
    col_names <- c(levels(attributes(tree)$data[,attributes(tree)$Y]), "Klasa")
    colnames(Klasy_Nowe) <- col_names
    
    return(Klasy_Nowe)
  }
}



# Sieci Neuronowe

sigmoid <- function( x ){
  return( 1 / (1 + exp(-x) ) )
}

d_sigmoid <- function( x ){
  return( x * ( 1 - x ) )
}

Relu<- function( x ){
  return( ifelse( x <= 0, 0, x ) )
}

d_Relu <- function( x ){
  return( ifelse( x <= 0, 0, 1 ) )
}

bladSS <- function( y_tar, y_hat ){
  return( 0.5 * sum( ( y_tar - y_hat )^2 ) )
}

bladEntropia <- function( y_tar, y_hat ){
  return( sum( ( y_tar - log( y_hat ) ) / nrow( y_tar ) ) )
}

SoftMax <- function( x ){
  return( exp( x ) / sum( exp( x ) ) )
}

MinMax_NN <- function( x ){
  return( (x-min(x)) / (max(x)-min(x)) )
}

MinMaxOdwrot <- function( x, y_min, y_max ){
  return(  x * (y_max - y_min) + y_min )
} 


Wprzod_Nowe <- function(x, W_in, wagi, W_out, typ){
 
  z_in <- cbind( matrix( 1, nrow = nrow(x)), sigmoid(x %*% W_in))
  
  if(length(wagi[[1]]) > 1)
  {
    z_wagi <- list()
    z_wagi[[1]] <- cbind( matrix( 1, nrow = nrow(x)), sigmoid(z_in %*% (wagi[[1]])))
    
    if (typ =="bin"){
      y_hat <- sigmoid( z_wagi[[1]] %*% W_out ) # klasyfikacja binarna
    }
    else if (typ == "multi"){
      y_hat <- matrix( t( apply( z_wagi[[1]] %*% W_out, 1, SoftMax ) ), nrow = nrow(x) ) # klasyfikacja wieloklasowa
    } 
    else if (typ == "reg"){
      y_hat <- Relu(z_wagi[[1]] %*% W_out) # regresja
    }
  }
  else
  {
    z_wagi <- list(0)
    
    if (typ =="bin"){
      y_hat <- sigmoid( z_in %*% W_out ) # klasyfikacja binarna
    }
    else if (typ == "multi"){
      y_hat <- matrix( t( apply( z_in %*% W_out, 1, SoftMax ) ), nrow = nrow(x) ) # klasyfikacja wieloklasowa
    } 
    else if (typ == "reg"){
      y_hat <- (z_in %*% W_out) # regresja
    }
  }
  
  return(list(z_in = z_in, z_wagi = z_wagi, y_hat = y_hat))
}


Wstecz_Nowe <- function(x, y_tar, y_hat, z_in, z_wagi, W_in, wagi, W_out, lr, typ){
  
  if (typ =="bin"){
    dy_hat <- (y_tar - y_hat) * d_sigmoid(y_hat) # klasyfikacja binarna
  }
  else if (typ == "multi"){
    dy_hat <- (y_tar - y_hat) / nrow(x) # klasyfikacja wieloklasowa
  } 
  else if (typ == "reg"){
    dy_hat <- (y_tar - y_hat) # regresja
  }
  
  if(length(wagi[[1]]) > 1){
    dW_out <- t(z_wagi[[length(z_wagi)]]) %*% dy_hat
    dz <- list()
    dw_wagi <- list()  
    
    if(length(z_wagi) == 1){
      dz[[(length(z_wagi)+1)]] <- dy_hat %*% t(W_out) * d_sigmoid(z_wagi[[length(z_wagi)]])
      dw_wagi[[(length(z_wagi)+1)]] <- t(z_in) %*% dz[[(length(z_wagi)+1)]][,-1]
    }else{
      dz[[(length(z_wagi)+1)]] <- dy_hat %*% t(W_out) * d_sigmoid(z_wagi[[length(z_wagi)]])
      dw_wagi[[(length(z_wagi)+1)]] <- t(z_wagi[[length(z_wagi)-1]]) %*% dz[[(length(z_wagi)+1)]][,-1]
    }
    
    if(length(z_wagi) > 1)
    {
      dz[[2]] <- dz[[3]][,-1] %*% t(wagi[[2]]) * d_sigmoid(z_wagi[[1]])
      dw_wagi[[2]] <- t(z_in) %*% dz[[2]][,-1]
    }
    dz[[1]] <- dz[[2]][,-1] %*% t(wagi[[1]]) * d_sigmoid(z_in)
    dw_wagi[[1]] <- t(x) %*% dz[[1]][,-1]
    
    W_in <- (lr * dw_wagi[[1]]) + W_in
    for (w in 1:length(wagi)){
      wagi[[w]] <- (lr * dw_wagi[[w+1]]) + wagi[[w]]
    }
    W_out <- (lr * dW_out) + W_out
  }
  else
  {
    dW_out <- t(z_in) %*% dy_hat
    dz_in <-  dy_hat %*% t(W_out) * d_sigmoid(z_in)
    dW_in <- t(x) %*% dz_in[,-1]
    W_in <- (lr * dW_in) + W_in
    W_out <- (lr * dW_out) + W_out
  }
  return(list(W_in = W_in, wagi = wagi, W_out = W_out))
}



Train_NN_Nowe <- function( Yname, Xnames, data, h, lr, iteracje_max, seed, typ){
  set.seed(seed)
  h <- unlist(h, use.names = FALSE)
  
  if(length(h) > 2)
  {
    message("Sieci Neuronowe tylko dla 1 i 2 warstw!")
    return(NULL)
  }

  y_tar <- as.matrix(data[,Yname])
  x <- cbind(rep(1, nrow(data[,Xnames]) ), data[,Xnames])

  W_in <- matrix(runif(ncol(x) * h[1], -1, 1), nrow = ncol(x))
  wagi <- list()
  W_out <- matrix(runif((h[length(h)] + 1)* ncol(y_tar), -1, 1), nrow = h[length(h)] + 1 )
    
  if(length(h) > 1){
    for (i in 1:(length(h)-1)){
      wagi[[i]] <- matrix(runif((h[i]+1)*h[i+1], -1, 1), nrow = 1+h[i])
    }
  }else{
    wagi[[1]] <- 0
  }

  for(i in 1:iteracje_max){
    prop_wp <- Wprzod_Nowe(x, W_in, wagi, W_out, typ)
    prop_ws <- Wstecz_Nowe(x, y_tar, y_hat = prop_wp$y_hat, z_in = prop_wp$z_in, z_wagi = prop_wp$z_wagi, W_in, wagi, W_out, lr, typ)
    W_in <- prop_ws$W_in
    wagi <- prop_ws$wagi
    W_out <- prop_ws$W_out
    
    cat(paste( "\rIteracja uczenia Sieci Neuronowej: ", i , "/", iteracje_max))
  }

  return(list(y_hat = prop_wp$y_hat, W_in = W_in, wagi = wagi, W_out = W_out))
}



Pred_NN_Nowe <- function(x, NN, typ){
  
  if(is.null(NN))
  {
    message("Siec Neuronowa - bledne przekazanie")
    return(NULL)
  }
  
  x <- cbind(rep(1, nrow(x)), x)
  z1 <- cbind(matrix(1, nrow = nrow(x)), sigmoid(x %*% NN$W_in))

  if(length(NN$wagi[[1]]) > 1){
    z2 <- list()
    z2[[1]] <- cbind(matrix(1, nrow = nrow(x)), sigmoid(z1 %*% NN$wagi[[1]]))
    
    if(length(NN$wagi) > 1){
      for (i in 2:length(NN$wagi)){
        z2[[i]] <- cbind(matrix(1, nrow = nrow(x)), sigmoid(z2[[i-1]] %*% NN$wagi[[i]]))
      }
    }

    if (typ =="bin"){
      y_hat <- sigmoid( z2[[length(NN$wagi)]] %*% NN$W_out)    # klasyfikacja binarna
    }
    else if (typ == "multi"){
      y_hat <- matrix(t(apply( z2[[length(NN$wagi)]] %*% NN$W_out, 1, SoftMax)), nrow = nrow(x))    # klasyfikacja wieloklasowa
    } 
    else if (typ == "reg"){
      y_hat <- z2[[length(NN$wagi)]] %*% NN$W_out   # regresja
    }
  }
  else
  {
    y_hat <- sigmoid(z1 %*% NN$W_out)
    
    if (typ =="bin"){
      y_hat <- sigmoid( z1 %*% NN$W_out)    # klasyfikacja binarna
    }
    else if (typ == "multi"){
      y_hat <- matrix(t(apply( z1 %*% NN$W_out, 1, SoftMax)), nrow = nrow(x))    # klasyfikacja wieloklasowa
    } 
    else if (typ == "reg"){
      y_hat <- z1 %*% NN$W_out   # regresja
    }
  }
  
  return(y_hat)
}




### OCENA PEDYKCJI MODELU ###

MAE <- function( y_tar, y_hat ){
  return(mean( abs( y_tar - y_hat ) ) )
}

MSE <- function(y_tar, y_hat ){
  return(mean((y_tar- y_hat)^2))
}

MAPE <- function(y_tar, y_hat ){
  return(mean(abs((y_tar-y_hat)/y_tar)))
}

AUC <- function(y_tar, y_hat){
  krzywa_roc <- roc(y_tar, y_hat, quiet = TRUE)
  czulosc <- rev(krzywa_roc$sensitivities)
  specyficznosc <- rev(1 - krzywa_roc$specificities)
  czulosc_op <- c(diff(czulosc), 0)
  specyficznosc_op <- c(diff(specyficznosc), 0)
  wynik <- sum(czulosc * specyficznosc_op) + sum(czulosc_op * specyficznosc_op)/2
  return(round(wynik,4))
}

Youden <- function(a, b){
  roc_krzywa <- roc(a, b, quiet = TRUE)
  TPR <- roc_krzywa$sensitivities
  FPR <- roc_krzywa$specificities
  max_Y <- 0
  
  for (i in 1:length(TPR)) {
    Youden <- (TPR[i] + FPR[i]-1)
    if(Youden > max_Y){
      max_Y=Youden
    }
  }
  return(max_Y)
}

Czulosc <- function(Mat){
  return(round((Mat[4] / (Mat[4] + Mat[2])),4))
}

Specyficznosc <- function(Mat){
  return(round((Mat[1] / (Mat[1] + Mat[3])),4))
}

Jakosc <- function(Mat){
  return(round(((Mat[1] + Mat[4]) / (Mat[1] + Mat[2] + Mat[3] + Mat[4])),4))
}

Dokladnosc <- function(y_tar, y_hat){
  return(sum(as.numeric(y_tar) == as.numeric(y_hat)) / length(y_tar))
}

Model_Ocena <- function(y_tar, y_hat)
{
  if(is.numeric(y_tar)){
    regresja <- c("MAE" = MAE(y_tar, y_hat), "MSE" = MSE(y_tar, y_hat), "MAPE" = MAPE(y_tar, y_hat))
    return(regresja)
  }
  else if(is.factor(y_tar) & nlevels(y_tar) == 2){
    if(length(y_tar) == 1){
      dokladnosc_trafien <- Dokladnosc(y_tar, y_hat = ifelse(y_hat < 0.5, 0, 1))
      miary <- c("AUC" = 0, "Czulosc" = 0, "Specyficznosc" = 0, "Jakosc" = dokladnosc_trafien)
      return(miary)
    }
    else{
      Mat <- table(y_tar, y_hat = ifelse(y_hat <= Youden(y_tar, y_hat), 0, 1))
      miary <- c( "AUC" = AUC(y_tar, y_hat), "Czulosc" = Czulosc(Mat), "Specyficznosc" = Specyficznosc(Mat), "Jakosc" = Jakosc(Mat))
      return(miary)
    }
  }
  else if(is.factor(y_tar) & nlevels(y_tar) > 2){
    multi <- c("Dokladnosc" = Dokladnosc(y_tar, y_hat))
    return(multi)
  }
  else{
    message("Dane nie sa prawidlowe!")
  }
}



Kroswalidacja <- function(dane, X, Y, kFold, parTune, algorytm, seed = 123)
{
  set.seed(seed)
  
  if(is.numeric(dane[,Y])){
    typ = "reg"
  }
  else if(is.factor(dane[,Y])){
    if (nlevels(dane[,Y]) == 2){
      typ = "bin"
    }
    else if(nlevels(dane[,Y]) > 2){
      typ = "multi"
    }
  }
  
  liczba_wierszy = nrow(dane)
  podzial_zbioru <- data.frame(matrix(ncol = kFold, nrow = nrow(dane)))
  
  for(i in 1:kFold)
  { 
    id_trening <- sample( 1:liczba_wierszy, size = (1/kFold * liczba_wierszy)-1, replace = F )
    podzial_zbioru[id_trening,i] <- 2
    podzial_zbioru[-id_trening,i] <- 1
  }
  
  tabela_parametrow <- as.data.frame(expand_grid(nr_modelu=c(1:kFold), parTune))
  

  if(algorytm == "KNN")
  {
    cat("KNN model: ")
    
    if(typ == "bin")
    {
      tabela_parametrow_bin <- data.frame(tabela_parametrow, AUCT=0, CzuloscT=0, SpecyficznoscT=0, JakoscT=0, AUCW=0, CzuloscW=0, SpecyficznoscW=0, JakoscW=0)
      
      for(nr_parametru in 1:nrow(tabela_parametrow_bin))
      {
        cat(paste0(nr_parametru," -> "))
        trening <- dane[podzial_zbioru[,tabela_parametrow_bin$nr_modelu[nr_parametru]] == 1,]
        walidacja <- dane[podzial_zbioru[,tabela_parametrow_bin$nr_modelu[nr_parametru]] == 2,]
        
        KNN_Model <- KNN_train(trening[,X], trening[,Y], tabela_parametrow_bin$k[nr_parametru], 0, 1) 
        KNN_trening_p <- KNN_pred(KNN_Model, X=trening[,X])
        KNN_walidacja_p <- KNN_pred(KNN_Model, X=walidacja[,X])
        
        trening_ocena = Model_Ocena((trening[,Y]), as.numeric(1-unlist(KNN_trening_p %>% select('0'))))
        walidacja_ocena = Model_Ocena((walidacja[,Y]), as.numeric(1-unlist(KNN_walidacja_p %>% select('0'))))
        
        tabela_parametrow_bin[nr_parametru, "AUCT"] <- trening_ocena["AUC"]
        tabela_parametrow_bin[nr_parametru, "CzuloscT"] <- trening_ocena["Czulosc"]
        tabela_parametrow_bin[nr_parametru, "SpecyficznoscT"] <- trening_ocena["Specyficznosc"]
        tabela_parametrow_bin[nr_parametru, "JakoscT"] <- trening_ocena["Jakosc"]
        tabela_parametrow_bin[nr_parametru, "AUCW"] <- walidacja_ocena["AUC"]
        tabela_parametrow_bin[nr_parametru, "CzuloscW"] <- walidacja_ocena["Czulosc"]
        tabela_parametrow_bin[nr_parametru, "SpecyficznoscW"] <- walidacja_ocena["Specyficznosc"]
        tabela_parametrow_bin[nr_parametru, "JakoscW"] <- walidacja_ocena["Jakosc"]
      }
      return(tabela_parametrow_bin)
    }
    else if(typ == "multi")
    {
      tabela_parametrow_multi <- data.frame(tabela_parametrow, ACCT=0, ACCW=0)
      
      for(nr_parametru in 1:nrow(tabela_parametrow_multi))
      {
        cat(paste0(nr_parametru," -> "))
        trening <- dane[podzial_zbioru[,tabela_parametrow_multi$nr_modelu[nr_parametru]] == 1,]
        walidacja <- dane[podzial_zbioru[,tabela_parametrow_multi$nr_modelu[nr_parametru]] == 2,]
        
        KNN_Model <- KNN_train(trening[,X], trening[,Y], tabela_parametrow_multi$k[nr_parametru], 0, 1) 
        KNN_trening_p <- KNN_pred(KNN_Model, X=trening[,X])
        KNN_walidacja_p <- KNN_pred(KNN_Model, X=walidacja[,X])
        
        tabela_parametrow_multi[nr_parametru, "ACCT"] = Model_Ocena((trening[,Y]), factor(unlist(KNN_trening_p %>% select("class")), levels = levels(dane[,Y])))
        tabela_parametrow_multi[nr_parametru, "ACCW"] = Model_Ocena((walidacja[,Y]), factor(unlist(KNN_walidacja_p %>% select("class")), levels = levels(dane[,Y])))
      }
      return(tabela_parametrow_multi)
    }
    else if(typ == "reg")
    {
      tabela_parametrow_reg <- data.frame(tabela_parametrow, MAET=0, MSET=0, MAPET=0, MAEW=0, MSEW=0, MAPEW=0)
      
      for(nr_parametru in 1:nrow(tabela_parametrow_reg))
      {
        cat(paste0(nr_parametru," -> "))
        trening <- dane[podzial_zbioru[,tabela_parametrow_reg$nr_modelu[nr_parametru]] == 1,]
        walidacja <- dane[podzial_zbioru[,tabela_parametrow_reg$nr_modelu[nr_parametru]] == 2,]
        
        KNN_Model <- KNN_train(trening[,X], trening[,Y], tabela_parametrow_reg$k[nr_parametru], 0, 1) 
        KNN_trening_p <- KNN_pred(KNN_Model, X=trening[,X])
        KNN_walidacja_p <- KNN_pred(KNN_Model, X=walidacja[,X])
        
        trening_ocena <- Model_Ocena(trening[,Y], unlist(KNN_trening_p))
        walidacja_ocena <- Model_Ocena(walidacja[,Y], unlist(KNN_walidacja_p))
        
        tabela_parametrow_reg[nr_parametru, "MAET"] <- trening_ocena["MAE"]
        tabela_parametrow_reg[nr_parametru, "MSET"] <- trening_ocena["MSE"]
        tabela_parametrow_reg[nr_parametru, "MAPET"] <- trening_ocena["MAPE"]
        tabela_parametrow_reg[nr_parametru, "MAEW"] <- walidacja_ocena["MAE"]
        tabela_parametrow_reg[nr_parametru, "MSEW"] <- walidacja_ocena["MSE"]
        tabela_parametrow_reg[nr_parametru, "MAPEW"] <- walidacja_ocena["MAPE"]
      }
      return(tabela_parametrow_reg)
    }
  }
  
  if(algorytm == "Drzewa")
  {
    cat("Drzewa model: ")
    
    if(typ == "bin")
    {
      tabela_parametrow_bin <- data.frame(tabela_parametrow, AUCT=0, CzuloscT=0, SpecyficznoscT=0, JakoscT=0, AUCW=0, CzuloscW=0, SpecyficznoscW=0, JakoscW=0)
      
      for(nr_parametru in 1:nrow(tabela_parametrow_bin))
      {
        cat(paste0(nr_parametru," -> "))
        trening <- dane[podzial_zbioru[,tabela_parametrow_bin$nr_modelu[nr_parametru]] == 1,]
        walidacja <- dane[podzial_zbioru[,tabela_parametrow_bin$nr_modelu[nr_parametru]] == 2,]
        
        Tree_Model <- Tree(Y, X, trening, type = tabela_parametrow_bin$type[nr_parametru], depth =  tabela_parametrow_bin$depth[nr_parametru], minobs =  tabela_parametrow_bin$minobs[nr_parametru], overfit =  tabela_parametrow_bin$overfit[nr_parametru], cf = tabela_parametrow_bin$cf[nr_parametru]) 
        
        Tree_trening_p <- PredictTree(Tree_Model, trening[,X])
        Tree_walidacja_p <- PredictTree(Tree_Model, walidacja[,X])

        trening_ocena = Model_Ocena((trening[,Y]), as.numeric(Tree_trening_p[,2]))
        walidacja_ocena = Model_Ocena((walidacja[,Y]), as.numeric(Tree_walidacja_p[,2]))
        
        trening_ocena_acc = Dokladnosc((trening[,Y]), as.numeric(Tree_trening_p[,3]))
        walidacja_ocena_acc = Dokladnosc((walidacja[,Y]), as.numeric(Tree_walidacja_p[,3]))
        
        tabela_parametrow_bin[nr_parametru, "AUCT"] <- trening_ocena["AUC"]
        tabela_parametrow_bin[nr_parametru, "CzuloscT"] <- trening_ocena["Czulosc"]
        tabela_parametrow_bin[nr_parametru, "SpecyficznoscT"] <- trening_ocena["Specyficznosc"]
        tabela_parametrow_bin[nr_parametru, "JakoscT"] <- trening_ocena_acc
        tabela_parametrow_bin[nr_parametru, "AUCW"] <- walidacja_ocena["AUC"]
        tabela_parametrow_bin[nr_parametru, "CzuloscW"] <- walidacja_ocena["Czulosc"]
        tabela_parametrow_bin[nr_parametru, "SpecyficznoscW"] <- walidacja_ocena["Specyficznosc"]
        tabela_parametrow_bin[nr_parametru, "JakoscW"] <- walidacja_ocena_acc
      }
      return(tabela_parametrow_bin)
    }
    else if(typ == "multi")
    {
      tabela_parametrow_multi <- data.frame(tabela_parametrow, ACCT=0, ACCW=0)
      
      for(nr_parametru in 1:nrow(tabela_parametrow_multi))
      {
        cat(paste0(nr_parametru," -> "))
        trening <- dane[podzial_zbioru[,tabela_parametrow_multi$nr_modelu[nr_parametru]] == 1,]
        walidacja <- dane[podzial_zbioru[,tabela_parametrow_multi$nr_modelu[nr_parametru]] == 2,]
        
        Tree_Model <- Tree(Y, X, trening, type = tabela_parametrow_multi$type[nr_parametru], depth =  tabela_parametrow_multi$depth[nr_parametru], minobs =  tabela_parametrow_multi$minobs[nr_parametru], overfit =  tabela_parametrow_multi$overfit[nr_parametru], cf = tabela_parametrow_multi$cf[nr_parametru]) 
        Tree_trening_p <- PredictTree(Tree_Model, trening[,X])
        Tree_walidacja_p <- PredictTree(Tree_Model, walidacja[,X])
        
        tabela_parametrow_multi[nr_parametru, "ACCT"] <- Model_Ocena(trening[,Y], Tree_trening_p[,"Klasa"])
        tabela_parametrow_multi[nr_parametru, "ACCW"] <- Model_Ocena(walidacja[,Y], Tree_walidacja_p[,"Klasa"])
      }
      return(tabela_parametrow_multi)
    }
    else if(typ == "reg")
    {
      tabela_parametrow_reg <- data.frame(tabela_parametrow, MAET=0, MSET=0, MAPET=0, MAEW=0, MSEW=0, MAPEW=0)
      
      for(nr_parametru in 1:nrow(tabela_parametrow_reg))
      {
        cat(paste0(nr_parametru," -> "))
        trening <- dane[podzial_zbioru[,tabela_parametrow_reg$nr_modelu[nr_parametru]] == 1,]
        walidacja <- dane[podzial_zbioru[,tabela_parametrow_reg$nr_modelu[nr_parametru]] == 2,]
      
        Tree_Model <- Tree(Y, X, trening, type = tabela_parametrow_reg$type[nr_parametru], depth =  tabela_parametrow_reg$depth[nr_parametru], minobs =  tabela_parametrow_reg$minobs[nr_parametru], overfit =  tabela_parametrow_reg$overfit[nr_parametru], cf = tabela_parametrow_reg$cf[nr_parametru]) 
        Tree_trening_p <- PredictTree(Tree_Model, trening[,X])
        Tree_walidacja_p <- PredictTree(Tree_Model, walidacja[,X])
        
        trening_ocena <- Model_Ocena(trening[,Y], Tree_trening_p)
        walidacja_ocena <- Model_Ocena(walidacja[,Y], Tree_walidacja_p)
        
        tabela_parametrow_reg[nr_parametru, "MAET"] <- trening_ocena["MAE"]
        tabela_parametrow_reg[nr_parametru, "MSET"] <- trening_ocena["MSE"]
        tabela_parametrow_reg[nr_parametru, "MAPET"] <- trening_ocena["MAPE"]
        tabela_parametrow_reg[nr_parametru, "MAEW"] <- walidacja_ocena["MAE"]
        tabela_parametrow_reg[nr_parametru, "MSEW"] <- walidacja_ocena["MSE"]
        tabela_parametrow_reg[nr_parametru, "MAPEW"] <- walidacja_ocena["MAPE"]
      }
      return(tabela_parametrow_reg)
    }
  }
  
  if(algorytm == "Sieci")
  {
    cat("Sieci Neuronowe model: ")
    
    if(typ == "bin")
    {
      tabela_parametrow_bin <- data.frame(tabela_parametrow, AUCT=0, CzuloscT=0, SpecyficznoscT=0, JakoscT=0, AUCW=0, CzuloscW=0, SpecyficznoscW=0, JakoscW=0)
      
      for(nr_parametru in 1:nrow(tabela_parametrow_bin))
      {
        cat(paste0("\t ", nr_parametru," -> "))
        dane_bin_NN <- dane
        dane_bin_NN[,X] <- sapply(dane_bin_NN[,X], MinMax_NN)
        
        trening <- dane_bin_NN[podzial_zbioru[,tabela_parametrow_bin$nr_modelu[nr_parametru]] == 1,]
        trening_Y <- trening[,Y]
        trening[,Y] <- MinMax_NN(as.numeric(trening[,Y]))
        trening <- as.matrix(trening)
        
        walidacja <- dane_bin_NN[podzial_zbioru[,tabela_parametrow_bin$nr_modelu[nr_parametru]] == 2,]
        walidacja_Y <- walidacja[,Y]
        walidacja[,Y] <- MinMax_NN(as.numeric(walidacja[,Y]))
        walidacja <- as.matrix(walidacja)
        
        NN_Model <- Train_NN_Nowe( Y, X, data = trening, h = tabela_parametrow_bin$h[nr_parametru], lr = tabela_parametrow_bin$lr[nr_parametru], iter = tabela_parametrow_bin$iter[nr_parametru], seed = 357, typ = typ)
        NN_trening_p <- Pred_NN_Nowe(trening[,X], NN_Model, typ = typ)
        NN_walidacja_p <- Pred_NN_Nowe(walidacja[,X], NN_Model, typ = typ)

        trening_ocena = Model_Ocena(trening_Y, NN_trening_p[,1])
        walidacja_ocena = Model_Ocena(walidacja_Y, NN_walidacja_p[,1])
        
        tabela_parametrow_bin[nr_parametru, "AUCT"] <- trening_ocena["AUC"]
        tabela_parametrow_bin[nr_parametru, "CzuloscT"] <- trening_ocena["Czulosc"]
        tabela_parametrow_bin[nr_parametru, "SpecyficznoscT"] <- trening_ocena["Specyficznosc"]
        tabela_parametrow_bin[nr_parametru, "JakoscT"] <- trening_ocena["Jakosc"]
        tabela_parametrow_bin[nr_parametru, "AUCW"] <- walidacja_ocena["AUC"]
        tabela_parametrow_bin[nr_parametru, "CzuloscW"] <- walidacja_ocena["Czulosc"]
        tabela_parametrow_bin[nr_parametru, "SpecyficznoscW"] <- walidacja_ocena["Specyficznosc"]
        tabela_parametrow_bin[nr_parametru, "JakoscW"] <- walidacja_ocena["Jakosc"]
      }
      return(tabela_parametrow_bin)
    }
    else if(typ == "multi")
    {
      tabela_parametrow_multi <- data.frame(tabela_parametrow, ACCT=0, ACCW=0)
      
      for(nr_parametru in 1:nrow(tabela_parametrow_multi))
      {
        cat(paste0("\t ", nr_parametru," -> "))
        dane_multi_NN <- dane
        dane_multi_NN[,X] <- sapply(dane_multi_NN[,X], MinMax_NN)
        
        trening <- dane_multi_NN[podzial_zbioru[,tabela_parametrow_multi$nr_modelu[nr_parametru]] == 1,]
        trening_Y <- trening[,Y]
        trening_Y_NN <- model.matrix( ~ trening[,Y] - 1, trening)
        trening_Y_name <- colnames(trening_Y_NN)
        trening_data <- as.matrix(cbind(trening[,X], trening_Y_NN))
        
        walidacja <- dane_multi_NN[podzial_zbioru[,tabela_parametrow_multi$nr_modelu[nr_parametru]] == 2,]
        
        NN_Model <- Train_NN_Nowe( trening_Y_name, X, data = trening_data, h = tabela_parametrow_multi$h[nr_parametru], lr = tabela_parametrow_multi$lr[nr_parametru], iter = tabela_parametrow_multi$iter[nr_parametru], seed = 123, typ = typ)
        
        NN_trening_p <- Pred_NN_Nowe(trening_data[,X], NN_Model, typ = typ)
        NN_walidacja_p <- Pred_NN_Nowe(as.matrix(walidacja[,X]), NN_Model, typ = typ)

        NN_trening_p_multi <- factor(levels(dane_multi_NN[,Y])[apply( NN_trening_p, 1, which.max )] , levels = levels(dane_multi_NN[,Y]))
        NN_walidacja_p_multi <- factor(levels(dane_multi_NN[,Y])[apply( NN_walidacja_p, 1, which.max )], levels = levels(dane_multi_NN[,Y]))

        tabela_parametrow_multi[nr_parametru, "ACCT"] <- Model_Ocena((trening[,Y]), (NN_trening_p_multi))
        tabela_parametrow_multi[nr_parametru, "ACCW"] <- Model_Ocena((walidacja[,Y]), (NN_walidacja_p_multi))
      }
      
      return(tabela_parametrow_multi)
    }
    else if(typ == "reg")
    {
      tabela_parametrow_reg <- data.frame(tabela_parametrow, MAET=0, MSET=0, MAPET=0, MAEW=0, MSEW=0, MAPEW=0)
      
      for(nr_parametru in 1:nrow(tabela_parametrow_reg))
      {
        cat(paste0("\t ", nr_parametru," -> "))
        dane_reg_NN <- sapply(dane, MinMax_NN)
  
        trening <- as.matrix(dane_reg_NN[podzial_zbioru[,tabela_parametrow_reg$nr_modelu[nr_parametru]] == 1,])
        walidacja <- as.matrix(dane_reg_NN[podzial_zbioru[,tabela_parametrow_reg$nr_modelu[nr_parametru]] == 2,])
        
        NN_Model <- Train_NN_Nowe(Y, X, data = trening, h = tabela_parametrow_reg$h[nr_parametru], lr = tabela_parametrow_reg$lr[nr_parametru], iter = tabela_parametrow_reg$iter[nr_parametru], seed = 123, typ = typ)

        NN_trening_p <- Pred_NN_Nowe(trening[,X], NN_Model, typ = typ)
        NN_walidacja_p <- Pred_NN_Nowe(walidacja[,X], NN_Model, typ = typ)

        NN_trening_p_reg <- MinMaxOdwrot(NN_trening_p[,1], y_min = min(dane[,Y]), y_max = max(dane[,Y]))
        NN_walidacja_p_reg <- MinMaxOdwrot(NN_walidacja_p[,1], y_min = min(dane[,Y]), y_max = max(dane[,Y]))
        
        trening_ocena <- Model_Ocena(dane[podzial_zbioru[,tabela_parametrow_reg$nr_modelu[nr_parametru]] == 1,Y], NN_trening_p_reg)
        walidacja_ocena <- Model_Ocena(dane[podzial_zbioru[,tabela_parametrow_reg$nr_modelu[nr_parametru]] == 2,Y], NN_walidacja_p_reg)
        
        tabela_parametrow_reg[nr_parametru, "MAET"] <- trening_ocena["MAE"]
        tabela_parametrow_reg[nr_parametru, "MSET"] <- trening_ocena["MSE"]
        tabela_parametrow_reg[nr_parametru, "MAPET"] <- trening_ocena["MAPE"]
        tabela_parametrow_reg[nr_parametru, "MAEW"] <- walidacja_ocena["MAE"]
        tabela_parametrow_reg[nr_parametru, "MSEW"] <- walidacja_ocena["MSE"]
        tabela_parametrow_reg[nr_parametru, "MAPEW"] <- walidacja_ocena["MAPE"]
      }
      return(tabela_parametrow_reg)
    }
  }
}


