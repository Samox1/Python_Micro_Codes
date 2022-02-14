library(tidyverse)
library(pROC)
library(ggplot2)
library(StatMatch)
library(dplyr)
library(data.tree)
library(caret)
library(microbenchmark)
library(rpart)
library(rpart.plot)
library(caTools)
library(nnet)
library(parallel)
library(biglm)
library(bigmemory)
library(pryr)
library("doParallel")
library("doSNOW")



MinMax <- function( x, new_min = 0, new_max = 1 ){
  return( ( ( x - min(x) ) / ( max(x) - min(x) ) ) * ( new_max - new_min ) + new_min )
}


KNNtrain <- function(X, y_tar, k, XminNew, XmaxNew) 
{
  if (any(is.na(X) == TRUE) || any(is.na(y_tar) == TRUE)) 
  {
    stop("Niekompletne dane!")
  }
  else if(k <= 0)
  {
    stop("Za male k")
  }
  else if(XminNew == XmaxNew)
  {
    stop("Nowe granice sa takie same!")
  }
  else if((is.matrix(X) == FALSE & is.data.frame(X) == FALSE))
  {
    stop("Dane nie sa macierza lub tablica danych!")
  }
  else
  {
    X <- data.frame(X)
    X_norm <- data.frame(X)
    X_norm[,] <- 0
    
    nazwa <- vector()
    minOrg <- vector()
    maxOrg <- vector()
    minmaxNew <- c(XminNew, XmaxNew)
    
    for(i in 1:ncol(X)) 
    {
      nazwa <- append(nazwa, i)
      
      if (is.numeric(X[,i])) 
      {
        minOrg <- append(minOrg, min(X[,i]))
        maxOrg <- append(maxOrg, max(X[,i]))
        
        X_norm[,i] <- MinMax(X[,i], XminNew, XmaxNew)
      }
      else if(is.factor(X[,i]) & is.ordered(X[,i]) | is.factor(X[,i]))
      {
        minOrg <- append(minOrg, NA)
        maxOrg <- append(maxOrg, NA)
        
        X_norm[,i] <- X[,i]
      }
      else
      {
        message(paste0("Wartosci niepoprawne w X, kolumna: ", name))
      }
    }
    
    names(maxOrg) <- nazwa
    names(minOrg) <- nazwa
    
    attr(X_norm, 'minOrg') <- minOrg
    attr(X_norm, 'maxOrg') <- maxOrg
    attr(X_norm, 'minmaxNew') <- minmaxNew
    
    knn <- list()
    knn[["X"]] <- X_norm
    knn[["y"]] <- y_tar
    knn[["k"]] <- k
    
    return(knn)
  }
}


d_euklides <- function(x_i, x_n)
{
  return(sqrt(sum((x_i - x_n)^2)))
}

d_hamming <- function(x_i, x_n, p)
{
  return((sum(x_i != x_n)) / p)
}

d_porzadkowa <- function(x_i, x_n, unikalne)
{
  return(sum(abs(as.numeric(x_i) - as.numeric(x_n))  / (unikalne - 1)))
}



KNNpred <- function(KNNmodel, X) 
{
  if (any(is.na(KNNmodel) == TRUE) || any(is.na(X) == TRUE)) 
  {
    stop("Niekompletne dane!")
  }
  else if((is.matrix(X) == FALSE & is.data.frame(X) == FALSE))
  {
    stop("Dane testowe nie sa typu MATRIX czy DATA.FRAME!")
  }
  else if(ncol(KNNmodel$X) != ncol(X) || colnames(KNNmodel$X) != colnames(X)) 
  {
    stop("Dane uczace z modelu nie zgadzaja sie z danymi wejsciowymi!")
  }
  else
  {
    if(!is.data.frame(X))
    {
      X <- data.frame(X)
    }
    
    X_znormalizowane <- matrix(0,nrow(X), ncol(X))
    X_znormalizowane <- data.frame(X_znormalizowane)
    
    n_wierszy_model = nrow(KNNmodel$X)
    n_kolumn_model = ncol(KNNmodel$X)
    
    n_wierszy_znorm = nrow(X_znormalizowane)
    n_kolumn_znorm = ncol(X_znormalizowane)
    
    kolumny_numeryczne <- 0
    kolumny_factor_order <- 0
    kolumny_factor <- 0
    
    for (i in 1:n_kolumn_znorm) 
    {
      if(is.numeric(X[,i]))
      {
        min_k <- as.numeric(attributes(KNNmodel$X)$minOrg[i])
        max_k <- as.numeric(attributes(KNNmodel$X)$maxOrg[i])
        newmin_k <- as.numeric(attributes(KNNmodel$X)$minmaxNew[1])
        newmax_k <- as.numeric(attributes(KNNmodel$X)$minmaxNew[2])
        
        X_znormalizowane[,i] <- ((X[,i] - min_k) / (max_k - min_k)) * (newmax_k - newmin_k) + newmin_k
        kolumny_numeryczne <- kolumny_numeryczne + 1
      }
      else if(is.factor(X[,i]))
      {
        X_znormalizowane[,i] <- X[,i]
        kolumny_factor <- kolumny_factor + 1
      }
      else if(is.factor(X[,i]) & is.ordered(X[,i]))
      {
        X_znormalizowane[,i] <- X[,i]
        kolumny_factor_order <- kolumny_factor_order + 1
      }
    }
    
    
    odleglosc <- matrix(0, n_wierszy_model, n_wierszy_znorm)
    
    if(kolumny_numeryczne == n_kolumn_znorm)
    {
      for(i in 1:n_wierszy_model)
      {
        for(j in 1:n_wierszy_znorm)
        {
          odleglosc[i,j] <- d_euklides(KNNmodel$X[i,], X_znormalizowane[j,])
        }
      }
    }
    else if(kolumny_factor == n_kolumn_znorm)
    {
      for(i in 1:n_wierszy_model)
      {
        for(j in 1:n_wierszy_znorm)
        {
          odleglosc[i,j] <- d_hamming(KNNmodel$X[i,], X_znormalizowane[j,], n_kolumn_znorm)
        }
      }
    }
    else if(kolumny_factor_order == n_kolumn_znorm)
    {
      for(i in 1:n_wierszy_model)
      {
        for(j in 1:n_wierszy_znorm)
        {
          for(k in 1:n_kolumn_znorm)
          {
            unikalne <- nlevels(X_znormalizowane[,k])
            odleglosc[i,j] <- d_porzadkowa(KNNmodel$X[i,], X_znormalizowane[j,], unikalne)
          }
        }
      }
    }
    else
    {
      for(i in 1:n_wierszy_model)
      {
        for(j in 1:n_wierszy_znorm)
        {
          temp <- 0
          
          for(k in 1:n_kolumn_znorm)
          {
            if(is.numeric(X_znormalizowane[,k]))
            {
              max_k <- as.numeric(attributes(KNNmodel$X)$maxOrg[k])
              min_k <- as.numeric(attributes(KNNmodel$X)$minOrg[k])
              
              temp <- temp + (abs(KNNmodel$X[i,k] - X_znormalizowane[j,k]) / (max_k -  min_k)) 
            }
            else if(is.factor(X_znormalizowane[,k]))
            {
              if(KNNmodel$X[i,k] != X_znormalizowane[j,k])
              {
                temp <- temp + 1
              }
            }
            else if(is.factor(X_znormalizowane[,k]) & is.ordered(X_znormalizowane[,k]))
            {
              z_i <- (i - 1) / (n_wierszy_model - 1)
              z_n <- (j - 1) / (n_wierszy_znorm - 1)
              temp <- temp + (abs(z_i - z_n) / (n_wierszy_model - 1))
            }
          }
          odleglosc[i, j] <- temp / n_kolumn_znorm
        }
      }
    }
    
    
    if(is.numeric(KNNmodel$y))
    {
      predykcja <- double(n_kolumn_znorm)
      
      for(i in 1:n_wierszy_znorm)
      {
        k_najblizej <- order(odleglosc[,i])[1:KNNmodel$k]
        
        y_predykcja <- mean(KNNmodel$y[k_najblizej])
        
        predykcja[i] <- y_predykcja
      }
      return(predykcja)
    }
    else if(is.factor(KNNmodel$y))
    {
      predykcja <- as.data.frame(matrix(nrow = n_wierszy_znorm, ncol = nlevels(KNNmodel$y)+1))
      
      for(i in 1:n_wierszy_znorm)
      {
        k_najblizej <- order(odleglosc[,i])[1:KNNmodel$k]
        
        if(nlevels(KNNmodel$y) == 2)
        {
          pozytywna <- sum(as.numeric(KNNmodel$y[k_najblizej]) == 1) / KNNmodel$k
          negatywna <- sum(as.numeric(KNNmodel$y[k_najblizej]) == 0) / KNNmodel$k
          
          predykcja_klasy <- ifelse(pozytywna >= 0.5, 'P', 'N')
          
          names(predykcja) <- c('P', 'N', 'Klasa')
          predykcja[i, 1] <- pozytywna
          predykcja[i, 2] <- negatywna
          predykcja[i, 3] <- predykcja_klasy
        }
        else if(nlevels(KNNmodel$y) > 2)
        {
          etykiety <- sort(unique(KNNmodel$y))
          names(predykcja) <- etykiety
          names(predykcja)[nlevels(KNNmodel$y)+1] <- 'Klasa'
          
          for (j in 1:length(etykiety))
          {
            pozytywna <- sum(KNNmodel$y[k_najblizej] == as.character(etykiety[j])) / KNNmodel$k
            predykcja[i,j] <- pozytywna
          }
          
          predykcja_klasy <- etykiety[which.max(predykcja[i,])]
          predykcja[i,'Klasa'] <- as.factor(predykcja_klasy)
        }
      }
      return(predykcja)
    }
    else
    {
      stop("Dane y modelu sa niepoprawne!")
    }
  }
}




StopIfNot <- function( Y, X, data, type, depth, minobs, overfit, cf){
  
  if (!is.data.frame(data)){
    print("Typem parametru data nie jest tablica danych.")
    return(FALSE)}
  
  if(!all(c(X) %in% names(data))){
    print("Nie wszystkie zmienne X istnieja w data.")
    return(FALSE)}
  
  if(!all(c(Y) %in% names(data))){
    print("Zmienna Y nie istnieja w data.")
    return(FALSE)}
  
  if(any(is.na(data[,X]) == TRUE)){
    print("Wystepuja braki danych w zmiennych X.")
    return(FALSE)}
  
  if(any(is.na(data[,Y]) == TRUE)){
    print("Występują braki danych w zmiennej Y.")
    return(FALSE)}
  
  if(depth  == 0 || depth  < 0){
    print("depth nie jest wieksze od 0")
    return(FALSE)}
  
  if(minobs  == 0 || minobs < 0){
    print("minobs nie jest wieksze od 0")
    return(FALSE)}
  
  if(type != 'Gini' && type != 'Entropy' && type != 'SS'){
    print("Niepoprawna wartosc typu!")
    return(FALSE)}
  
  if(overfit != 'none' && overfit != 'prune' ){
    print("Niepoprawna wartosc overfit!")
    return(FALSE)}
  
  if(cf <= 0 || cf > 0.5 ){
    print("Wartosc cf nie jest z przedzialu (0, 0.5]!")
    return(FALSE)}
  
  if((is.factor(data[,Y])== TRUE && type=="SS") || 
     (is.numeric(data[,Y])==TRUE &&( type == "Entropy" || type == "Gini"))){
    print("Wybrana kombinacja parametrow type i data nie jest mozliwa.")
    return(FALSE)}
  
  return(TRUE)
}

Prob <- function( y_tar ){
  res <- unname( table( y_tar ) )
  res <- res / sum( res )
  return(res)
}

Entropy <- function( prob ){
  
  res <- prob * log2( prob )
  res[ prob == 0 ] <- 0
  res <- -sum( res )
  return( res )
}

Gini <- function(prob){
  
  res <- prob^2
  res <- 1-sum(res)
  return(res)
}

SS <- function(y){
  
  res <- (y-mean(y))^2
  res <- sum(res)
  return(res)
}


AssignInitialMeasures <- function(tree,Y,data,type,depth) {
  tree$Depth <- 0
  
  if (type == "Entropy") {
    tree$inf <- Entropy(Prob(data[,Y]))
  }
  else if (type == "Gini") {
    tree$inf <- Gini(Prob(data[,Y]))
  }
  else if (type == "SS") {
    tree$inf <- SS(data[,Y])
  }
  else {
    stop("Nieprawidlowy typ!")
  }
  return (tree)
}

AssignInfo <- function( tree, Y, X, data, type, depth, minobs, overfit, cf){
  
  attr(tree, 'Y') <- Y
  attr(tree, 'X') <- X
  attr(tree, 'data') <- data
  attr(tree, 'type') <- type
  attr(tree, 'depth') <- depth
  attr(tree, 'minobs') <- minobs
  attr(tree, 'overfit') <- overfit
  attr(tree, 'cf') <- cf
  
  return(tree)
}

SpliNum <- function( Y, x, parentVal, splits, minobs, type){
  
  n <- length( x )
  res <- data.frame( matrix( 0, length(splits), 6 ) )
  colnames( res ) <- c("InfGain","lVal","rVal","point","ln","rn")
  
  for( i in 1:length(splits) ){
    
    if(is.numeric(x)){
      partition <- x <= splits[i] 
    }
    else{
      partition <- x %in% splits[1:i]
    }
    
    ln <- sum( partition )
    rn <- n - ln
    
    if( any((c(ln,rn) < minobs| any(is.na(c(ln,rn)))))){ 
      
      res[i,] <- 0
      
    }else{
      
      lVal <- if (type=="Gini") {
        Gini(Prob(Y[partition]))
      } else if (type=="Entropy") {
        Entropy(Prob(Y[partition]))
      } else {
        SS(Y[partition])
      }   
      
      rVal <- if (type=="Gini") {
        Gini(Prob(Y[!partition]))
      } else if (type=="Entropy") {
        Entropy(Prob(Y[!partition]))
      } else {
        SS(Y[!partition])
      } 
      
      InfGain <- parentVal - ( lVal * ln/n  + rVal * rn/n )
      
      res[i,"InfGain"] <- InfGain
      res[i,"lVal"] <- lVal
      res[i,"rVal"] <- rVal
      if (is.numeric(x)){
        res[i,"point"] <- splits[i]
      }
      else{
        res[i,"point"] <- as.character(paste(splits[i]))
      }
      res[i,"ln"] <- ln
      res[i,"rn"] <- rn
    }
  }
  return( res )
}


SplitVar <- function( Y, x, parentVal, minobs, type ){
  
  s <- unique( x )
  if( length(x) == 1 ){
    splits <- s
  }else{
    splits <- head( sort( s ), -1 )
  }
  
  res <- SpliNum( Y, x, parentVal, splits, minobs, type)
  
  incl <- res$ln >= minobs & res$rn >= minobs & res$InfGain > 0
  res <- res[ incl, , drop = F ]
  
  best <- which.max( res$InfGain )
  
  res <- res[ best, , drop = F ]
  
  return( res )
  
}

FindBestSplit <- function( Y, X, data, parentVal, type, minobs ){
  res <- sapply( X, function(i){
    
    if (is.numeric(data[,i])) { 
      d <- data[,i]
    }
    else if(is.ordered(data[,i])){
      d <- as.numeric(paste(data[,i]))
    }
    else{
      if(is.factor(data[,Y])){
        temp <- data[data[,Y] == 1,]
        z <- prop.table(table(temp[,i]))
        z <- sort(z)
        d <- (paste(factor(data[,i], levels = names(z), ordered = TRUE)))
      }
      else {
        z <- tapply(data[,Y], data[,i], mean)
        z <- sort(z)
        d <- (paste(factor(data[,i], levels = names(z), ordered = TRUE)))
      }
    }
    SplitVar( data[,Y], d, parentVal, minobs, type )
  }, simplify = F )
  
  res <- do.call( "rbind", res )
  
  best <- which.max( res$InfGain )
  res <- res[ best, , drop = F ]
  
  return( res )
}


BuildTree <- function(node, Y, X, data, depth, type , minobs){
  
  node$Count <- nrow( data )
 
  if (is.factor(data[,Y])) {
    node$Prob <- Prob(data[,Y])
    node$Class <- levels(data[,Y])[which.max(node$Prob)]
  }
  else {
    node$Prob <- SS(data[,Y])
    node$Class <- mean(data[,Y])
  }
  
  bestSplit <- FindBestSplit(Y, X, data, node$inf, type, minobs) 
  
  ifStop <- nrow(bestSplit) == 0
  
  if( ifStop| node$Depth == depth | all( node$Prob %in% c(0,1) )){
    
    node$Leaf <- "*"
    return( node )
    
  }else{
    
    split_indx <- data[,rownames(bestSplit)] <= bestSplit$point
    child_frame <- split( data, split_indx )
    
    name_l <- sprintf( "%s <= %s", rownames(bestSplit), bestSplit$point ) 
    child_l <- node$AddChild( name_l )
    child_l$value <- split_indx
    child_l$Depth <- node$Depth + 1
    child_l$inf <- bestSplit$lVal
    child_l$feature <- rownames(bestSplit)
    child_l$BestSplit <- bestSplit$point
    
    BuildTree( child_l, Y, X, child_frame[["TRUE"]], depth, type, minobs)
    
    
    name_r <- sprintf( "%s >  %s", rownames(bestSplit), bestSplit$point )
    child_r <- node$AddChild( name_r )
    child_r$value <- split_indx
    child_r$Depth <- node$Depth + 1
    child_r$inf <- bestSplit$rVal
    child_r$feature <- rownames(bestSplit)
    child_r$BestSplit <- bestSplit$point
    
    BuildTree( child_r, Y, X, child_frame[["FALSE"]], depth, type, minobs ) 
    
  }
}


PE <- function( p, n, z ){
  return( ( p + (z^2)/(2*n) + z*sqrt( p/n - (p^2)/(n) + (z^2)/(4*n^2) ) ) / ( 1 + z^2/n ) )
}

PruneTree <- function( tree, cf){
  errcf <- qnorm( 1 - cf )

  if( length( tree$Get("pathString") ) == 1 ) return( NULL ) 

  liscie_byly <- c()
  
  repeat{
    sciezka_lisci <- tree$Get("pathString", filterFun = isLeaf )
    
    if( all( sciezka_lisci %in% liscie_byly) | sciezka_lisci[1] == "Root" ) break
    
    temp <- strsplit( sciezka_lisci[ !sciezka_lisci %in% liscie_byly ][1], "/")[[1]]
    leaf <- eval( parse( text = paste( "tree", paste0( paste0( "'", temp[-1] ), "'", collapse = "$" ), sep = "$" ) ) )
    
    parent <- leaf$parent
    sibling <- leaf$siblings[[1]]
    
    leaf_class <- leaf$Class
    sibling_class <- sibling$Class
    parent_class <- parent$Class
    
    leaf_prob <- leaf$Prob
    sibling_prob <- sibling$Prob
    parent_prob <- parent$Prob
    
    leaf_count <- leaf$Count
    sibling_count <- sibling$Count
    parent_count <- parent$Count
    
    leaf_isLeaf <- leaf$isLeaf
    sibling_isLeaf <- sibling$isLeaf
    
    leaf_PE <- PE( 1 - max(leaf_prob), leaf_count, errcf )
    sibling_PE <- PE( 1 - max(sibling_prob), sibling_count, errcf )
    parent_PE <- PE( 1 - max(parent_prob), parent_count, errcf )
    
    if_prune <- parent_PE <= leaf_count / parent_count * leaf_PE + sibling_count / parent_count * sibling_PE
    
    if( if_prune & leaf_isLeaf == sibling_isLeaf ){
      parent$RemoveChild( sibling$name )
      parent$RemoveChild( leaf$name )
      parent$Leaf <- "*"
    }else{
      liscie_byly <- c( liscie_byly, leaf$pathString )
    }
  }
}


Tree <- function(Y, X, data, type, depth, minobs, overfit, cf){
  
  if(StopIfNot(Y, X, data, type, depth, minobs, overfit, cf) == FALSE){
    return(FALSE)
  }

  tree <- data.tree::Node$new("Root")
  tree$Count <- nrow(data)
  
  # Dodanie do 'Root' potrzebnych informacji
  tree <- AssignInitialMeasures(tree, Y, data, type, depth)
  
  # Budowanie Drzewa
  BuildTree(node = tree, Y = Y, X = X, data = data, depth = depth, type = type, minobs = minobs)
  
  # Przyciecie drzewa - wykorzystanie parametru 'overfit' i 'cf'
  if(overfit == 'prune')
  {
    PruneTree(tree, cf)
  }
  
  # Dodatnie info o drzewie decyzyjnym jako atrybuty
  AssignInfo(tree,Y,X,data,type,depth, minobs, overfit, cf)
  
  return(tree)
}


Predykcja_Drzewa <- function(tree, obs) {
  if (tree$isLeaf) {
    return(c(as.numeric(tree$Prob), "Class" = tree$Class))
  }
  
  if (is.numeric(tree$children[[1]]$BestSplit) | is.ordered(tree$children[[1]]$BestSplit)) {
    child <- tree$children[[ifelse(obs[,tree$children[[1]]$feature] > (tree$children[[1]]$BestSplit), 2, 1)]]
  }
  else {
    split <- tree$children[[1]]$feature
    child <- tree$children[[ifelse((obs[,tree$children[[1]]$feature] %in% split), 1, 2)]]
  }
  return (Predykcja_Drzewa(child,obs))
}


PredictTree <- function(tree, data) {
  
  if (is.factor(attributes(tree)$data[,attributes(tree)$Y])) {
    res <- c()
    
    for (i in 1:nrow(data)) {
      res <- rbind(res, (Predykcja_Drzewa(tree, data[i, ,drop = F])))
    }
  }
  else{
    res <- c()
    for (i in 1:nrow(data)) {
      res[i] <- as.numeric(Predykcja_Drzewa(tree, data[i, ,drop = F])["Class"])}
  }
  return (res)
}



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
MinMaxOdwrot <- function( x, y_min, y_max ){
  return(  x * (y_max - y_min) + y_min )
} 


wprzod <- function( X, W1, W2, W3, typ ){
  Z1 <- cbind( matrix( 1, nrow = nrow(X) ), sigmoid( X %*% W1 )  )
  Z2 <- cbind( matrix( 1, nrow = nrow(X) ), sigmoid( Z1 %*% W2 )  )
  if (typ =="bin"){
    y_hat <- sigmoid( Z2 %*% W3 )                                           # klasyfikacja binarna
  }
  else if (typ == "multi"){
    y_hat <- matrix( t( apply( Z2 %*% W3, 1, SoftMax ) ), nrow = nrow(X) )  # klasyfikacja wieloklasowa
  } 
  else if (typ == "reg"){
    y_hat <- Z2 %*% W3                                                      # regresja
  }
  return( list( y_hat = y_hat, Z1 = Z1, Z2 = Z2 ) )
}


wstecz <- function( X, y_tar, y_hat, W1, W2, W3, Z1, Z2, lr, typ ){
  if (typ =="bin"){
    dy_hat <- (y_tar - y_hat) * dsigmoid( y_hat )                   # klasyfikacja binarna
  }
  else if (typ == "multi"){
    dy_hat <- (y_tar - y_hat) / nrow( X )                           # klasyfikacja wieloklasowa
  } 
  else if (typ == "reg"){
    dy_hat <- (y_tar - y_hat)                                       # regresja
  }
  dW3 <- t(Z2) %*% dy_hat
  dZ2<- dy_hat %*% t(W3) * dsigmoid( Z2 )
  dW2 <- t(Z1) %*% dZ2[,-1]
  dZ1<- dZ2[,-1] %*% t(W2) * dsigmoid( Z1 )
  dW1 <- t(X) %*% dZ1[,-1]
  W1 <- W1 + lr * dW1
  W2 <- W2 + lr * dW2
  W3 <- W3 + lr * dW3
  return( list( W1 = W1, W2 = W2, W3 = W3 ) )
}

trainNN <- function( x, y_tar, h = c(5,5), lr = 0.01, iter = 10000, seed = 399, typ = "bin" ){
  set.seed( seed )
  # Error_NN <- double( iter )
  
  X <- cbind( rep( 1, nrow(x) ), x )
  h = unlist(h, use.names = FALSE)
  W1 <- matrix( runif( ncol(X) * h[1], -1, 1 ), nrow = ncol(X) )
  W2 <- matrix( runif( (h[1]+1) * h[2], -1, 1 ), nrow = h[1] + 1 )
  W3 <- matrix( runif( (h[2]+1) * ncol(y_tar), -1, 1 ), nrow = h[2] + 1 )
  
  for( i in 1:iter ){
    sygnalwprzod <- wprzod( X, W1, W2, W3, typ=typ )
    sygnalwtyl <- wstecz( X, y_tar, y_hat = sygnalwprzod$y_hat, W1, W2, W3, Z1 = sygnalwprzod$Z1, Z2 = sygnalwprzod$Z2, lr, typ=typ )
    W1 <- sygnalwtyl$W1
    W2 <- sygnalwtyl$W2
    W3 <- sygnalwtyl$W3
    cat( paste0( "\rIteracja: ", i , " / ", iter) )
    
    # Error_NN[i] <- lossSS( y_tar, sygnalwprzod$y_hat )
  }
  # xwartosci <- seq( 1, iter, length = 1000 )
  # print( qplot( xwartosci, Error_NN[xwartosci], geom = "line", main = "Error", xlab = "Iteracje" ) )
  return( list( y_hat = sygnalwprzod$y_hat, W1 = W1, W2 = W2, W3 = W3 ) )
}

predNN <- function( xnew, nn, typ = "bin" ){
  xnew <- cbind( rep( 1, nrow(xnew) ), xnew )
  Z1 <- cbind( matrix( 1, nrow = nrow(xnew) ), sigmoid( xnew %*% nn$W1 )  )
  Z2 <- cbind( matrix( 1, nrow = nrow(xnew) ), sigmoid( Z1 %*% nn$W2 )  )
  
  if (typ =="bin"){
    y_hat <- sigmoid( Z2 %*% nn$W3 )                                              # klasyfikacja binarna
  }
  else if (typ == "multi"){
    y_hat <- matrix( t( apply( Z2 %*% nn$W3, 1, SoftMax ) ), nrow = nrow(xnew) )  # klasyfikacja wieloklasowa
  } 
  else if (typ == "reg"){
    y_hat <- Z2 %*% nn$W3                                                         # regresja
  }
  
  return( y_hat )
}





MAE <- function( y_tar, y_hat ){
  return( mean( abs( y_tar - y_hat ) ) )
}

MSE <- function(y_tar, y_hat ){
  return(mean((y_tar- y_hat)^2))
}

MAPE <- function(y_tar, y_hat ){
  return(mean(abs((y_tar-y_hat)/y_tar)))
}


AUC <- function(y_tar, y_hat){
  krzywa_roc <- pROC::roc(y_tar, y_hat, quiet = TRUE)
  czulosc <- rev(krzywa_roc$sensitivities)
  specyficznosc <- rev(1 - krzywa_roc$specificities)
  czulosc_op <- c(diff(czulosc), 0)
  specyficznosc_op <- c(diff(specyficznosc), 0)
  wynik <- sum(czulosc * specyficznosc_op) + sum(czulosc_op * specyficznosc_op)/2
  return(round(wynik,4))
}


Youden <- function(a, b){
  roc_krzywa <- pROC::roc(a, b, quiet = TRUE)
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


Czulosc <- function(Mat)
{
  return(round((Mat[4] / (Mat[4] + Mat[2])),4))
}

Specyficznosc <- function(Mat)
{
  return(round((Mat[1] / (Mat[1] + Mat[3])),4))
}

Jakosc <- function(Mat)
{
  return(round(((Mat[1] + Mat[4]) / (Mat[1] + Mat[2] + Mat[3] + Mat[4])),4))
}


Jakosc__ <- function(y_tar, y_hat){
  return( sum(as.numeric(y_tar) == as.numeric(y_hat)) / length(y_tar) )
}


ModelOcena <- function(y_tar, y_hat)
{
  
  if(is.numeric(y_tar)){
    regresja <- c("MAE" = MAE(y_tar, y_hat), "MSE" = MSE(y_tar, y_hat), "MAPE" = MAPE(y_tar, y_hat))
    return(regresja)
  }
  else if(is.factor(y_tar) & nlevels(y_tar) == 2){
    if(length(y_tar) == 1){
      Acc <- Jakosc__(y_tar, y_hat = ifelse(y_hat < 0.5, 0, 1))
      miary <- c("AUC" = 0, "Czulosc" = 0, "Specyficznosc" = 0, "Jakosc" = Acc)
      return(miary)
    }else{
      Mat <- table(y_tar, y_hat = ifelse(y_hat <= Youden(y_tar, y_hat), 0, 1))
      miary <- c( "AUC" = AUC(y_tar, y_hat), "Czulosc" = Czulosc(Mat), "Specyficznosc" = Specyficznosc(Mat), "Jakosc" = Jakosc(Mat))
      return(miary)
    }
    
  }else if(is.factor(y_tar) & nlevels(y_tar) > 2){
    multi <- c("Acc" = Jakosc__(y_tar, y_hat))
    return(multi)
  }else{
    print("Niepoprawne dane")
  }
}

CrossValidTune <- function(dane, X, Y, kFold = 9, parTune = expand.grid(k = c(5)), algorytm = 'KNN', seed = 399)
{
  set.seed(seed)

  if(is.numeric(dane[,Y])){
    typ = "reg"
  }else if(is.factor(dane[,Y])){
    if (nlevels(dane[,Y]) == 2){
      typ = "bin"
    }else if(nlevels(dane[,Y]) > 2){
      typ = "multi"
    }
  }
  
  n = nrow(dane)
  trening_walidacja <- data.frame(matrix(ncol = kFold, nrow = nrow(dane)))
  macierz_parametrow <- as.data.frame(expand_grid(k_=c(1:kFold), parTune))

  for(i in 1:kFold){ 
    id_trening <- sample( 1:n, size = (1/kFold * n)-1, replace = F )
    trening_walidacja[id_trening,i] <- 2
    trening_walidacja[-id_trening,i] <- 1
  }
  

  nCores <- detectCores()
  klaster <- makeCluster(nCores-1)
  registerDoParallel(klaster)
  
  
  if(algorytm == "KNN")
  {
    cat('\n')
    
    if(typ == "bin")
    {
      cat("Obliczenia dla KNN - problem klasyfikacji binarnej: ")
      # cat(paste0("Liczba modeli x kroswalidacja = ", nrow(macierz_parametrow) * kFold))
      
      macierz_parametrow_bin <- data.frame(macierz_parametrow, AUCT=0, CzuloscT=0, SpecyficznoscT=0, JakoscT=0, AUCW=0, CzuloscW=0, SpecyficznoscW=0, JakoscW=0)
      
      clusterExport(klaster, "X", envir = environment())
      clusterExport(klaster, "Y", envir = environment())
      clusterExport(klaster, "dane", envir = environment())
      clusterExport(klaster, "macierz_parametrow_bin", envir = environment())
      clusterExport(klaster, "KNNtrain", envir = environment())
      clusterExport(klaster, "KNNpred") 
      clusterExport(klaster, "ModelOcena")
      clusterExport(klaster, "MinMax")
      clusterExport(klaster, "AUC")
      clusterExport(klaster, "Youden")
      clusterExport(klaster, "Czulosc")
      clusterExport(klaster, "Jakosc")
      clusterExport(klaster, "Specyficznosc")
      clusterExport(klaster, "d_euklides")
      clusterExport(klaster, "d_hamming")
      clusterExport(klaster, "d_porzadkowa")
      
      # wynik <- foreach(id_modele = 1:nrow(macierz_parametrow_bin), .export = c("X", "Y", "dane", "macierz_parametrow_bin", "KNNtrain", "KNNpred", "ModelOcena", "MinMax", "AUC", "Youden", "Czulosc", "Jakosc", "Specyficznosc", "d_euklides"), .combine = rbind) %dopar%
      wynik <- foreach(id_modele = 1:nrow(macierz_parametrow_bin), .combine = rbind) %dopar%
      {
            trening_dane <- dane[trening_walidacja[,macierz_parametrow_bin$k_[id_modele]] == 1,]
            walidacyjne_dane <- dane[trening_walidacja[,macierz_parametrow_bin$k_[id_modele]] == 2,]

            KNN_Model <- KNNtrain(trening_dane[,X], trening_dane[,Y], macierz_parametrow_bin$k[id_modele], 0, 1)

            KNN_pred_Trening <- KNNpred(KNN_Model, X=trening_dane[,X])
            KNN_pred_Walid <- KNNpred(KNN_Model, X=walidacyjne_dane[,X])

            Trening_Ocena = ModelOcena((trening_dane[,Y]), as.numeric(1-KNN_pred_Trening[,1]))
            Walidacja_Ocena = ModelOcena((walidacyjne_dane[,Y]), as.numeric(1-KNN_pred_Walid[,1]))

            macierz_parametrow_bin[id_modele, "AUCT"] <- Trening_Ocena["AUC"]
            macierz_parametrow_bin[id_modele, "CzuloscT"] <- Trening_Ocena["Czulosc"]
            macierz_parametrow_bin[id_modele, "SpecyficznoscT"] <- Trening_Ocena["Specyficznosc"]
            macierz_parametrow_bin[id_modele, "JakoscT"] <- Trening_Ocena["Jakosc"]

            macierz_parametrow_bin[id_modele, "AUCW"] <- Walidacja_Ocena["AUC"]
            macierz_parametrow_bin[id_modele, "CzuloscW"] <- Walidacja_Ocena["Czulosc"]
            macierz_parametrow_bin[id_modele, "SpecyficznoscW"] <- Walidacja_Ocena["Specyficznosc"]
            macierz_parametrow_bin[id_modele, "JakoscW"] <- Walidacja_Ocena["Jakosc"]
            
            return(macierz_parametrow_bin[id_modele,])
        }
      
      stopCluster(klaster)

      env <- foreach:::.foreachGlobals
      rm(list=ls(name=env), pos=env)
      
      macierz_parametrow_bin <- wynik
      return(macierz_parametrow_bin)
      
    }
    else if(typ == "multi")
    {
      cat("Obliczenia dla KNN - problem klasyfikacji wieloklasowej: ")
      # cat(paste0("Liczba modeli x kroswalidacja = ", nrow(macierz_parametrow) * kFold))
      
      macierz_parametrow_multi <- data.frame(macierz_parametrow, ACCT=0, ACCW=0)
      
      clusterExport(klaster, "X", envir = environment())
      clusterExport(klaster, "Y", envir = environment())
      clusterExport(klaster, "dane", envir = environment())
      clusterExport(klaster, "macierz_parametrow_multi", envir = environment())
      clusterExport(klaster, "KNNtrain", envir = environment())
      clusterExport(klaster, "KNNpred") 
      clusterExport(klaster, "ModelOcena")
      clusterExport(klaster, "MinMax")
      clusterExport(klaster, "Jakosc__")
      clusterExport(klaster, "d_euklides")
      clusterExport(klaster, "d_hamming")
      clusterExport(klaster, "d_porzadkowa")
      
      wynik <- foreach(id_modele = 1:nrow(macierz_parametrow_multi), .combine = rbind) %dopar%
      # for(id_modele in 1:nrow(macierz_parametrow_multi))
      {
        # cat(paste0(id_modele," => "))
        
        trening_dane <- dane[trening_walidacja[,macierz_parametrow_multi$k_[id_modele]] == 1,]
        walidacyjne_dane <- dane[trening_walidacja[,macierz_parametrow_multi$k_[id_modele]] == 2,]
        
        KNN_Model <- KNNtrain(trening_dane[,X], trening_dane[,Y], macierz_parametrow_multi$k[id_modele], 0, 1) 
        
        KNN_pred_Trening <- KNNpred(KNN_Model, X=trening_dane[,X])
        KNN_pred_Walid <- KNNpred(KNN_Model, X=walidacyjne_dane[,X])
        
        macierz_parametrow_multi[id_modele, "ACCT"] = ModelOcena((trening_dane[,Y]), as.numeric(KNN_pred_Trening[,"Klasa"]))
        macierz_parametrow_multi[id_modele, "ACCW"] = ModelOcena((walidacyjne_dane[,Y]), as.numeric(KNN_pred_Walid[,"Klasa"]))
      
        return(macierz_parametrow_multi[id_modele,])
      }
      
      stopCluster(klaster)
      
      env <- foreach:::.foreachGlobals
      rm(list=ls(name=env), pos=env)
      
      macierz_parametrow_multi <- wynik
      
      return(macierz_parametrow_multi)
    }
    else if(typ == "reg")
    {
      cat("Obliczenia dla KNN - problem regresji: ")
      # cat(paste0("Liczba modeli x kroswalidacja = ", nrow(macierz_parametrow) * kFold))
      
      macierz_parametrow_reg <- data.frame(macierz_parametrow, MAET=0, MSET=0, MAPET=0, MAEW=0, MSEW=0, MAPEW=0)

      clusterExport(klaster, "X", envir = environment())
      clusterExport(klaster, "Y", envir = environment())
      clusterExport(klaster, "dane", envir = environment())
      clusterExport(klaster, "macierz_parametrow_reg", envir = environment())
      clusterExport(klaster, "KNNtrain", envir = environment())
      clusterExport(klaster, "KNNpred") 
      clusterExport(klaster, "ModelOcena")
      clusterExport(klaster, "MinMax")
      clusterExport(klaster, "MAE")
      clusterExport(klaster, "MSE")
      clusterExport(klaster, "MAPE")
      clusterExport(klaster, "d_euklides")
      clusterExport(klaster, "d_hamming")
      clusterExport(klaster, "d_porzadkowa")
      
      wynik <- foreach(id_modele = 1:nrow(macierz_parametrow_reg), .combine = rbind) %dopar%
      # for(id_modele in 1:nrow(macierz_parametrow_reg))
      {
        # cat(paste0(id_modele," => "))
        
        trening_dane <- dane[trening_walidacja[,macierz_parametrow_reg$k_[id_modele]] == 1,]
        walidacyjne_dane <- dane[trening_walidacja[,macierz_parametrow_reg$k_[id_modele]] == 2,]
        
        KNN_Model <- KNNtrain(trening_dane[,X], trening_dane[,Y], macierz_parametrow_reg$k[id_modele], 0, 1) 
        
        KNN_pred_Trening <- KNNpred(KNN_Model, X=trening_dane[,X])
        KNN_pred_Walid <- KNNpred(KNN_Model, X=walidacyjne_dane[,X])
        
        Ocena_Trening <- ModelOcena(trening_dane[,Y], KNN_pred_Trening)
        Ocena_Walidacja <- ModelOcena(walidacyjne_dane[,Y], KNN_pred_Walid)
        
        macierz_parametrow_reg[id_modele, "MAET"] <- Ocena_Trening["MAE"]
        macierz_parametrow_reg[id_modele, "MSET"] <- Ocena_Trening["MSE"]
        macierz_parametrow_reg[id_modele, "MAPET"] <- Ocena_Trening["MAPE"]
        
        macierz_parametrow_reg[id_modele, "MAEW"] <- Ocena_Walidacja["MAE"]
        macierz_parametrow_reg[id_modele, "MSEW"] <- Ocena_Walidacja["MSE"]
        macierz_parametrow_reg[id_modele, "MAPEW"] <- Ocena_Walidacja["MAPE"]
        
        return(macierz_parametrow_reg[id_modele,])
      }
      
      stopCluster(klaster)
      
      env <- foreach:::.foreachGlobals
      rm(list=ls(name=env), pos=env)
      
      macierz_parametrow_reg <- wynik
      
      return(macierz_parametrow_reg)
    }
    
    
  }

  
  
  
  ### Drzewa decyzyjne ###
  

  if(algorytm == "Tree")
  {
    # cat("Model Progress: ")
    
    if(typ == "bin")
    {
      cat("Obliczenia dla Drzew Decyzyjnych - problem klasyfikacji binarnej: ")
      # cat(paste0("Liczba modeli x kroswalidacja = ", nrow(macierz_parametrow) * kFold))
      
      macierz_parametrow_bin <- data.frame(macierz_parametrow, AUCT=0, CzuloscT=0, SpecyficznoscT=0, JakoscT=0, AUCW=0, CzuloscW=0, SpecyficznoscW=0, JakoscW=0)
      
      clusterExport(klaster, "X", envir = environment())
      clusterExport(klaster, "Y", envir = environment())
      clusterExport(klaster, "dane", envir = environment())
      clusterExport(klaster, "macierz_parametrow_bin", envir = environment())
      clusterExport(klaster, "StopIfNot")
      clusterExport(klaster, "Prob")
      clusterExport(klaster, "Entropy")
      clusterExport(klaster, "Gini")
      clusterExport(klaster, "SS")
      clusterExport(klaster, "AssignInitialMeasures")
      clusterExport(klaster, "AssignInfo")
      clusterExport(klaster, "SpliNum")
      clusterExport(klaster, "SplitVar")
      clusterExport(klaster, "FindBestSplit")
      clusterExport(klaster, "BuildTree")
      clusterExport(klaster, "PE")
      clusterExport(klaster, "PruneTree")
      clusterExport(klaster, "Tree", envir = environment())
      clusterExport(klaster, "Predykcja_Drzewa")
      clusterExport(klaster, "PredictTree")
      clusterExport(klaster, "ModelOcena")
      clusterExport(klaster, "MinMax")
      clusterExport(klaster, "AUC")
      clusterExport(klaster, "Youden")
      clusterExport(klaster, "Czulosc")
      clusterExport(klaster, "Jakosc")
      clusterExport(klaster, "Specyficznosc")

      wynik <- foreach(id_modele = 1:nrow(macierz_parametrow_bin), .combine = rbind, .packages = 'data.tree') %dopar%
      # for(id_modele in 1:nrow(macierz_parametrow_bin))
      {
        # cat(paste0(id_modele," => "))

        trening_dane <- dane[trening_walidacja[,macierz_parametrow_bin$k_[id_modele]] == 1,]
        walidacyjne_dane <- dane[trening_walidacja[,macierz_parametrow_bin$k_[id_modele]] == 2,]

        Tree_Model <- Tree(Y, X, trening_dane, type = macierz_parametrow_bin$type[id_modele], depth =  macierz_parametrow_bin$depth[id_modele], minobs =  macierz_parametrow_bin$minobs[id_modele], overfit =  macierz_parametrow_bin$overfit[id_modele], cf = macierz_parametrow_bin$cf[id_modele])

        Tree_pred_Trening <- PredictTree(Tree_Model, trening_dane[,X])
        Tree_pred_Walid <- PredictTree(Tree_Model, walidacyjne_dane[,X])

        Trening_Ocena = ModelOcena((trening_dane[,Y]), as.numeric(Tree_pred_Trening[,2]))
        Walidacja_Ocena = ModelOcena((walidacyjne_dane[,Y]), as.numeric(Tree_pred_Walid[,2]))

        macierz_parametrow_bin[id_modele, "AUCT"] <- Trening_Ocena["AUC"]
        macierz_parametrow_bin[id_modele, "CzuloscT"] <- Trening_Ocena["Czulosc"]
        macierz_parametrow_bin[id_modele, "SpecyficznoscT"] <- Trening_Ocena["Specyficznosc"]
        macierz_parametrow_bin[id_modele, "JakoscT"] <- Trening_Ocena["Jakosc"]

        macierz_parametrow_bin[id_modele, "AUCW"] <- Walidacja_Ocena["AUC"]
        macierz_parametrow_bin[id_modele, "CzuloscW"] <- Walidacja_Ocena["Czulosc"]
        macierz_parametrow_bin[id_modele, "SpecyficznoscW"] <- Walidacja_Ocena["Specyficznosc"]
        macierz_parametrow_bin[id_modele, "JakoscW"] <- Walidacja_Ocena["Jakosc"]
        
        return(macierz_parametrow_bin[id_modele,])
      }
      
      stopCluster(klaster)
      
      env <- foreach:::.foreachGlobals
      rm(list=ls(name=env), pos=env)
      
      macierz_parametrow_bin <- wynik
      
      return(macierz_parametrow_bin)
      
    }
    else if(typ == "multi")
    {
      macierz_parametrow_multi <- data.frame(macierz_parametrow, ACCT=0, ACCW=0)
      
      cat("Obliczenia dla Drzew Decyzyjnych - problem klasyfikacji wieloklasowej: ")
      # cat(paste0("Liczba modeli x kroswalidacja = ", nrow(macierz_parametrow) * kFold))
      
      clusterExport(klaster, "X", envir = environment())
      clusterExport(klaster, "Y", envir = environment())
      clusterExport(klaster, "dane", envir = environment())
      clusterExport(klaster, "macierz_parametrow_multi", envir = environment())
      clusterExport(klaster, "StopIfNot")
      clusterExport(klaster, "Prob")
      clusterExport(klaster, "Entropy")
      clusterExport(klaster, "Gini")
      clusterExport(klaster, "SS")
      clusterExport(klaster, "AssignInitialMeasures")
      clusterExport(klaster, "AssignInfo")
      clusterExport(klaster, "SpliNum")
      clusterExport(klaster, "SplitVar")
      clusterExport(klaster, "FindBestSplit")
      clusterExport(klaster, "BuildTree")
      clusterExport(klaster, "PE")
      clusterExport(klaster, "PruneTree")
      clusterExport(klaster, "Tree", envir = environment())
      clusterExport(klaster, "Predykcja_Drzewa")
      clusterExport(klaster, "PredictTree")
      clusterExport(klaster, "ModelOcena")
      clusterExport(klaster, "MinMax")
      clusterExport(klaster, "Jakosc__")
      
      wynik <- foreach(id_modele = 1:nrow(macierz_parametrow_multi), .combine = rbind, .packages = 'data.tree') %dopar%
      # for(id_modele in 1:nrow(macierz_parametrow_multi))
      {
        cat(paste0(id_modele," => "))
        
        trening_dane <- dane[trening_walidacja[,macierz_parametrow_multi$k_[id_modele]] == 1,]
        walidacyjne_dane <- dane[trening_walidacja[,macierz_parametrow_multi$k_[id_modele]] == 2,]

        Tree_Model <- Tree(Y, X, trening_dane, type = macierz_parametrow_multi$type[id_modele], depth =  macierz_parametrow_multi$depth[id_modele], minobs =  macierz_parametrow_multi$minobs[id_modele], overfit =  macierz_parametrow_multi$overfit[id_modele], cf = macierz_parametrow_multi$cf[id_modele]) 
        
        Tree_pred_Trening <- PredictTree(Tree_Model, trening_dane[,X])
        print(Tree_pred_Trening)
        
        Tree_pred_Walid <- PredictTree(Tree_Model, walidacyjne_dane[,X])
        
        macierz_parametrow_multi[id_modele, "ACCT"] <- ModelOcena(trening_dane[,Y], Tree_pred_Trening[,"Class"])
        macierz_parametrow_multi[id_modele, "ACCW"] <- ModelOcena(walidacyjne_dane[,Y], Tree_pred_Walid[,"Class"])
        
        return(macierz_parametrow_multi[id_modele,])
      }
      
      stopCluster(klaster)
      
      env <- foreach:::.foreachGlobals
      rm(list=ls(name=env), pos=env)
      
      macierz_parametrow_multi <- wynik
      
      return(macierz_parametrow_multi)
    }
    else if(typ == "reg")
    {
      macierz_parametrow_reg <- data.frame(macierz_parametrow, MAET=0, MSET=0, MAPET=0, MAEW=0, MSEW=0, MAPEW=0)
      
      cat("Obliczenia dla Drzew Decyzyjnych - problem regresji: ")
      # cat(paste0("Liczba modeli x kroswalidacja = ", nrow(macierz_parametrow) * kFold))
      
      clusterExport(klaster, "X", envir = environment())
      clusterExport(klaster, "Y", envir = environment())
      clusterExport(klaster, "dane", envir = environment())
      clusterExport(klaster, "macierz_parametrow_reg", envir = environment())
      clusterExport(klaster, "StopIfNot")
      clusterExport(klaster, "Prob")
      clusterExport(klaster, "Entropy")
      clusterExport(klaster, "Gini")
      clusterExport(klaster, "SS")
      clusterExport(klaster, "AssignInitialMeasures")
      clusterExport(klaster, "AssignInfo")
      clusterExport(klaster, "SpliNum")
      clusterExport(klaster, "SplitVar")
      clusterExport(klaster, "FindBestSplit")
      clusterExport(klaster, "BuildTree")
      clusterExport(klaster, "PE")
      clusterExport(klaster, "PruneTree")
      clusterExport(klaster, "Tree", envir = environment())
      clusterExport(klaster, "Predykcja_Drzewa")
      clusterExport(klaster, "PredictTree")
      clusterExport(klaster, "ModelOcena")
      clusterExport(klaster, "MinMax")
      clusterExport(klaster, "MAE")
      clusterExport(klaster, "MSE")
      clusterExport(klaster, "MAPE")
      
      wynik <- foreach(id_modele = 1:nrow(macierz_parametrow_reg), .combine = rbind, .packages = 'data.tree') %dopar%
      # for(id_modele in 1:nrow(macierz_parametrow_reg))
      {
        cat(paste0(id_modele," => "))
        
        trening_dane <- dane[trening_walidacja[,macierz_parametrow_reg$k_[id_modele]] == 1,]
        walidacyjne_dane <- dane[trening_walidacja[,macierz_parametrow_reg$k_[id_modele]] == 2,]
        
        Tree_Model <- Tree(Y, X, trening_dane, type = macierz_parametrow_reg$type[id_modele], depth =  macierz_parametrow_reg$depth[id_modele], minobs =  macierz_parametrow_reg$minobs[id_modele], overfit =  macierz_parametrow_reg$overfit[id_modele], cf = macierz_parametrow_reg$cf[id_modele]) 

        Ocena_Trening <- ModelOcena(trening_dane[,Y], PredictTree(Tree_Model, trening_dane[,X]))
        Ocena_Walidacja <- ModelOcena(walidacyjne_dane[,Y], PredictTree(Tree_Model, walidacyjne_dane[,X]))
    
        macierz_parametrow_reg[id_modele, "MAET"] <- Ocena_Trening["MAE"]
        macierz_parametrow_reg[id_modele, "MSET"] <- Ocena_Trening["MSE"]
        macierz_parametrow_reg[id_modele, "MAPET"] <- Ocena_Trening["MAPE"]
        
        macierz_parametrow_reg[id_modele, "MAEW"] <- Ocena_Walidacja["MAE"]
        macierz_parametrow_reg[id_modele, "MSEW"] <- Ocena_Walidacja["MSE"]
        macierz_parametrow_reg[id_modele, "MAPEW"] <- Ocena_Walidacja["MAPE"]
        
        return(macierz_parametrow_reg[id_modele,])
      }
      
      stopCluster(klaster)
      
      env <- foreach:::.foreachGlobals
      rm(list=ls(name=env), pos=env)
      
      macierz_parametrow_reg <- wynik
      
      return(macierz_parametrow_reg)
    }
    
    
  }
  

  
  
  ### Sieci Neuronowe ###
  
  
  if(algorytm == "NN")
  {
    

    if(typ == "bin")
    {
      cat("Obliczenia dla Sieci Neuronowych - problem klasyfikacji binarnej: ")
      # cat(paste0("Liczba modeli x kroswalidacja = ", nrow(macierz_parametrow) * kFold))
      
      macierz_parametrow_bin <- data.frame(macierz_parametrow, AUCT=0, CzuloscT=0, SpecyficznoscT=0, JakoscT=0, AUCW=0, CzuloscW=0, SpecyficznoscW=0, JakoscW=0)

      clusterExport(klaster, "X", envir = environment())
      clusterExport(klaster, "Y", envir = environment())
      clusterExport(klaster, "dane", envir = environment())
      clusterExport(klaster, "macierz_parametrow_bin", envir = environment())
      clusterExport(klaster, "sigmoid")
      clusterExport(klaster, "dsigmoid")
      clusterExport(klaster, "ReLu")
      clusterExport(klaster, "dReLu")
      clusterExport(klaster, "lossSS")
      clusterExport(klaster, "SoftMax")
      clusterExport(klaster, "MinMaxOdwrot")
      clusterExport(klaster, "wprzod")
      clusterExport(klaster, "wstecz")
      clusterExport(klaster, "trainNN")
      clusterExport(klaster, "predNN")
      clusterExport(klaster, "ModelOcena")
      clusterExport(klaster, "MinMax")
      clusterExport(klaster, "AUC")
      clusterExport(klaster, "Youden")
      clusterExport(klaster, "Czulosc")
      clusterExport(klaster, "Jakosc")
      clusterExport(klaster, "Specyficznosc")
      
      wynik <- foreach(id_modele = 1:nrow(macierz_parametrow_bin), .combine = rbind) %dopar%
      # for(id_modele in 1:nrow(macierz_parametrow_bin))
      {
        cat(paste0("\t ", id_modele," => "))

        dane_bin_NN <- dane
        dane_bin_NN[,X] <- sapply(dane_bin_NN[,X], MinMax)
        
        trening_dane <- dane_bin_NN[trening_walidacja[,macierz_parametrow_bin$k_[id_modele]] == 1,]
        walidacyjne_dane <- dane_bin_NN[trening_walidacja[,macierz_parametrow_bin$k_[id_modele]] == 2,]

        NN_trening_X = as.matrix(trening_dane[,X])
        NN_trening_Y = model.matrix( ~ trening_dane[,Y] - 1, trening_dane)
        NN_walidacja_X = as.matrix(walidacyjne_dane[,X])
        NN_walidacja_Y = model.matrix( ~ walidacyjne_dane[,Y] - 1, walidacyjne_dane )
        
        NN_Model <- trainNN( NN_trening_X, NN_trening_Y, h = macierz_parametrow_bin$h[id_modele], lr = macierz_parametrow_bin$lr[id_modele], iter = macierz_parametrow_bin$iter[id_modele], seed = 399, typ = typ)

        NN_pred_Trening <- predNN(NN_trening_X, NN_Model, typ = typ)
        NN_pred_Walid <- predNN(NN_walidacja_X, NN_Model, typ = typ)
        
        Trening_Ocena = ModelOcena(trening_dane[,Y], NN_pred_Trening[,2])
        Walidacja_Ocena = ModelOcena(walidacyjne_dane[,Y], NN_pred_Walid[,2])
        
        macierz_parametrow_bin[id_modele, "AUCT"] <- Trening_Ocena["AUC"]
        macierz_parametrow_bin[id_modele, "CzuloscT"] <- Trening_Ocena["Czulosc"]
        macierz_parametrow_bin[id_modele, "SpecyficznoscT"] <- Trening_Ocena["Specyficznosc"]
        macierz_parametrow_bin[id_modele, "JakoscT"] <- Trening_Ocena["Jakosc"]
        
        macierz_parametrow_bin[id_modele, "AUCW"] <- Walidacja_Ocena["AUC"]
        macierz_parametrow_bin[id_modele, "CzuloscW"] <- Walidacja_Ocena["Czulosc"]
        macierz_parametrow_bin[id_modele, "SpecyficznoscW"] <- Walidacja_Ocena["Specyficznosc"]
        macierz_parametrow_bin[id_modele, "JakoscW"] <- Walidacja_Ocena["Jakosc"]
        
        return(macierz_parametrow_bin[id_modele,])
      }
      
      stopCluster(klaster)
      
      env <- foreach:::.foreachGlobals
      rm(list=ls(name=env), pos=env)
      
      macierz_parametrow_bin <- wynik
      
      return(macierz_parametrow_bin)
      
    }
    else if(typ == "multi")
    {
      cat("Obliczenia dla Sieci Neuronowych - problem klasyfikacji wieloklasowej: ")
      # cat(paste0("Liczba modeli x kroswalidacja = ", nrow(macierz_parametrow) * kFold))
      
      macierz_parametrow_multi <- data.frame(macierz_parametrow, ACCT=0, ACCW=0)
      
      clusterExport(klaster, "X", envir = environment())
      clusterExport(klaster, "Y", envir = environment())
      clusterExport(klaster, "dane", envir = environment())
      clusterExport(klaster, "macierz_parametrow_multi", envir = environment())
      clusterExport(klaster, "sigmoid")
      clusterExport(klaster, "dsigmoid")
      clusterExport(klaster, "ReLu")
      clusterExport(klaster, "dReLu")
      clusterExport(klaster, "lossSS")
      clusterExport(klaster, "SoftMax")
      clusterExport(klaster, "MinMaxOdwrot")
      clusterExport(klaster, "wprzod")
      clusterExport(klaster, "wstecz")
      clusterExport(klaster, "trainNN")
      clusterExport(klaster, "predNN")
      clusterExport(klaster, "ModelOcena")
      clusterExport(klaster, "MinMax")
      clusterExport(klaster, "Jakosc__")

      wynik <- foreach(id_modele = 1:nrow(macierz_parametrow_multi), .combine = rbind) %dopar%
      # for(id_modele in 1:nrow(macierz_parametrow_multi))
      {
        cat(paste0("\t ", id_modele," => "))
        
        dane_multi_NN <- dane
        dane_multi_NN[,X] <- sapply(dane_multi_NN[,X], MinMax)
        
        trening_dane <- dane_multi_NN[trening_walidacja[,macierz_parametrow_multi$k_[id_modele]] == 1,]
        walidacyjne_dane <- dane_multi_NN[trening_walidacja[,macierz_parametrow_multi$k_[id_modele]] == 2,]
        
        NN_trening_X = as.matrix(trening_dane[,X])
        NN_trening_Y = model.matrix( ~ trening_dane[,Y] - 1, trening_dane)
        NN_walidacja_X = as.matrix(walidacyjne_dane[,X])
        NN_walidacja_Y = model.matrix( ~ walidacyjne_dane[,Y] - 1, walidacyjne_dane )
        
        NN_Model <- trainNN( NN_trening_X, NN_trening_Y, h = macierz_parametrow_multi$h[id_modele], lr = macierz_parametrow_multi$lr[id_modele], iter = macierz_parametrow_multi$iter[id_modele], seed = 399, typ = typ)
        
        NN_pred_Trening <- predNN(NN_trening_X, NN_Model, typ = typ)
        NN_pred_Walid <- predNN(NN_walidacja_X, NN_Model, typ = typ)
        
        NN_pred_Trening_multi <- as.numeric( levels(dane_multi_NN[,Y])[apply( NN_pred_Trening, 1, which.max )] )
        NN_pred_Walid_multi <- as.numeric( levels(dane_multi_NN[,Y])[apply( NN_pred_Walid, 1, which.max )] )
        
        macierz_parametrow_multi[id_modele, "ACCT"] <- ModelOcena(trening_dane[,Y], NN_pred_Trening_multi)
        macierz_parametrow_multi[id_modele, "ACCW"] <- ModelOcena(walidacyjne_dane[,Y], NN_pred_Walid_multi)
        
        return(macierz_parametrow_multi[id_modele,])
      }
      
      stopCluster(klaster)
      
      env <- foreach:::.foreachGlobals
      rm(list=ls(name=env), pos=env)
      
      macierz_parametrow_multi <- wynik
      
      return(macierz_parametrow_multi)
    }
    else if(typ == "reg")
    {
      cat("Obliczenia dla Sieci Neuronowych - problem regresji: ")
      # cat(paste0("Liczba modeli x kroswalidacja = ", nrow(macierz_parametrow) * kFold))
      
      macierz_parametrow_reg <- data.frame(macierz_parametrow, MAET=0, MSET=0, MAPET=0, MAEW=0, MSEW=0, MAPEW=0)
      
      clusterExport(klaster, "X", envir = environment())
      clusterExport(klaster, "Y", envir = environment())
      clusterExport(klaster, "dane", envir = environment())
      clusterExport(klaster, "macierz_parametrow_reg", envir = environment())
      clusterExport(klaster, "sigmoid")
      clusterExport(klaster, "dsigmoid")
      clusterExport(klaster, "ReLu")
      clusterExport(klaster, "dReLu")
      clusterExport(klaster, "lossSS")
      clusterExport(klaster, "SoftMax")
      clusterExport(klaster, "MinMaxOdwrot")
      clusterExport(klaster, "wprzod")
      clusterExport(klaster, "wstecz")
      clusterExport(klaster, "trainNN")
      clusterExport(klaster, "predNN")
      clusterExport(klaster, "ModelOcena")
      clusterExport(klaster, "MinMax")
      clusterExport(klaster, "MAE")
      clusterExport(klaster, "MSE")
      clusterExport(klaster, "MAPE")
      
      wynik <- foreach(id_modele = 1:nrow(macierz_parametrow_reg), .combine = rbind) %dopar%
      # for(id_modele in 1:nrow(macierz_parametrow_reg))
      {
        cat(paste0("\t ", id_modele," => "))
        
        dane_reg_NN <- sapply(dane, MinMax)
        
        trening_dane <- dane_reg_NN[trening_walidacja[,macierz_parametrow_reg$k_[id_modele]] == 1,]
        walidacyjne_dane <- dane_reg_NN[trening_walidacja[,macierz_parametrow_reg$k_[id_modele]] == 2,]
        
        NN_trening_X = as.matrix(trening_dane[,X])
        NN_trening_Y = as.matrix(trening_dane[,Y])
        NN_walidacja_X = as.matrix(walidacyjne_dane[,X])
        NN_walidacja_Y = as.matrix(walidacyjne_dane[,Y])
        
        NN_Model <- trainNN(NN_trening_X, NN_trening_Y, h = macierz_parametrow_reg$h[id_modele], lr = macierz_parametrow_reg$lr[id_modele], iter = macierz_parametrow_reg$iter[id_modele], seed = 399, typ = typ)
        
        NN_pred_Trening <- predNN(NN_trening_X, NN_Model, typ = typ)
        NN_pred_Walid <- predNN(NN_walidacja_X, NN_Model, typ = typ)
        
        NN_pred_Trening_reg <- MinMaxOdwrot(NN_pred_Trening[,1], y_min = min(dane[,Y]), y_max = max(dane[,Y]))
        NN_pred_Walid_reg <- MinMaxOdwrot(NN_pred_Walid[,1], y_min = min(dane[,Y]), y_max = max(dane[,Y]))
        
        Ocena_Trening <- ModelOcena(dane[trening_walidacja[,macierz_parametrow_reg$k_[id_modele]] == 1,Y], NN_pred_Trening_reg)
        Ocena_Walidacja <- ModelOcena(dane[trening_walidacja[,macierz_parametrow_reg$k_[id_modele]] == 2,Y], NN_pred_Walid_reg)
        
        macierz_parametrow_reg[id_modele, "MAET"] <- Ocena_Trening["MAE"]
        macierz_parametrow_reg[id_modele, "MSET"] <- Ocena_Trening["MSE"]
        macierz_parametrow_reg[id_modele, "MAPET"] <- Ocena_Trening["MAPE"]
        
        macierz_parametrow_reg[id_modele, "MAEW"] <- Ocena_Walidacja["MAE"]
        macierz_parametrow_reg[id_modele, "MSEW"] <- Ocena_Walidacja["MSE"]
        macierz_parametrow_reg[id_modele, "MAPEW"] <- Ocena_Walidacja["MAPE"]
        
        return(macierz_parametrow_reg[id_modele,])
      }
      
      stopCluster(klaster)
      
      env <- foreach:::.foreachGlobals
      rm(list=ls(name=env), pos=env)
      
      macierz_parametrow_reg <- wynik
      
      return(macierz_parametrow_reg)
    }
  }
}
  
