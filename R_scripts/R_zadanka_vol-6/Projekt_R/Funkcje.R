library(parallel)
library(doParallel)
library(foreach)
library(tidyverse)
library(pROC)
library(ggplot2)
library(dplyr)
library(caret)
library(rpart)
library(nnet)

  ##  FUNKCJE ##

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



## K-najblizszych sasiadow ##

KNNtrain <- function(X, y_tar, k, XminNew=0, XmaxNew=1) 
{
  if (any(is.na(X) == TRUE) || any(is.na(y_tar) == TRUE) || k <= 0 || (is.data.frame(X) == FALSE & is.matrix(X) == FALSE) )
  {
    stop("Bledne dane lub parametry")
  }
  else
  {
    X <- data.frame(X)
    X_new <- data.frame(matrix(0,ncol = ncol(X), nrow = nrow(X)))
    colnames(X_new) <- colnames(X)
    
    kolumny <- vector()
    minOrg <- vector()
    maxOrg <- vector()
    minmaxNew <- c(XminNew, XmaxNew)
    
    for(i in 1:ncol(X)) 
    {
      kolumny <- append(kolumny, i)
      
      if (is.numeric(X[,i])) 
      {
        X_new[,i] <- MinMax(X[,i])
        #X_new[,i] <- ((X[,i] - min(X[,i])) / (max(X[,i]) - min(X[,i]))) * (XmaxNew - XminNew) + XminNew
        minOrg <- append(minOrg, min(X[,i]))
        maxOrg <- append(maxOrg, max(X[,i]))
      }
      else if(is.factor(X[,i]) & is.ordered(X[,i]) || is.factor(X[,i]))
      {
        X_new[,i] <- X[,i]
        minOrg <- append(minOrg, NA)
        maxOrg <- append(maxOrg, NA)
      }
      else
      {
        stop("Bledne kolumny")
      }
    }
    
    names(maxOrg) <- kolumny
    names(minOrg) <- kolumny
    attr(X_new, 'minOrg') <- minOrg
    attr(X_new, 'maxOrg') <- maxOrg
    attr(X_new, 'minmaxNew') <- minmaxNew
    
    model_treningowy <- list()
    model_treningowy[["X"]] <- X_new
    model_treningowy[["y"]] <- y_tar
    model_treningowy[["k"]] <- k
    
    return(model_treningowy)
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
  if (is.na(KNNmodel) == TRUE || is.na(X) == TRUE || ncol(KNNmodel$X) != ncol(X) || colnames(KNNmodel$X) != colnames(X)){
    stop("Niekompletne dane!")
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
    
    for (i in 1:n_kolumn_znorm) {
      if(is.numeric(X[,i])){
        min_k <- as.numeric(attributes(KNNmodel$X)$minOrg[i])
        max_k <- as.numeric(attributes(KNNmodel$X)$maxOrg[i])
        newmin_k <- as.numeric(attributes(KNNmodel$X)$minmaxNew[1])
        newmax_k <- as.numeric(attributes(KNNmodel$X)$minmaxNew[2])
        
        X_znormalizowane[,i] <- ((X[,i] - min_k) / (max_k - min_k)) * (newmax_k - newmin_k) + newmin_k
        kolumny_numeryczne <- kolumny_numeryczne + 1
      }
      else if(is.factor(X[,i])){
        X_znormalizowane[,i] <- X[,i]
        kolumny_factor <- kolumny_factor + 1
      }
      else if(is.factor(X[,i]) & is.ordered(X[,i])){
        X_znormalizowane[,i] <- X[,i]
        kolumny_factor_order <- kolumny_factor_order + 1
      }
    }
    
    
    odleglosc <- matrix(0, n_wierszy_model, n_wierszy_znorm)
    
    if(kolumny_numeryczne == n_kolumn_znorm){
      for(i in 1:n_wierszy_model){
        for(j in 1:n_wierszy_znorm){
          odleglosc[i,j] <- d_euklides(KNNmodel$X[i,], X_znormalizowane[j,])
        }
      }
    }
    else if(kolumny_factor == n_kolumn_znorm){
      for(i in 1:n_wierszy_model){
        for(j in 1:n_wierszy_znorm){
          odleglosc[i,j] <- d_hamming(KNNmodel$X[i,], X_znormalizowane[j,], n_kolumn_znorm)
        }
      }
    }
    else if(kolumny_factor_order == n_kolumn_znorm){
      for(i in 1:n_wierszy_model){
        for(j in 1:n_wierszy_znorm){
          for(k in 1:n_kolumn_znorm){
            unikalne <- nlevels(X_znormalizowane[,k])
            odleglosc[i,j] <- d_porzadkowa(KNNmodel$X[i,], X_znormalizowane[j,], unikalne)
          }
        }
      }
    }
    else
    {
      for(i in 1:n_wierszy_model){
        for(j in 1:n_wierszy_znorm){
          temp <- 0
          
          for(k in 1:n_kolumn_znorm){
            if(is.numeric(X_znormalizowane[,k])){
              max_k <- as.numeric(attributes(KNNmodel$X)$maxOrg[k])
              min_k <- as.numeric(attributes(KNNmodel$X)$minOrg[k])
              
              temp <- temp + (abs(KNNmodel$X[i,k] - X_znormalizowane[j,k]) / (max_k -  min_k)) 
            }
            else if(is.factor(X_znormalizowane[,k])){
              if(KNNmodel$X[i,k] != X_znormalizowane[j,k]){
                temp <- temp + 1
              }
            }
            else if(is.factor(X_znormalizowane[,k]) & is.ordered(X_znormalizowane[,k])){
              z_i <- (i - 1) / (n_wierszy_model - 1)
              z_n <- (j - 1) / (n_wierszy_znorm - 1)
              temp <- temp + (abs(z_i - z_n) / (n_wierszy_model - 1))
            }
          }
          odleglosc[i, j] <- temp / n_kolumn_znorm
        }
      }
    }
    
    
    if(is.numeric(KNNmodel$y)){
      predykcja <- double(n_kolumn_znorm)
      
      for(i in 1:n_wierszy_znorm){
        k_najblizej <- order(odleglosc[,i])[1:KNNmodel$k]
        
        y_predykcja <- mean(KNNmodel$y[k_najblizej])
        
        predykcja[i] <- y_predykcja
      }
      return(predykcja)
    }
    else if(is.factor(KNNmodel$y)){
      predykcja <- as.data.frame(matrix(nrow = n_wierszy_znorm, ncol = nlevels(KNNmodel$y)+1))
      
      for(i in 1:n_wierszy_znorm){
        k_najblizej <- order(odleglosc[,i])[1:KNNmodel$k]
        
        print(k_najblizej)
        
        if(nlevels(KNNmodel$y) == 2){
          print(as.numeric(KNNmodel$y[k_najblizej]) == 1)
          print(as.numeric(KNNmodel$y[k_najblizej]) == 0)
          
          pozytywna <- sum(as.numeric(KNNmodel$y[k_najblizej]) == 1) / KNNmodel$k
          negatywna <- sum(as.numeric(KNNmodel$y[k_najblizej]) == 0) / KNNmodel$k
          
          predykcja_klasy <- ifelse(pozytywna >= 0.5, 'P', 'N')
          
          names(predykcja) <- c('P', 'N', 'Klasa')
          predykcja[i, 1] <- pozytywna
          predykcja[i, 2] <- negatywna
          predykcja[i, 3] <- predykcja_klasy
        }
        else if(nlevels(KNNmodel$y) > 2){
          etykiety <- sort(unique(KNNmodel$y))
          names(predykcja) <- etykiety
          names(predykcja)[nlevels(KNNmodel$y)+1] <- 'Klasa'
          
          for (j in 1:length(etykiety)){
            pozytywna <- sum(KNNmodel$y[k_najblizej] == as.character(etykiety[j])) / KNNmodel$k
            predykcja[i,j] <- pozytywna
          }
          
          predykcja_klasy <- etykiety[which.max(predykcja[i,])]
          predykcja[i,'Klasa'] <- as.factor(predykcja_klasy)
        }
      }
      return(predykcja)
    }
    else{
      stop("Dane y modelu sa niepoprawne!")
    }
  }
}



  ##  Drzewa decyzyjne  ##

library(data.tree)
StopIfNot <- function(Y, X, data, type, depth, minobs, overfit, cf) {
  
  if (!is.data.frame(data)) {
    mes<<-("Zbior danych musi byc tabela danych!")
    return(FALSE)} 
  else if (!all(c(Y,X) %in% names(data))) {  
    mes<<-("Zmienne nie istnieja w zbiorze danych!")
    return(FALSE)} 
  else if (any(is.na(data[,c(Y,X)]))) {
    mes<<-("Wystpuja braki danych!")
    return(FALSE)} 
  else if (depth<=0 | minobs<=0) {
    mes<<-("Parametry depth i minobs musza byc wieksze od 0!")
    return(FALSE)} 
  else if (!(type %in% c("Gini", "Entropy", "SS"))) {
    mes <<- ("Type powinien przyjmować jedna z wartosci Gini, Entropy lub SS")
    return(FALSE)} 
  else if (!(overfit %in% c("none", "prune"))) {
    mes <<- ("overfit powinien przyjmować jedna z wartosci none lub prune")
    return(FALSE)} 
  else if (cf<=0 | cf>0.5) {
    mes <<- ("cf powinien byc z przedzialu [0,0.5]")
    return(FALSE)} 
  #niedopuszczalne kombinacje to SS i y factor, Entropy i y numeryczny oraz gini i y numeryczny
  else if (type=="SS" & is.factor(data[,Y]) | type=="Entropy" & is.numeric(data[,Y]) | type=="Gini" & is.numeric(data[,Y]))  {
    mes <<- ("Bledna kombinacja parametrow!")
    return(FALSE)} 
  else {
    return(TRUE)
  }
  
}

Prob <- function( y){
  result <- unname( table( y ) )
  result <- result / sum( result )
  return( result )
}

Entropy <- function(prob){ 
  if (any(prob > 1 | prob < 0) == TRUE) {
    print("Prawdopodobienstwo powinno byc z przedzialu [0,1]")
  }
  else {
    result <- -prob * log2(prob)
    result[prob == 0] <- 0
    result <- sum(result)
    return(result)
  }
}

Gini <- function(prob) { 
  if (any(prob > 1 | prob < 0) == TRUE) {
    print("Prawdopodobienstwo powinno byc z przedzialu [0,1]")
  }
  else {
    result <- 1 - sum(prob^2)
    return (result)
  }
}

SS <- function(y){
  result <- (y-mean(y))^2
  result <- sum(result)
  return(result)
}


AssignInitialMeasures <- function(tree, Y, data, type, depth) {
  tree$Depth <- 0
  if (type == "Entropy") {
    Entropy <- Entropy(Prob(data[,Y]))
    tree$inf <- Entropy
  }
  else if (type == "Gini") {
    Gini <- Gini(Prob(data[,Y]))
    tree$inf <- Gini
  }
  else {
    SS <- SS(data[,Y])
    tree$inf <- SS
  }
  return (tree)
}

AssignInfo <- function(tree, Y, X, data, type, depth, minobs, overfit, cf) {
  attr(tree, 'Y') <- data[,Y]
  attr(tree, 'X') <- data[,X]
  attr(tree, 'data') <- data
  attr(tree, 'type') <- type
  attr(tree, 'depth') <- depth
  attr(tree, 'minobs') <- minobs
  attr(tree, 'overfit') <- overfit
  attr(tree, 'cf') <- cf
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


SplitNum <- function(Y, x, parentVal, splits, minobs, type) {
  
  n <- length( x )
  res <- data.frame( matrix( 0, length(splits), 6 ) )
  colnames( res ) <- c("InfGain","lVal","rVal","point","ln","rn")
  
  for( i in 1:length(splits) ) {
    
    if (is.numeric(x)) { 
      partition <- x <= splits[i] 
    }
    
    else {
      partition <- x %in% splits[1:i]
    }
    
    ln <- sum(partition)
    rn <- n - ln
    
    if( any((c(ln,rn) < minobs | any(is.na(c(ln,rn)))))) {
      res[i,] <- 0
    }
    
    else {
      lVal <- if (type=="Gini") { Gini(Prob(Y[partition])) } 
      else if (type=="Entropy") { Entropy(Prob(Y[partition])) }
      else { SS(Y[partition]) }   
      
      rVal <- if (type=="Gini") { Gini(Prob(Y[!partition])) }
      else if (type=="Entropy") { Entropy(Prob(Y[!partition])) }
      else { SS(Y[!partition]) } 
      
      InfGain <- parentVal - ( lVal * ln/n  + rVal * rn/n )
      
      res[i,"InfGain"] <- InfGain
      res[i,"lVal"] <- lVal
      res[i,"rVal"] <- rVal
      
      if (is.numeric(x)) {
        res[i,"point"] <- splits[i]
      }
      else {
        res[i,"point"] <- as.character(paste(splits[i]))
      }
      
      res[i,"ln"] <- ln
      res[i,"rn"] <- rn
    }
  }
  return(res)
}

SplitVar <- function(Y, x, parentVal, minobs, type) {
  s <- unique(x)
  
  if(length(x) == 1) {
    splits <- s
  }
  
  else {
    splits <- head(sort( s ), -1)
  }
  
  res <- SplitNum(Y, x, parentVal, splits, minobs, type)
  incl <- res$ln >= minobs & res$rn >= minobs & res$InfGain > 0
  res <- res[ incl, , drop = F ]
  best <- which.max(res$InfGain)
  res <- res[best, , drop = F]
  
  return(res)
}

FindBestSplit <- function(Y, X, data, parentVal, type, minobs) {
  
  res <- sapply(X, function(i) {
    
    if (is.numeric(data[,i])) {
      dane <- data[,i]
    }
    else if (is.ordered(data[,i])) {
      dane <- as.numeric(paste(data[,i]))
    }
    else {
      if (is.factor(data[,Y])) {
        temp <- data[data[,Y] == 1,]
        d <- prop.table(table(temp[,i]))
        d <- sort(d)
        dane <- (paste(factor(data[,i], levels = names(d), ordered = TRUE)))
      }
      else {
        d <- tapply(data[,Y], data[,i], mean)
        d <- sort(d)
        dane <- (paste(factor(data[,i], levels = names(d), ordered = TRUE)))
      }
    }
    SplitVar(data[,Y], dane, parentVal, minobs, type)
  }, simplify = F )
  
  res <- do.call("rbind", res)
  best <- which.max(res$InfGain)
  res <- res[best, , drop = F]
  
  return (res)
}

BuildTree <- function(node, Y, X, data, depth, type , minobs) {
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
  
  if ((is.factor(data[,Y] ) & (node$Depth == depth | ifStop | all(node$Prob %in% c(0,1)))) | (!is.factor(data[,Y]) & (node$Depth == depth | ifStop))) {
    node$Leaf <- "*"
    return( node )
  }
  
  else {
    
    split_indx <- data[,rownames(bestSplit)] <= bestSplit$point
    child_frame <- split(data, split_indx)
    
    name_l <- sprintf("%s <= %s", rownames(bestSplit), bestSplit$point ) 
    child_l <- node$AddChild(name_l)
    child_l$value <- split_indx
    child_l$Depth <- node$Depth + 1
    child_l$inf <- bestSplit$lVal
    child_l$feature <- rownames(bestSplit)
    child_l$BestSplit <- bestSplit$point
    
    BuildTree(child_l, Y, X, child_frame[["TRUE"]], depth, type, minobs)
    
    name_r <- sprintf( "%s >  %s", rownames(bestSplit), bestSplit$point )
    child_r <- node$AddChild( name_r )
    child_r$value <- split_indx
    child_r$Depth <- node$Depth + 1
    child_r$inf <- bestSplit$rVal
    child_r$feature <- rownames(bestSplit)
    child_r$BestSplit <- bestSplit$point
    
    BuildTree(child_r, Y, X, child_frame[["FALSE"]], depth, type, minobs) 
  }
}

Tree <- function(Y, X, data, type, depth, minobs, overfit, cf ) {
  
  if (StopIfNot(Y, X, data, type, depth, minobs, overfit,cf) == FALSE) {
    return(FALSE)
  }
  
  tree<- Node$new("Root")
  tree$Count <- nrow(data)
  
  if (type == "Entropy") {
    tree$inf <- Entropy(Prob(data[,Y]))
  }
  else if (type == "Gini") {
    tree$inf <- Gini(Prob(data[,Y]))
  }
  else {
    tree$inf <- SS(data[,Y])
  }
  
  tree <- AssignInitialMeasures(tree, Y, data, type, depth)
  BuildTree(tree, Y, X, data, depth, type, minobs)  
  if(overfit == 'prune'){ PruneTree(tree, cf) }
  AssignInfo(tree,Y,X,data,type,depth, minobs, overfit, cf )
  
  return(tree)
}



strTree <- function(tree, obs) {
  if (tree$isLeaf) { 
    return((c((tree$Prob), "Class" = (tree$Class))))}
  
  if (is.numeric(tree$children[[1]]$BestSplit) | is.ordered(tree$children[[1]]$BestSplit)) {
    child <- tree$children[[ifelse(obs[,tree$children[[1]]$feature] > (tree$children[[1]]$BestSplit), 2, 1)]]
  }else{ 
    split <- tree$children[[1]]$feature
    child <- tree$children[[ifelse((obs[,tree$children[[1]]$feature] %in% split), 1, 2)]] 
  }
  return (strTree(child,obs))
}


PredictTree <- function(tree, data){
  if (is.factor(attributes(tree)$Y)){
    out <- data.frame(matrix(0, nrow = nrow(data), ncol = nlevels(attributes(tree)$Y)+1))
    for (i in 1:nrow(data)){out[i,] <- (strTree(tree, data[i, ,drop = F]))}
    out[,ncol(out)] <- factor(out[,ncol(out)], levels = levels(attributes(tree)$Y))
  }else{
    out <- c()
    for (i in 1:nrow(data)){out[i] <- (strTree(tree, data[i, ,drop = F])['Class'])}
  }
  return (out)
}



  ##  Sieci Neuronowe ##

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
  return(exp( x ) / sum( exp( x ) ))
}
MinMax <- function( x ){
  return( ( x - min(x) ) / ( max(x) - min(x) ) )
}
MinMaxOdwrot <- function( x, y_min, y_max ){
  return(  x * (y_max - y_min) + y_min )
}


wprzod <- function( X, W1, W2, W3, typ ){
  h1 <- cbind( matrix( 1, nrow = nrow(X) ), sigmoid( X %*% W1 )  )
  h2 <- cbind( matrix( 1, nrow = nrow(X) ), sigmoid( h1 %*% W2 )  )
  if (typ =="bin"){
    y_hat <- sigmoid( h2 %*% W3 ) # klasyfikacja binarna
  }
  else if (typ == "multi"){
    y_hat <- matrix( t( apply( h2 %*% W3, 1, SoftMax ) ), nrow = nrow(X) ) # klasyfikacja wieloklasowa
  }
  else if (typ == "reg"){
    y_hat <- h2 %*% W3 # regresja
  }
  return( list( y_hat = y_hat, H1 = h1, H2 = h2 ) )
}


wstecz <- function( X, y_tar, y_hat, W1, W2, W3, H1, H2, lr, typ ){
  if (typ =="bin"){
    dy_hat <- (y_tar - y_hat) * dsigmoid( y_hat ) # klasyfikacja binarna
  }
  else if (typ == "multi"){
    dy_hat <- (y_tar - y_hat) / nrow( X ) # klasyfikacja wieloklasowa
  }
  else if (typ == "reg"){
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

trainNN <- function( x, y_tar, h = c(5,5), lr = 0.01, iter = 10000, seed = 123, typ = "bin" ){
  if (typ != "bin" & typ != "multi" & typ != "reg") {
    stop("bledny typ danych")
  }
  set.seed( seed )
  X <- cbind( rep( 1, nrow(x) ), x )
  
  h = unlist(h, use.names = FALSE)
  
  W1 <- matrix( runif( ncol(X) * h[1], -1, 1 ), nrow = ncol(X) )
  W2 <- matrix( runif( (h[1]+1) * h[2], -1, 1 ), nrow = h[1] + 1 )
  W3 <- matrix( runif( (h[2]+1) * ncol(y_tar), -1, 1 ), nrow = h[2] + 1 )
  
  # error <- double( iter )
  
  for( i in 1:iter ){
    sygnalwprzod <- wprzod( X, W1, W2, W3, typ=typ )
    sygnalwtyl <- wstecz( X, y_tar, y_hat = sygnalwprzod$y_hat, W1, W2, W3, H1 = sygnalwprzod$H1, H2 = sygnalwprzod$H2, lr, typ=typ )
    W1 <- sygnalwtyl$W1
    W2 <- sygnalwtyl$W2
    W3 <- sygnalwtyl$W3
    cat( paste0( "\rIteracja: ", i , " / ", iter) )
    
    # error[i] <- lossSS( y_tar, sygnalwprzod$y_hat )
  }
  # x_wartosci <- seq(1,MaxIter, length = 1000)
  # print(qplot(x_wartosci, error, main = "Error", xlab = "Iteracje", geom = "line"))
  return( list( y_hat = sygnalwprzod$y_hat, W1 = W1, W2 = W2, W3 = W3 ) )
}


predNN <- function( xnew, nn, typ = "bin" ){
  xnew <- cbind( rep( 1, nrow(xnew) ), xnew )
  h1 <- cbind( matrix( 1, nrow = nrow(xnew) ), sigmoid( xnew %*% nn$W1 )  )
  h2 <- cbind( matrix( 1, nrow = nrow(xnew) ), sigmoid( h1 %*% nn$W2 )  )
  # y_hat <- sigmoid( h2 %*% nn$W3 )
  
  if (typ =="bin"){
    y_hat <- sigmoid( h2 %*% nn$W3 ) # klasyfikacja binarna
  }
  else if (typ == "multi"){
    y_hat <- matrix( t( apply( h2 %*% nn$W3, 1, SoftMax ) ), nrow = nrow(xnew) ) # klasyfikacja wieloklasowa
  } 
  else if (typ == "reg"){
    y_hat <- h2 %*% nn$W3 # regresja
  }
  
  return( y_hat )
}



  ## STATYSTYKI ##


library("pROC")
MAE <- function(y_tar, y_hat){ return(mean(abs(y_tar - y_hat)))}
MSE <- function(y_tar, y_hat){ return(mean((y_tar - y_hat)^2))}
MAPE <- function(y_tar, y_hat){ return( mean(abs((y_tar - y_hat)/y_tar) ))}

#AUC_MT = suma(czulosc* specyficznosc t - specyficznosc t-1) + suma ((specyficznosc t - specyficznosc t-1)*(czulosc t - czulosc t-1))

AUC <- function(y_tar, y_hat){
  indx_modeluroc <- pROC::roc(y_tar, y_hat, quiet = TRUE)
  #dla poprawnosci wynikow czulosc i specyficznosc sortuje rosnaco
  sensit<- sort(indx_modeluroc$"sensitivities", F) 
  specif<- sort(1 - indx_modeluroc$"specificities", F) 
  diff_specif <- c(diff(sensit), 0)
  diff_sensit <- c(diff(specif), 0) 
  AUC_wynik <- sum(sensit * diff_sensit) + sum(diff_specif* diff_sensit)/2
  return(AUC_wynik)
}
#J = argt Czu?o?? + Specyficzno?? - 1
J <- function(y_tar, y_hat){
  indx_modeluroc <- pROC::roc(y_tar, y_hat, quiet = TRUE)
  sensit <- indx_modeluroc$"sensitivities" 
  specif<- indx_modeluroc$"specificities"
  max_J <- 0
  for (i in 1:length(sensit)) {
    J <- (sensit[i] + specif[i]-1)
    if(J > max_J){
      max_J=J
    }
  }
  return(max_J)
}
# Mat <- table(y_tar, y_hat = ifelse(y_hat <= J(y_tar, y_hat), 0, 1))
Czulosc <- function(Mat){ return((Mat[1] / (Mat[1] + Mat[3])))}
Specyficznosc <- function(Mat){ return((Mat[4] / (Mat[4] + Mat[2])))}
Jakosc <- function(Mat){ return(((Mat[1] + Mat[4]) / (Mat[1] + Mat[2] + Mat[3] + Mat[4])))}
Jakosc_multi <- function(y_tar, y_hat){ return( sum(as.numeric(y_tar) == as.numeric(y_hat)) / length(y_tar) )}

ModelOcena <- function(y_tar, y_hat){
  if(is.numeric(y_tar)){
    regresja <- c("MAE" = MAE(y_tar, y_hat), "MSE" = MSE(y_tar, y_hat), "MAPE" = MAPE(y_tar, y_hat))
    return(regresja)
  }
  else if(is.factor(y_tar) & nlevels(y_tar) == 2){
    Mat <- table(y_tar, y_hat = ifelse(y_hat <= J(y_tar, y_hat), 0, 1))
    miary <- c( "AUC" = AUC(y_tar, y_hat), "Czulosc" = Czulosc(Mat), "Specyficznosc" = Specyficznosc(Mat), "Jakosc" = Jakosc(Mat))
    # klasyfikacja <- list(Mat, J(y_tar, y_hat), miary)
    klasyfikacja <- miary
    return(klasyfikacja)
  }
  else if(is.factor(y_tar) & nlevels(y_tar) > 2){
    klasyfikacja_multi <- c("Jakosc" = Jakosc_multi(y_tar, y_hat))
    return(klasyfikacja_multi)
  }
  else{
    c("Dane niepoprawne")
  }
}


  ##  Kroswalidacja ##


CrossValidTune <- function(dane, X, Y, kFold, parTune, algorytm, seed = 264)
{
  set.seed(seed)
  
  if(is.numeric(dane[,Y])){ typ = "reg" }
  else if(is.factor(dane[,Y])){
    if (nlevels(dane[,Y]) == 2){ typ = "bin" }
    else if(nlevels(dane[,Y]) > 2){ typ = "multi" } }
  
  
  print(paste0("Algorytm: ", algorytm))
  print(paste0("Dane: ", typ))
  print(paste0("Liczba wariantow modeli: ", nrow(parTune), " -> liczba wszystkim modeli = ", nrow(parTune) * kFold))
  
  
  wiersze = nrow(dane)
  tabela_indx <- data.frame(matrix(ncol = kFold, nrow = nrow(dane)))
  
  tabela <- as.data.frame(expand_grid(indx_modelu = c(1:kFold), parTune))
  
  for(i in 1:kFold){ 
    indx_tr <- sample( 1:wiersze, size = (1/kFold * wiersze)-1, replace = F )
    tabela_indx[indx_tr,i] <- 2
    tabela_indx[-indx_tr,i] <- 1
  }
  
  

  
  if(algorytm == "KNN"){
    cat("ID modelu: ")
    
    nCores <- detectCores()
    klaster <- makeCluster(nCores-1)
    registerDoParallel(klaster)
    
    if(typ == "bin"){
      tabela_bin <- data.frame(tabela, AUCT=0, CzuloscT=0, SpecyficznoscT=0, JakoscT=0, AUCW=0, CzuloscW=0, SpecyficznoscW=0, JakoscW=0)
      
      wynik_bin <- foreach(indx_tabeli = 1:nrow(tabela_bin), .export = c("X", "Y", "dane", "tabela_bin", "KNNtrain", "KNNpred", "ModelOcena", "MinMax", "AUC", "J", "Specyficznosc", "Czulosc", "Jakosc", "Jakosc_multi", "d_euklides", "d_hamming", "d_porzadkowa"), .combine = rbind) %dopar%
      # wynik_bin <- foreach(indx_tabeli = 1:1, .export = c("X", "Y", "dane", "tabela_bin", "KNNtrain", "KNNpred", "ModelOcena", "MinMax", "AUC", "J", "Specyficznosc", "Czulosc", "Jakosc", "Jakosc_multi", "d_euklides", "d_hamming", "d_porzadkowa"), .combine = rbind) %dopar%
      {
      # for(indx_tabeli in 1:nrow(tabela_bin)){
        cat(paste0(indx_tabeli,"="))
        
        dane_tr <- dane[tabela_indx[,tabela_bin$indx_modelu[indx_tabeli]] == 1,]
        dane_wal <- dane[tabela_indx[,tabela_bin$indx_modelu[indx_tabeli]] == 2,]
        
        KNN_Model <- KNNtrain(dane_tr[,X], dane_tr[,Y], tabela_bin$k[indx_tabeli], 0, 1) 
        KNN_pred_Trening <- KNNpred(KNN_Model, X=dane_tr[,X])
        KNN_pred_Walid <- KNNpred(KNN_Model, X=dane_wal[,X])
        
        Trening_Ocena = ModelOcena((dane_tr[,Y]), as.numeric(1-KNN_pred_Trening[,1]))
        Walidacja_Ocena = ModelOcena((dane_wal[,Y]), as.numeric(1-KNN_pred_Walid[,1]))
        tabela_bin[indx_tabeli, "AUCT"] <- Trening_Ocena["AUC"]
        tabela_bin[indx_tabeli, "CzuloscT"] <- Trening_Ocena["Czulosc"]
        tabela_bin[indx_tabeli, "SpecyficznoscT"] <- Trening_Ocena["Specyficznosc"]
        tabela_bin[indx_tabeli, "JakoscT"] <- Trening_Ocena["Jakosc"]
        tabela_bin[indx_tabeli, "AUCW"] <- Walidacja_Ocena["AUC"]
        tabela_bin[indx_tabeli, "CzuloscW"] <- Walidacja_Ocena["Czulosc"]
        tabela_bin[indx_tabeli, "SpecyficznoscW"] <- Walidacja_Ocena["Specyficznosc"]
        tabela_bin[indx_tabeli, "JakoscW"] <- Walidacja_Ocena["Jakosc"]
        
        return(tabela_bin[indx_tabeli,])
        # return(tabela_bin)
      }
      stopCluster(klaster)
      
      env <- foreach:::.foreachGlobals
      rm(list=ls(name=env), pos=env)
      
      tabela_bin <- wynik_bin
      return(tabela_bin)
    }
    else if(typ == "multi"){
      tabela_multi <- data.frame(tabela, ACCT=0, ACCW=0)
      
      wynik_multi <- foreach(indx_tabeli = 1:nrow(tabela_multi), .export = c("X", "Y", "dane", "tabela_multi", "KNNtrain", "KNNpred", "ModelOcena", "MinMax", "AUC", "J", "Specyficznosc", "Czulosc", "Jakosc", "Jakosc_multi", "d_euklides", "d_hamming", "d_porzadkowa"), .combine = rbind) %dopar%
      {
      # for(indx_tabeli in 1:nrow(tabela_multi)){
        cat(paste0(indx_tabeli,"="))
        
        dane_tr <- dane[tabela_indx[,tabela_multi$indx_modelu[indx_tabeli]] == 1,]
        dane_wal <- dane[tabela_indx[,tabela_multi$indx_modelu[indx_tabeli]] == 2,]
        
        KNN_Model <- KNNtrain(dane_tr[,X], dane_tr[,Y], tabela_multi$k[indx_tabeli], 0, 1) 
        KNN_pred_Trening <- KNNpred(KNN_Model, X=dane_tr[,X])
        KNN_pred_Walid <- KNNpred(KNN_Model, X=dane_wal[,X])
        
        tabela_multi[indx_tabeli, "ACCT"] = ModelOcena((dane_tr[,Y]), as.numeric(KNN_pred_Trening[,"Klasa"]))
        tabela_multi[indx_tabeli, "ACCW"] = ModelOcena((dane_wal[,Y]), as.numeric(KNN_pred_Walid[,"Klasa"]))
        
        return(tabela_multi[indx_tabeli,])
        # return(tabela_multi)
      }
      stopCluster(klaster)
      
      env <- foreach:::.foreachGlobals
      rm(list=ls(name=env), pos=env)
      
      tabela_multi <- wynik_multi
      return(tabela_multi)
    }
    else if(typ == "reg"){
      tabela_reg <- data.frame(tabela, MAET=0, MSET=0, MAPET=0, 
                               MAEW=0, MSEW=0, MAPEW=0)
      
      wynik_reg <- foreach(indx_tabeli = 1:nrow(tabela_reg), .export = c("X", "Y", "dane", "tabela_reg", "KNNtrain", "KNNpred", "ModelOcena", "MinMax", "MSE", "MAE", "MAPE", "d_euklides", "d_hamming", "d_porzadkowa"), .combine = rbind) %dopar%
      {
      # for(indx_tabeli in 1:nrow(tabela_reg)){
        cat(paste0(indx_tabeli,"="))
        
        dane_tr <- dane[tabela_indx[,tabela_reg$indx_modelu[indx_tabeli]] == 1,]
        dane_wal <- dane[tabela_indx[,tabela_reg$indx_modelu[indx_tabeli]] == 2,]
        
        KNN_Model <- KNNtrain(dane_tr[,X], dane_tr[,Y], tabela_reg$k[indx_tabeli], 0, 1) 
        KNN_pred_Trening <- KNNpred(KNN_Model, X=dane_tr[,X])
        KNN_pred_Walid <- KNNpred(KNN_Model, X=dane_wal[,X])
        
        Ocena_Trening <- ModelOcena(dane_tr[,Y], KNN_pred_Trening)
        Ocena_Walidacja <- ModelOcena(dane_wal[,Y], KNN_pred_Walid)
        tabela_reg[indx_tabeli, "MAET"] <- Ocena_Trening["MAE"]
        tabela_reg[indx_tabeli, "MSET"] <- Ocena_Trening["MSE"]
        tabela_reg[indx_tabeli, "MAPET"] <- Ocena_Trening["MAPE"]
        tabela_reg[indx_tabeli, "MAEW"] <- Ocena_Walidacja["MAE"]
        tabela_reg[indx_tabeli, "MSEW"] <- Ocena_Walidacja["MSE"]
        tabela_reg[indx_tabeli, "MAPEW"] <- Ocena_Walidacja["MAPE"]
        
        return(tabela_reg[indx_tabeli,])
        # return(tabela_reg)
      }
      stopCluster(klaster)
      
      env <- foreach:::.foreachGlobals
      rm(list=ls(name=env), pos=env)
      
      tabela_reg <- wynik_reg
      return(tabela_reg)
    }
    
    # stopCluster(klaster)
    # 
    # env <- foreach:::.foreachGlobals
    # rm(list=ls(name=env), pos=env)
  }


  if(algorytm == "Tree"){
    cat("ID modelu: ")
    
    if(typ == "bin"){
      tabela_bin <- data.frame(tabela, AUCT=0, CzuloscT=0, SpecyficznoscT=0, JakoscT=0, 
                               AUCW=0, CzuloscW=0, SpecyficznoscW=0, JakoscW=0)
      
      # wynik_bin <- foreach(indx_tabeli = 1:nrow(tabela_bin), .export = c("X", "Y", "dane", "tabela_bin", "Tree", "PredictTree", "strTree", "BuildTree", "FindBestSplit", "SplitVar", "SplitNum", "PruneTree", "PE", "AssignInfo", "AssignInitialMeasures", "SS", "Gini", "Entropy", "Prob", "StopIfNot", "ModelOcena", "MinMax", "AUC", "J", "Specyficznosc", "Czulosc", "Jakosc", "Jakosc_multi"), .combine = rbind) %dopar%
      # wynik_bin <- foreach(indx_tabeli = 1:1, .export = c("X", "Y", "dane", "tabela_bin", "Tree", "PredictTree", "strTree", "BuildTree", "FindBestSplit", "SplitVar", "SplitNum", "PruneTree", "PE", "AssignInfo", "AssignInitialMeasures", "SS", "Gini", "Entropy", "Prob", "StopIfNot", "ModelOcena", "MinMax", "AUC", "J", "Specyficznosc", "Czulosc", "Jakosc", "Jakosc_multi"), .combine = rbind) %dopar%
        
      # {
      for(indx_tabeli in 1:nrow(tabela_bin)){
        cat(paste0(indx_tabeli,"="))
        
        dane_tr <- dane[tabela_indx[,tabela_bin$indx_modelu[indx_tabeli]] == 1,]
        dane_wal <- dane[tabela_indx[,tabela_bin$indx_modelu[indx_tabeli]] == 2,]
        
        Tree_Model <- Tree(Y, X, dane_tr, type = tabela_bin$type[indx_tabeli], depth =  tabela_bin$depth[indx_tabeli], minobs =  tabela_bin$minobs[indx_tabeli], overfit =  tabela_bin$overfit[indx_tabeli], cf = tabela_bin$cf[indx_tabeli]) 
        Tree_pred_Trening <- PredictTree(Tree_Model, dane_tr[,X])
        Tree_pred_Walid <- PredictTree(Tree_Model, dane_wal[,X])

        Trening_Ocena = ModelOcena((dane_tr[,Y]), as.numeric(Tree_pred_Trening[,2]))
        Walidacja_Ocena = ModelOcena((dane_wal[,Y]), as.numeric(Tree_pred_Walid[,2]))
        tabela_bin[indx_tabeli, "AUCT"] <- Trening_Ocena["AUC"]
        tabela_bin[indx_tabeli, "CzuloscT"] <- Trening_Ocena["Czulosc"]
        tabela_bin[indx_tabeli, "SpecyficznoscT"] <- Trening_Ocena["Specyficznosc"]
        tabela_bin[indx_tabeli, "JakoscT"] <- Trening_Ocena["Jakosc"]
        tabela_bin[indx_tabeli, "AUCW"] <- Walidacja_Ocena["AUC"]
        tabela_bin[indx_tabeli, "CzuloscW"] <- Walidacja_Ocena["Czulosc"]
        tabela_bin[indx_tabeli, "SpecyficznoscW"] <- Walidacja_Ocena["Specyficznosc"]
        tabela_bin[indx_tabeli, "JakoscW"] <- Walidacja_Ocena["Jakosc"]
        
        # return(tabela_bin[indx_tabeli,])
        # return(tabela_bin)
      }
      # stopCluster(klaster)
      # 
      # env <- foreach:::.foreachGlobals
      # rm(list=ls(name=env), pos=env)
      # 
      # tabela_bin <- wynik_bin
      return(tabela_bin)
    }
    else if(typ == "multi"){
      tabela_multi <- data.frame(tabela, ACCT=0, ACCW=0)
      
      # wynik_multi <- foreach(indx_tabeli = 1:nrow(tabela_multi), .export = c("X", "Y", "dane", "tabela_multi", "Tree", "PredictTree", "strTree", "BuildTree", "FindBestSplit", "SplitVar", "SplitNum", "PruneTree", "PE", "AssignInfo", "AssignInitialMeasures", "SS", "Gini", "Entropy", "Prob", "StopIfNot", "ModelOcena", "MinMax", "AUC", "J", "Specyficznosc", "Czulosc", "Jakosc", "Jakosc_multi"), .combine = rbind) %dopar%
      # {
      for(indx_tabeli in 1:nrow(tabela_multi)){
        cat(paste0(indx_tabeli,"="))
        
        dane_tr <- dane[tabela_indx[,tabela_multi$indx_modelu[indx_tabeli]] == 1,]
        dane_wal <- dane[tabela_indx[,tabela_multi$indx_modelu[indx_tabeli]] == 2,]
        
        Tree_Model <- Tree(Y, X, dane_tr, type = tabela_multi$type[indx_tabeli], depth =  tabela_multi$depth[indx_tabeli], minobs =  tabela_multi$minobs[indx_tabeli], overfit =  tabela_multi$overfit[indx_tabeli], cf = tabela_multi$cf[indx_tabeli]) 
        Tree_pred_Trening <- PredictTree(Tree_Model, dane_tr[,X])
        Tree_pred_Walid <- PredictTree(Tree_Model, dane_wal[,X])
        
        tabela_multi[indx_tabeli, "ACCT"] <- ModelOcena(dane_tr[,Y], Tree_pred_Trening[,ncol(Tree_pred_Trening)])
        tabela_multi[indx_tabeli, "ACCW"] <- ModelOcena(dane_wal[,Y], Tree_pred_Walid[,ncol(Tree_pred_Walid)])
        
        # return(tabela_multi[indx_tabeli,])
        # return(tabela_multi)
      }
      # stopCluster(klaster)
      # 
      # env <- foreach:::.foreachGlobals
      # rm(list=ls(name=env), pos=env)
      
      # tabela_multi <- wynik_multi
      return(tabela_multi)
    }
    else if(typ == "reg"){
      tabela_reg <- data.frame(tabela, MAET=0, MSET=0, MAPET=0, 
                               MAEW=0, MSEW=0, MAPEW=0)
      
      # wynik_reg <- foreach(indx_tabeli = 1:nrow(tabela_reg), .export = c("X", "Y", "dane", "tabela_reg", "Tree", "PredictTree", "strTree", "BuildTree", "FindBestSplit", "SplitVar", "SplitNum", "PruneTree", "PE", "AssignInfo", "AssignInitialMeasures", "SS", "Gini", "Entropy", "Prob", "StopIfNot", "ModelOcena", "MinMax", "MAE", "MSE", "MAPE"), .combine = rbind) %dopar%
      # {
      for(indx_tabeli in 1:nrow(tabela_reg)){
        cat(paste0(indx_tabeli,"="))
        
        dane_tr <- dane[tabela_indx[,tabela_reg$indx_modelu[indx_tabeli]] == 1,]
        dane_wal <- dane[tabela_indx[,tabela_reg$indx_modelu[indx_tabeli]] == 2,]
        
        Tree_Model <- Tree(Y, X, dane_tr, type = tabela_reg$type[indx_tabeli], depth =  tabela_reg$depth[indx_tabeli], minobs =  tabela_reg$minobs[indx_tabeli], overfit =  tabela_reg$overfit[indx_tabeli], cf = tabela_reg$cf[indx_tabeli]) 

        Ocena_Trening <- ModelOcena(dane_tr[,Y], as.numeric(PredictTree(Tree_Model, dane_tr[,X])))
        Ocena_Walidacja <- ModelOcena(dane_wal[,Y], as.numeric(PredictTree(Tree_Model, dane_wal[,X])))
        tabela_reg[indx_tabeli, "MAET"] <- Ocena_Trening["MAE"]
        tabela_reg[indx_tabeli, "MSET"] <- Ocena_Trening["MSE"]
        tabela_reg[indx_tabeli, "MAPET"] <- Ocena_Trening["MAPE"]
        tabela_reg[indx_tabeli, "MAEW"] <- Ocena_Walidacja["MAE"]
        tabela_reg[indx_tabeli, "MSEW"] <- Ocena_Walidacja["MSE"]
        tabela_reg[indx_tabeli, "MAPEW"] <- Ocena_Walidacja["MAPE"]
        
        # return(tabela_reg[indx_tabeli,])
        # return(tabela_reg)
      }
      # stopCluster(klaster)
      
      # env <- foreach:::.foreachGlobals
      # rm(list=ls(name=env), pos=env)
      
      # tabela_reg <- wynik_reg
      return(tabela_reg)
    }
  }
  
  if(algorytm == "NN"){
    cat("ID modelu: ")
    
    if(typ == "bin"){
      tabela_bin <- data.frame(tabela, AUCT=0, CzuloscT=0, SpecyficznoscT=0, JakoscT=0, 
                               AUCW=0, CzuloscW=0, SpecyficznoscW=0, JakoscW=0)
      
      for(indx_tabeli in 1:nrow(tabela_bin)){
        cat(paste0(indx_tabeli,"="))
        
        dane_bin_NN <- dane
        dane_bin_NN[,X] <- sapply(dane_bin_NN[,X], MinMax)
        
        dane_tr <- dane_bin_NN[tabela_indx[,tabela_bin$indx_modelu[indx_tabeli]] == 1,]
        dane_wal <- dane_bin_NN[tabela_indx[,tabela_bin$indx_modelu[indx_tabeli]] == 2,]
        
        X_treningowe_NN = as.matrix(dane_tr[,X])
        Y_treningowe_NN = model.matrix( ~ dane_tr[,Y] - 1, dane_tr)
        X_walidacyjne_NN = as.matrix(dane_wal[,X])
        Y_walidacyjne_NN = model.matrix( ~ dane_wal[,Y] - 1, dane_wal )
        
        NN_Model <- trainNN( X_treningowe_NN, Y_treningowe_NN, h = tabela_bin$h[indx_tabeli], lr = tabela_bin$lr[indx_tabeli], iter = tabela_bin$iter[indx_tabeli], seed = 264, typ = typ)
        NN_pred_Trening <- predNN(X_treningowe_NN, NN_Model, typ = typ)
        NN_pred_Walid <- predNN(X_walidacyjne_NN, NN_Model, typ = typ)
        
        Trening_Ocena = ModelOcena(dane_tr[,Y], NN_pred_Trening[,2])
        Walidacja_Ocena = ModelOcena(dane_wal[,Y], NN_pred_Walid[,2])
        tabela_bin[indx_tabeli, "AUCT"] <- Trening_Ocena["AUC"]
        tabela_bin[indx_tabeli, "CzuloscT"] <- Trening_Ocena["Czulosc"]
        tabela_bin[indx_tabeli, "SpecyficznoscT"] <- Trening_Ocena["Specyficznosc"]
        tabela_bin[indx_tabeli, "JakoscT"] <- Trening_Ocena["Jakosc"]
        tabela_bin[indx_tabeli, "AUCW"] <- Walidacja_Ocena["AUC"]
        tabela_bin[indx_tabeli, "CzuloscW"] <- Walidacja_Ocena["Czulosc"]
        tabela_bin[indx_tabeli, "SpecyficznoscW"] <- Walidacja_Ocena["Specyficznosc"]
        tabela_bin[indx_tabeli, "JakoscW"] <- Walidacja_Ocena["Jakosc"]
        
        # return(tabela_bin)
      }
      return(tabela_bin)
    }
    else if(typ == "multi"){
      tabela_multi <- data.frame(tabela, ACCT=0, ACCW=0)
      
      for(indx_tabeli in 1:nrow(tabela_multi)){
        cat(paste0(indx_tabeli,"="))
        
        dane_multi_NN <- dane
        dane_multi_NN[,X] <- sapply(dane_multi_NN[,X], MinMax)
        dane_tr <- dane_multi_NN[tabela_indx[,tabela_multi$indx_modelu[indx_tabeli]] == 1,]
        dane_wal <- dane_multi_NN[tabela_indx[,tabela_multi$indx_modelu[indx_tabeli]] == 2,]
        Y_treningowe_NN = model.matrix( ~ dane_tr[,Y] - 1, dane_tr)
        Y_walidacyjne_NN = model.matrix( ~ dane_wal[,Y] - 1, dane_wal )
        
        NN_Model <- trainNN( as.matrix(dane_tr[,X]), Y_treningowe_NN, h = tabela_multi$h[indx_tabeli], lr = tabela_multi$lr[indx_tabeli], iter = tabela_multi$iter[indx_tabeli], seed = 264, typ = typ)
        NN_pred_Trening <- predNN(as.matrix(dane_tr[,X]), NN_Model, typ = typ)
        NN_pred_Walid <- predNN(as.matrix(dane_wal[,X]), NN_Model, typ = typ)

        NN_pred_Trening_multi <- factor( levels(dane_tr[,dane_multi_Y])[apply( NN_pred_Trening, 1, which.max )], levels = levels(dane_tr[,Y]))
        NN_pred_Walid_multi <- factor( levels(dane_wal[,dane_multi_Y])[apply( NN_pred_Walid, 1, which.max )], levels = levels(dane_tr[,Y]))
        tabela_multi[indx_tabeli, "ACCT"] <- ModelOcena(dane_tr[,Y], NN_pred_Trening_multi)
        tabela_multi[indx_tabeli, "ACCW"] <- ModelOcena(dane_wal[,Y], NN_pred_Walid_multi)
        
        # return(tabela_multi)
      }
      return(tabela_multi)
    }
    else if(typ == "reg"){
      tabela_reg <- data.frame(tabela, MAET=0, MSET=0, MAPET=0, MAEW=0, MSEW=0, MAPEW=0)
      
      for(indx_tabeli in 1:nrow(tabela_reg)){
        cat(paste0(indx_tabeli,"="))
        
        dane_reg_NN <- sapply(dane, MinMax)
        dane_tr <- dane_reg_NN[tabela_indx[,tabela_reg$indx_modelu[indx_tabeli]] == 1,]
        dane_wal <- dane_reg_NN[tabela_indx[,tabela_reg$indx_modelu[indx_tabeli]] == 2,]
        X_treningowe_NN = as.matrix(dane_tr[,X])
        Y_treningowe_NN = as.matrix(dane_tr[,Y])
        X_walidacyjne_NN = as.matrix(dane_wal[,X])
        Y_walidacyjne_NN = as.matrix(dane_wal[,Y])
        
        NN_Model <- trainNN(X_treningowe_NN, Y_treningowe_NN, h = tabela_reg$h[indx_tabeli], lr = tabela_reg$lr[indx_tabeli], iter = tabela_reg$iter[indx_tabeli], seed = 264, typ = typ)
        NN_pred_Trening <- predNN(X_treningowe_NN, NN_Model, typ = typ)
        NN_pred_Walid <- predNN(X_walidacyjne_NN, NN_Model, typ = typ)
        
        NN_pred_Trening_reg <- MinMaxOdwrot(NN_pred_Trening[,1], y_min = min(dane[,Y]), y_max = max(dane[,Y]))
        NN_pred_Walid_reg <- MinMaxOdwrot(NN_pred_Walid[,1], y_min = min(dane[,Y]), y_max = max(dane[,Y]))
        Ocena_Trening <- ModelOcena(dane[tabela_indx[,tabela_reg$indx_modelu[indx_tabeli]] == 1,Y], NN_pred_Trening_reg)
        Ocena_Walidacja <- ModelOcena(dane[tabela_indx[,tabela_reg$indx_modelu[indx_tabeli]] == 2,Y], NN_pred_Walid_reg)
        tabela_reg[indx_tabeli, "MAET"] <- Ocena_Trening["MAE"]
        tabela_reg[indx_tabeli, "MSET"] <- Ocena_Trening["MSE"]
        tabela_reg[indx_tabeli, "MAPET"] <- Ocena_Trening["MAPE"]
        tabela_reg[indx_tabeli, "MAEW"] <- Ocena_Walidacja["MAE"]
        tabela_reg[indx_tabeli, "MSEW"] <- Ocena_Walidacja["MSE"]
        tabela_reg[indx_tabeli, "MAPEW"] <- Ocena_Walidacja["MAPE"]
        
        # return(tabela_reg)
      }
      return(tabela_reg)
    }
  }
}



