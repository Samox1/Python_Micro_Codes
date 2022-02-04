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
library(doParallel)
library(foreach)


# KNN 

MinMax <- function(x, new_min = 0, new_max = 1){
  return(((x - min(x)) / (max(x) - min(x))) * (new_max - new_min) + new_min)
}


KNNtrain <- function(X, y_tar, k, XminNew, XmaxNew) 
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
        X_new[,i] <- MinMax(X[,i], new_min = XminNew, new_max = XmaxNew)
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
          pozytywna <- sum(KNNmodel$y[k_najblizej] == 1) / KNNmodel$k
          negatywna <- sum(KNNmodel$y[k_najblizej] == 0) / KNNmodel$k
          
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







# Drzewa decyzyjne

StopIfNot <- function(Y, X, data, type, depth, minobs, overfit, cf){

  war1 <- is.data.frame(data)
  war2 <- all(is.element(Y, colnames(data)))
  war3 <- all(is.element(X, colnames(data)))

  if(war2 && war3)
  {
    war4 <- !any(is.na(data[,Y]))
    war5 <- !any(is.na(data[,X]))
    war11 <- (is.numeric(data[,Y]) && type=="SS") || (is.factor(data[,Y]) && type=="Gini") || (is.factor(data[,Y]) && type == "Entropy")
  }
  else{
    warning("Nie ma wskazanych kolumn")
    return(FALSE)
  }

  war6 <- depth > 0
  war7 <- minobs > 0
  war8 <- (type == "Gini" || type == "SS" || type == "Entropy")
  war9 <- (overfit == "none" || overfit =="prune")
  war10 <- cf > 0 && cf <= 0.5


  if(war1 == FALSE){
    warning("tabela danych nie jest typu 'data.frame'")
  }
  if(war4 == FALSE){
    warning("kolumny 'Y' -> braki danych")
  }
  if(war5 == FALSE){
    warning("kolumny 'X' -> braki danych")
  }
  if(war6 == FALSE){
    warning("parametr 'depth' powinien byc wiekszy od 0")
  }
  if(war7 == FALSE){
    warning("parametr 'minobs' powinien byc wiekszy od 0")
  }
  if(war8 == FALSE){
    warning("parametr 'type' moze przyjmowac wartosci: 'Gini', 'Entropy', 'SS'")
  }
  if(war9 == FALSE){
    warning("parametr 'overfit' powinien byc: 'none' lub 'prune'")
  }
  if(war10 == FALSE){
    warning("parametr 'cf' zawiera sie w przedziale: (0,0.5]")
  }
  if(war11 == FALSE){
    warning("kombinacja 'Y' i parametru 'type' nie ma sensu")
  }

  if(war1 && war2 && war3 && war4 && war5 && war6 && war7 && war8 && war9 && war10 && war11)
  {
    return(TRUE)
  }
  else
  {
    return(FALSE)
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

Gini <- function(prob){
  res <- prob*prob
  res <- sum(res)
  res <- 1-res
  return(res)
}

SS <- function(y){
  n<-length(y)
  y_hat <- sum(y)/n
  res <- y-y_hat
  res <- res*res
  res <- sum(res)
  return(res)
}


AssignInitialMeasures <- function(tree, Y, data, type, depth){
  tree$Depth <- 0

  if(type == "SS")
  {
    value <- SS(data[,Y])
    tree$inf <- value
  }
  else if(type == "Gini")
  {
    pr <- Prob(data[,Y])
    value <- Gini(pr)
    tree$inf <- value
  }
  else if(type == "Entropy")
  {
    pr <- Prob(data[,Y])
    value <- Entropy(pr)
    tree$inf <- value
  }
  return(tree)
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

SpliNum <- function( Y, x, parentVal, splits, minobs, type){
  
  n <- length( x )
  res <- data.frame( matrix( 0, length(splits), 6 ) )
  colnames( res ) <- c("InfGain","lVal","rVal","point","ln","rn")
  
  for( i in 1:length(splits))
  {
    if(is.numeric(x))
    {
      partition <- x <= splits[i] 
    }
    else
    {
      partition <- x %in% splits[1:i]
    }
    
    ln <- sum( partition )
    rn <- n - ln
    
    if( any((c(ln,rn) < minobs| any(is.na(c(ln,rn))))))
    { 
      res[i,] <- 0
    }
    else
    {
      if(type=="Entropy")
      {
        prob1 <- Prob(Y[partition])
        prob2 <- Prob(Y[!partition])
        lVal <- Entropy(prob1)
        rVal <- Entropy(prob2)
      }
      else if(type=="Gini")
      {
        prob1 <- Prob(Y[partition])
        prob2 <- Prob(Y[!partition])
        lVal <- Gini(prob1)
        rVal <- Gini(prob2)
      }
      else if(type=="SS")
      {
        lVal <- SS(Y[partition])
        rVal <- SS(Y[!partition])
      }

      InfGain <- parentVal - (ln/n * lVal + rn/n * rVal)
      res[i,"InfGain"] <- InfGain
      res[i,"lVal"] <- lVal
      res[i,"rVal"] <- rVal
      if (is.numeric(x))
      {
        res[i,"point"] <- splits[i]
      }
      else
      {
        res[i,"point"] <- as.character(paste(splits[i]))
      }
      res[i,"ln"] <- ln
      res[i,"rn"] <- rn
    }
  }
  return( res )
}


SplitVar <- function( Y, x, parentVal, minobs, type ){
  s <- unique(x)
  if( length(x) == 1)
  {
    splits <- s
  }
  else
  {
    splits <- head( sort( s ), -1)
  }
  res <- SpliNum(Y, x, parentVal, splits, minobs, type)
  incl <- res$ln >= minobs & res$rn >= minobs & res$InfGain > 0
  res <- res[incl, , drop = F]
  best <- which.max(res$InfGain)
  res <- res[best, , drop = F]
  
  return( res )
}

FindBestSplit <- function(Y, X, data, parentVal, type, minobs){
  res <- sapply( X, function(i)
  {
    if (is.numeric(data[,i]))
    { 
      d <- data[,i]
    }
    else if(is.ordered(data[,i]))
    {
      d <- as.numeric(paste(data[,i]))
    }
    else
    {
      if(is.factor(data[,Y]))
      {
        temp <- data[data[,Y] == 1,]
        z <- prop.table(table(temp[,i]))
        z <- sort(z)
        d <- (paste(factor(data[,i], levels = names(z), ordered = TRUE)))
      }
      else 
      {
        z <- tapply(data[,Y], data[,i], mean)
        z <- sort(z)
        d <- (paste(factor(data[,i], levels = names(z), ordered = TRUE)))
      }
    }
    SplitVar(data[,Y], d, parentVal, minobs, type)
  }, simplify = F)
  
  res <- do.call("rbind", res)
  best <- which.max( res$InfGain)
  res <- res[best, , drop = F]
  
  return(res)
}


BuildTree <- function(node, Y, X, data, depth, type , minobs){
  node$Count <- nrow(data)
  
  if (is.factor(data[,Y])) 
  {
    node$Prob <- Prob(data[,Y])
    node$Class <- levels(data[,Y])[which.max(node$Prob)]
  }
  else
  {
    node$Prob <- SS(data[,Y])
    node$Class <- mean(data[,Y])
  }
  
  bestSplit <- FindBestSplit(Y, X, data, node$inf, type, minobs) 
  ifStop <- nrow(bestSplit) == 0
  
  if( ifStop| node$Depth == depth | all( node$Prob %in% c(0,1) ))
  {
    node$Leaf <- "*"
    return(node)
  }
  else
  {
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

PE <- function(p, n, z){
  return((p+(z^2)/(2*n)+z*sqrt(p/n-(p^2)/(n)+(z^2)/(4*n^2)))/(1+z^2/n))
}

PruneTree <- function(tree, cf){
  errcf <- qnorm(1 - cf)
  if( length( tree$Get("pathString") ) == 1 ) return( NULL )
  liscie_byly <- c()

  repeat{
    sciezka_lisci <- tree$Get("pathString", filterFun = isLeaf)
    if(all( sciezka_lisci %in% liscie_byly) | sciezka_lisci[1] == "Root") break
    temp <- strsplit(sciezka_lisci[ !sciezka_lisci %in% liscie_byly ][1], "/")[[1]]
    leaf <- eval(parse(text = paste("tree", paste0(paste0("'", temp[-1]), "'", collapse = "$"), sep = "$")))
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

    leaf_PE <- PE( 1 - max(leaf_prob), leaf_count, errcf)
    sibling_PE <- PE( 1 - max(sibling_prob), sibling_count, errcf)
    parent_PE <- PE( 1 - max(parent_prob), parent_count, errcf)

    if_prune <- parent_PE <= leaf_count / parent_count * leaf_PE + sibling_count / parent_count * sibling_PE

    if( if_prune & leaf_isLeaf == sibling_isLeaf)
    {
      parent$RemoveChild(sibling$name)
      parent$RemoveChild(leaf$name)
      parent$Leaf <- "*"
    }
    else
    {
      liscie_byly <- c(liscie_byly, leaf$pathString)
    }
  }
}


Tree <- function(Y, X, data, type, depth, minobs, overfit, cf){

  if(StopIfNot (Y=Y, X=X, data=data, type=type, depth=depth, minobs=minobs, overfit=overfit, cf=cf) == FALSE)
  {
    stop("Zle dane lub parametry wejsciowe!")
  }

  tree <- Node$new("Root")
  tree$Count <- nrow(data)

  AssignInfo(tree,Y,X,data,type,depth, minobs, overfit, cf)
  AssignInitialMeasures(tree, Y, data, type, depth)

  BuildTree(node = tree, Y = Y, X = X, data = data, depth = depth, type = type, minobs = minobs)

  if(overfit == 'prune')
  {
    PruneTree(tree, cf)
  }

  return(tree)
}



Prediction <- function(tree, obs){
  if(tree$isLeaf)
  {
    return(c(as.numeric(tree$Prob), "Class" = tree$Class))
  }
  
  if(is.numeric(tree$children[[1]]$BestSplit) | is.ordered(tree$children[[1]]$BestSplit))
  {
    child <- tree$children[[ifelse(obs[,tree$children[[1]]$feature] > (tree$children[[1]]$BestSplit), 2, 1)]]
  }
  else
  {
    split <- tree$children[[1]]$feature
    child <- tree$children[[ifelse((obs[,tree$children[[1]]$feature] %in% split), 1, 2)]]
  }
  return (Prediction(child,obs))
}


PredictTree <- function(tree, data){
  
  if(all(colnames(data) %in% attributes(tree)$X) == FALSE )
  {
    warning("'Y' lub 'X' brakuje w danych")
    return(NULL)
  }
  
  for(i in colnames(data))
  {
    if(is.factor(data[,i]) || is.ordered(data[,i]))
    {
      if(all(levels(data[,i]) %in% levels(attributes(tree)$data[,i])) == FALSE)
      {
        warning("Poziomy w danych do predykcji nie zgadzaja sie z danymi uczacymi")
        return(NULL)
      }
    }
  }
  
  if (is.factor(attributes(tree)$data[,attributes(tree)$Y]))
  {
    res <- c()
    for (i in 1:nrow(data))
    {
      res <- rbind(res, (Prediction(tree, data[i, ,drop = F])))
    }
  }
  else
  {
    res <- c()
    for (i in 1:nrow(data))
    {
      res[i] <- as.numeric(Prediction(tree, data[i, ,drop = F])["Class"])
    }
  }
  return (res)
}




# Sieci neuronowe


sigmoid <- function(x){
  return(1 / (1 + exp(-x)))
}

d_sigmoid <- function(x){
  return(x * ( 1 - x ) )
}

Relu<- function(x){
  return(ifelse( x <= 0, 0, x ) )
}

d_Relu <- function(x){
  return(ifelse( x <= 0, 0, 1 ) )
}

bladSS <- function(y_tar, y_hat){
  return(0.5 * sum((y_tar - y_hat )^2))
}

bladEntropia <- function(y_tar, y_hat){
  return(sum(( y_tar - log(y_hat))/nrow(y_tar)) )
}

SoftMax <- function(x){
  return(exp(x  / sum(exp(x))))
}

MinMax_NN <- function(x){
  return((x-min(x)) / (max(x)-min(x)))
}

MinMaxOdwrot <- function(x, y_min, y_max){
  return(x * (y_max - y_min) + y_min)
} 


propagacja_wprzod <- function(x, W_in, wagi_lista, W_out, typ){
  
  h_in <- cbind( matrix( 1, nrow = nrow(x)), sigmoid(x %*% W_in))
  
  if(length(wagi_lista[[1]]) > 1)
  {
    h_lista <- list()
    h_lista[[1]] <- cbind( matrix( 1, nrow = nrow(x)), sigmoid(h_in %*% (wagi_lista[[1]])))
    
    if(length(wagi_lista) > 1)
    {
      for (i in 2:length(wagi_lista))
      {
        h_lista[[i]] <- cbind( matrix( 1, nrow = nrow(x)), sigmoid(h_lista[[i-1]] %*% wagi_lista[[i]]))
      }
    }
    
    if (typ =="bin")
    {
      y_hat <- sigmoid( h_lista[[length(h_lista)]] %*% W_out )                                          # klasyfikacja binarna
    }
    else if (typ == "multi")
    {
      y_hat <- matrix( t( apply( h_lista[[length(h_lista)]] %*% W_out, 1, SoftMax ) ), nrow = nrow(x) ) # klasyfikacja wieloklasowa
    } 
    else if (typ == "reg")
    {
      y_hat <- Relu(h_lista[[length(h_lista)]] %*% W_out)                                               # regresja
    }
  }
  else
  {
    h_lista <- list(0)
    
    if (typ =="bin")
    {
      y_hat <- sigmoid( h_in %*% W_out )                                          # klasyfikacja binarna
    }
    else if (typ == "multi")
    {
      y_hat <- matrix( t( apply( h_in %*% W_out, 1, SoftMax ) ), nrow = nrow(x) ) # klasyfikacja wieloklasowa
    } 
    else if (typ == "reg")
    {
      y_hat <- (h_in %*% W_out)                                                   # regresja
    }
  }
  
  return(list(h_in = h_in, h_lista = h_lista, y_hat = y_hat))
}


propagacja_wstecz <- function(x, y_tar, y_hat, h_in, h_lista, W_in, wagi_lista, W_out, lr, typ){
  
  if (typ =="bin"){
    dy_hat <- (y_tar - y_hat) * d_sigmoid(y_hat)        # klasyfikacja binarna
  }
  else if (typ == "multi"){
    dy_hat <- (y_tar - y_hat) / nrow(x)                 # klasyfikacja wieloklasowa
  } 
  else if (typ == "reg"){
    dy_hat <- (y_tar - y_hat)                           # regresja
  }
  
  if(length(wagi_lista[[1]]) > 1){
    dW_out <- t(h_lista[[length(h_lista)]]) %*% dy_hat
    dz <- list()
    dw_lista <- list()  
    
    if(length(h_lista) == 1)
    {
      dz[[(length(h_lista)+1)]] <- dy_hat %*% t(W_out) * d_sigmoid(h_lista[[length(h_lista)]])
      dw_lista[[(length(h_lista)+1)]] <- t(h_in) %*% dz[[(length(h_lista)+1)]][,-1]
    }
    else
    {
      dz[[(length(h_lista)+1)]] <- dy_hat %*% t(W_out) * d_sigmoid(h_lista[[length(h_lista)]])
      dw_lista[[(length(h_lista)+1)]] <- t(h_lista[[length(h_lista)-1]]) %*% dz[[(length(h_lista)+1)]][,-1]
    }
    
    if(length(h_lista) > 1)
    {
      if(length(h_lista) > 2)
      {
        for (i in (length(h_lista)):3)
        {
          dz[[i]] <- dz[[i+1]][,-1] %*% t(wagi_lista[[i]]) * d_sigmoid(h_lista[[i-1]])
          dw_lista[[i]] <- t(h_lista[[i-2]]) %*% dz[[i]][,-1]
        }
      }
      dz[[2]] <- dz[[3]][,-1] %*% t(wagi_lista[[2]]) * d_sigmoid(h_lista[[1]])
      dw_lista[[2]] <- t(h_in) %*% dz[[2]][,-1]
    }
    dz[[1]] <- dz[[2]][,-1] %*% t(wagi_lista[[1]]) * d_sigmoid(h_in)
    dw_lista[[1]] <- t(x) %*% dz[[1]][,-1]
    
    W_in <- (lr * dw_lista[[1]]) + W_in
    
    for (w in 1:length(wagi_lista))
    {
      wagi_lista[[w]] <- (lr * dw_lista[[w+1]]) + wagi_lista[[w]]
    }
    
    W_out <- (lr * dW_out) + W_out
  }
  else
  {
    dW_out <- t(h_in) %*% dy_hat
    dz_in <-  dy_hat %*% t(W_out) * d_sigmoid(h_in)
    dW_in <- t(x) %*% dz_in[,-1]
    W_in <- (lr * dW_in) + W_in
    W_out <- (lr * dW_out) + W_out
  }
  return(list(W_in = W_in, wagi_lista = wagi_lista, W_out = W_out))
}



trainNN <- function( Yname, Xnames, data, h, lr, iter, seed, typ){
  set.seed(seed)
  h <- unlist(h, use.names = FALSE)
  y_tar <- as.matrix(data[,Yname])
  x <- cbind(rep(1, nrow(data[,Xnames]) ), data[,Xnames])
  
  W_in <- matrix(runif(ncol(x) * h[1], -1, 1), nrow = ncol(x))
  wagi_lista <- list()
  W_out <- matrix(runif((h[length(h)] + 1)* ncol(y_tar), -1, 1), nrow = h[length(h)] + 1 )
  
  if(length(h) > 1)
  {
    for (i in 1:(length(h)-1))
    {
      wagi_lista[[i]] <- matrix(runif((h[i]+1)*h[i+1], -1, 1), nrow = 1+h[i])
    }
  }
  else
  {
    wagi_lista[[1]] <- 0
  }
  
  # error <- double(iter)
  
  for(i in 1:iter)
  {
    propagacja_wprzod_list <- propagacja_wprzod(x, W_in, wagi_lista, W_out, typ)
    propagacja_wstecz_list <- propagacja_wstecz(x, y_tar, y_hat = propagacja_wprzod_list$y_hat, h_in = propagacja_wprzod_list$h_in, h_lista = propagacja_wprzod_list$h_lista, W_in, wagi_lista, W_out, lr, typ)
    
    W_in <- propagacja_wstecz_list$W_in
    wagi_lista <- propagacja_wstecz_list$wagi_lista
    W_out <- propagacja_wstecz_list$W_out

    # if(typ == "bin" || typ == "multi")
    # {
    #   error[i] <- bladEntropia(y_tar = y_tar, y_hat = propagacja_wprzod_list$y_hat)
    # }
    # else
    # {
    #   error[i] <- bladSS(y_tar = y_tar, y_hat = propagacja_wprzod_list$y_hat )
    # }
    
    cat(paste( "\rIteracja: ", i , "/", iter))
  }
  # x_pomocnicze <- seq(1, iter)
  # print(qplot(x_pomocnicze, error[x_pomocnicze], geom = "line", main = "Error", xlab = "Iteracje"))
  
  return(list(y_hat = propagacja_wprzod_list$y_hat, W_in = W_in, wagi_lista = wagi_lista, W_out = W_out))
}



predNN <- function(x, NN, typ){
  
  x <- cbind(rep(1, nrow(x)), x)
  h1 <- cbind(matrix(1, nrow = nrow(x)), sigmoid(x %*% NN$W_in))
  
  if(length(NN$wagi_lista[[1]]) > 1){
    h2 <- list()
    h2[[1]] <- cbind(matrix(1, nrow = nrow(x)), sigmoid(h1 %*% NN$wagi_lista[[1]]))
    
    if(length(NN$wagi_lista) > 1)
    {
      for (i in 2:length(NN$wagi_lista))
      {
        h2[[i]] <- cbind(matrix(1, nrow = nrow(x)), sigmoid(h2[[i-1]] %*% NN$wagi_lista[[i]]))
      }
    }
    
    if (typ =="bin")
    {
      y_hat <- sigmoid( h2[[length(NN$wagi_lista)]] %*% NN$W_out)                                         # klasyfikacja binarna
    }
    else if (typ == "multi")
    {
      y_hat <- matrix(t(apply( h2[[length(NN$wagi_lista)]] %*% NN$W_out, 1, SoftMax)), nrow = nrow(x))    # klasyfikacja wieloklasowa
    } 
    else if (typ == "reg")
    {
      y_hat <- h2[[length(NN$wagi_lista)]] %*% NN$W_out                                                   # regresja
    }
  }
  else
  {
    y_hat <- sigmoid(h1 %*% NN$W_out)
    
    if (typ =="bin")
    {
      y_hat <- sigmoid( h1 %*% NN$W_out)                                          # klasyfikacja binarna
    }
    else if (typ == "multi")
    {
      y_hat <- matrix(t(apply( h1 %*% NN$W_out, 1, SoftMax)), nrow = nrow(x))     # klasyfikacja wieloklasowa
    } 
    else if (typ == "reg")
    {
      y_hat <- h1 %*% NN$W_out                                                    # regresja
    }
  }
  
  return(y_hat)
}





# Statystyka Modeli i CrossValidTune


MSE <- function(y_tar, y_hat){
  return(mean(y_tar-y_hat)^2)
}

MAE <- function(y_tar, y_hat){
  return(mean(abs(y_tar - y_hat)))
}

MAPE <- function(y_tar, y_hat){
  return(mean(abs((y_tar-y_hat)/y_tar)) * 100)
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


Sensitivity <- function(Mat)
{
  return(round((Mat[4] / (Mat[4] + Mat[2])),4))
}

Specificity <- function(Mat)
{
  return(round((Mat[1] / (Mat[1] + Mat[3])),4))
}

Accuracy <- function(Mat)
{
  return(round(((Mat[1] + Mat[4]) / (Mat[1] + Mat[2] + Mat[3] + Mat[4])),4))
}


Accuracy__ <- function(y_tar, y_hat){
  return( sum(as.numeric(y_tar) == as.numeric(y_hat)) / length(y_tar) )
}


ModelOcena <- function(y_tar, y_hat)
{
  
  if(is.numeric(y_tar))
  {
    regresja <- c("MAE" = MAE(y_tar, y_hat), "MSE" = MSE(y_tar, y_hat), "MAPE" = MAPE(y_tar, y_hat))
    return(regresja)
  }
  else if(is.factor(y_tar) & nlevels(y_tar) == 2)
  {
    if(length(y_tar) == 1)
    {
      Acc <- Accuracy__(y_tar, y_hat = ifelse(y_hat < 0.5, 0, 1))
      miary <- c("AUC" = 0, "Sensitivity" = 0, "Specificity" = 0, "Accuracy" = Acc)
      return(miary)
    }
    else
    {
      Mat <- table(y_tar, y_hat = ifelse(y_hat <= Youden(y_tar, y_hat), 0, 1))
      miary <- c( "AUC" = AUC(y_tar, y_hat), "Sensitivity" = Sensitivity(Mat), "Specificity" = Specificity(Mat), "Accuracy" = Accuracy(Mat))
      return(miary)
    }
    
  }
  else if(is.factor(y_tar) & nlevels(y_tar) > 2)
  {
    multi <- c("Accuracy" = Accuracy__(y_tar, y_hat))
    return(multi)
  }
  else
  {
    print("Niepoprawne dane")
  }
}





CrossValidTune <- function(dane, X, Y, kFold, parTune, algorytm, seed = 123)
{
  set.seed(seed)

  if(is.numeric(dane[,Y]))
  {
    typ = "reg"
  }
  else if(is.factor(dane[,Y]))
  {
    if (nlevels(dane[,Y]) == 2)
    {
      typ = "bin"
    }
    else if(nlevels(dane[,Y]) > 2)
    {
      typ = "multi"
    }
  }
  
  tabela <- as.data.frame(expand_grid(nr=c(1:kFold), parTune))
  
  print(paste0("Model: ", algorytm))
  print(paste0("Dane: ", typ))
  print(paste0("Ilosc modeli do nauczenia: ", nrow(tabela)))
    
  
  
  ile_danych = nrow(dane)
  tabela_podzialu <- data.frame(matrix(ncol = kFold, nrow = nrow(dane)))

  for(i in 1:kFold)
  { 
    id_trening <- sample( 1:ile_danych, size = (1/kFold * ile_danych)-1, replace = F )
    tabela_podzialu[id_trening,i] <- 2
    tabela_podzialu[-id_trening,i] <- 1
  }
  
  
  # KNN
  
  if(algorytm == "KNN")
  {
    cat("Model w trakcie uczenia: ")
    
    if(typ == "bin")
    {
      tabela_bin <- data.frame(tabela, AUCW=0, SensitivityW=0, SpecificityW=0, AccuracyW=0)
      
      nCores <- detectCores()
      klaster <- makeCluster(nCores-1)
      registerDoParallel(klaster)

      wynik <- foreach(nr_modelu = 1:nrow(tabela_bin), .export = c("X", "Y", "dane", "tabela_bin", "KNNtrain", "KNNpred", "ModelOcena", "MinMax", "AUC", "Youden", "Sensitivity", "Specificity", "Accuracy", "Accuracy__", "d_euklides", "d_hamming", "d_porzadkowa"), .combine = rbind) %dopar%
      # for(nr_modelu in 1:nrow(tabela_bin))
      {
        cat(paste0(nr_modelu,">"))
        dane_treningowe <- dane[tabela_podzialu[,tabela_bin$nr[nr_modelu]] == 1,]
        dane_walidacyjne <- dane[tabela_podzialu[,tabela_bin$nr[nr_modelu]] == 2,]
        
        KNN_Model <- KNNtrain(dane_treningowe[,X], dane_treningowe[,Y], tabela_bin$k[nr_modelu], 0, 1) 

        KNN_pred_Walid <- KNNpred(KNN_Model, X=dane_walidacyjne[,X])
        dane_walidacyjne_ocena = ModelOcena((dane_walidacyjne[,Y]), as.numeric(1-KNN_pred_Walid[,1]))
        tabela_bin[nr_modelu, "AUCW"] <- dane_walidacyjne_ocena["AUC"]
        tabela_bin[nr_modelu, "SensitivityW"] <- dane_walidacyjne_ocena["Sensitivity"]
        tabela_bin[nr_modelu, "SpecificityW"] <- dane_walidacyjne_ocena["Specificity"]
        tabela_bin[nr_modelu, "AccuracyW"] <- dane_walidacyjne_ocena["Accuracy"]
        
        return(tabela_bin[nr_modelu,])
      }
      stopCluster(klaster)
      # print(wynik)
      
      tabela_bin <- wynik
      
      return(tabela_bin)
    }
    else if(typ == "multi")
    {
      tabela_multi <- data.frame(tabela, ACCW=0)
      
      nCores <- detectCores()
      klaster <- makeCluster(nCores-1)
      registerDoParallel(klaster)
      
      wynik <- foreach(nr_modelu = 1:nrow(tabela_multi), .export = c("X", "Y", "dane", "tabela_multi", "KNNtrain", "KNNpred", "ModelOcena", "MinMax", "AUC", "Youden", "Sensitivity", "Specificity", "Accuracy", "Accuracy__", "d_euklides", "d_hamming", "d_porzadkowa"), .combine = rbind) %dopar%
      # for(nr_modelu in 1:nrow(tabela_multi))
      {
        cat(paste0(nr_modelu,">"))
        dane_treningowe <- dane[tabela_podzialu[,tabela_multi$nr[nr_modelu]] == 1,]
        dane_walidacyjne <- dane[tabela_podzialu[,tabela_multi$nr[nr_modelu]] == 2,]
        
        KNN_Model <- KNNtrain(dane_treningowe[,X], dane_treningowe[,Y], tabela_multi$k[nr_modelu], 0, 1) 
        
        KNN_pred_Walid <- KNNpred(KNN_Model, X=dane_walidacyjne[,X])
        tabela_multi[nr_modelu, "ACCW"] = ModelOcena((dane_walidacyjne[,Y]), as.numeric(KNN_pred_Walid[,"Klasa"]))
        
        return(tabela_multi[nr_modelu,])
      }
      stopCluster(klaster)
      # print(wynik)
      
      tabela_multi <- wynik
      
      return(tabela_multi)
    }
    else if(typ == "reg")
    {
      tabela_reg <- data.frame(tabela, MAEW=0, MSEW=0, MAPEW=0)

      nCores <- detectCores()
      klaster <- makeCluster(nCores-1)
      registerDoParallel(klaster)
      
      wynik <- foreach(nr_modelu = 1:nrow(tabela_reg), .export = c("X", "Y", "dane", "tabela_reg", "KNNtrain", "KNNpred", "ModelOcena", "MinMax", "MAE", "MSE", "MAPE", "d_euklides", "d_hamming", "d_porzadkowa"), .combine = rbind) %dopar%
      # for(nr_modelu in 1:nrow(tabela_reg))
      {
        cat(paste0(nr_modelu,">"))
        dane_treningowe <- dane[tabela_podzialu[,tabela_reg$nr[nr_modelu]] == 1,]
        dane_walidacyjne <- dane[tabela_podzialu[,tabela_reg$nr[nr_modelu]] == 2,]
        
        KNN_Model <- KNNtrain(dane_treningowe[,X], dane_treningowe[,Y], tabela_reg$k[nr_modelu], 0, 1) 

        KNN_pred_Walid <- KNNpred(KNN_Model, X=dane_walidacyjne[,X])
        dane_walidacyjne_ocena <- ModelOcena(dane_walidacyjne[,Y], KNN_pred_Walid)
        tabela_reg[nr_modelu, "MAEW"] <- dane_walidacyjne_ocena["MAE"]
        tabela_reg[nr_modelu, "MSEW"] <- dane_walidacyjne_ocena["MSE"]
        tabela_reg[nr_modelu, "MAPEW"] <- dane_walidacyjne_ocena["MAPE"]
        
        return(tabela_reg[nr_modelu,])
      }
      stopCluster(klaster)
      # print(wynik)
      
      tabela_reg <- wynik
      
      return(tabela_reg)
    }
  }


  
  # Drzewa decyzyjne
  
  
  if(algorytm == "Tree")
  {
    cat("Model w trakcie uczenia: ")
    
    if(typ == "bin")
    {
      tabela_bin <- data.frame(tabela, AUCW=0, SensitivityW=0, SpecificityW=0, AccuracyW=0)
      
      for(nr_modelu in 1:nrow(tabela_bin))
      {
        cat(paste0(nr_modelu,">"))
        
        dane_treningowe <- dane[tabela_podzialu[,tabela_bin$nr[nr_modelu]] == 1,]
        dane_walidacyjne <- dane[tabela_podzialu[,tabela_bin$nr[nr_modelu]] == 2,]

        Tree_Model <- Tree(Y, X, dane_treningowe, type = tabela_bin$type[nr_modelu], depth =  tabela_bin$depth[nr_modelu], minobs =  tabela_bin$minobs[nr_modelu], overfit =  tabela_bin$overfit[nr_modelu], cf = tabela_bin$cf[nr_modelu]) 
        
        Tree_pred_Walid <- PredictTree(Tree_Model, dane_walidacyjne[,X])
        dane_walidacyjne_ocena = ModelOcena((dane_walidacyjne[,Y]), as.numeric(Tree_pred_Walid[,2]))
        tabela_bin[nr_modelu, "AUCW"] <- dane_walidacyjne_ocena["AUC"]
        tabela_bin[nr_modelu, "SensitivityW"] <- dane_walidacyjne_ocena["Sensitivity"]
        tabela_bin[nr_modelu, "SpecificityW"] <- dane_walidacyjne_ocena["Specificity"]
        tabela_bin[nr_modelu, "AccuracyW"] <- dane_walidacyjne_ocena["Accuracy"]
      }
      return(tabela_bin)
    }
    else if(typ == "multi")
    {
      tabela_multi <- data.frame(tabela, ACCW=0)
      
      for(nr_modelu in 1:nrow(tabela_multi))
      {
        cat(paste0(nr_modelu,">"))
        
        dane_treningowe <- dane[tabela_podzialu[,tabela_multi$nr[nr_modelu]] == 1,]
        dane_walidacyjne <- dane[tabela_podzialu[,tabela_multi$nr[nr_modelu]] == 2,]

        Tree_Model <- Tree(Y, X, dane_treningowe, type = tabela_multi$type[nr_modelu], depth =  tabela_multi$depth[nr_modelu], minobs =  tabela_multi$minobs[nr_modelu], overfit =  tabela_multi$overfit[nr_modelu], cf = tabela_multi$cf[nr_modelu]) 
        
        Tree_pred_Walid <- PredictTree(Tree_Model, dane_walidacyjne[,X])
        tabela_multi[nr_modelu, "ACCW"] <- ModelOcena(dane_walidacyjne[,Y], factor(Tree_pred_Walid[,"Class"], levels = levels(dane[,Y])))
      }
      return(tabela_multi)
    }
    else if(typ == "reg")
    {
      tabela_reg <- data.frame(tabela, MAEW=0, MSEW=0, MAPEW=0)
      
      for(nr_modelu in 1:nrow(tabela_reg))
      {
        cat(paste0(nr_modelu,">"))
        
        dane_treningowe <- dane[tabela_podzialu[,tabela_reg$nr[nr_modelu]] == 1,]
        dane_walidacyjne <- dane[tabela_podzialu[,tabela_reg$nr[nr_modelu]] == 2,]
        
        Tree_Model <- Tree(Y, X, dane_treningowe, type = tabela_reg$type[nr_modelu], depth =  tabela_reg$depth[nr_modelu], minobs =  tabela_reg$minobs[nr_modelu], overfit =  tabela_reg$overfit[nr_modelu], cf = tabela_reg$cf[nr_modelu]) 

        ocena_Walidacja <- ModelOcena(dane_walidacyjne[,Y], PredictTree(Tree_Model, dane_walidacyjne[,X]))
        tabela_reg[nr_modelu, "MAEW"] <- ocena_Walidacja["MAE"]
        tabela_reg[nr_modelu, "MSEW"] <- ocena_Walidacja["MSE"]
        tabela_reg[nr_modelu, "MAPEW"] <- ocena_Walidacja["MAPE"]
      }
      return(tabela_reg)
    }
  }
  

  
  
  # Sieci Neuronowe
  
  
  if(algorytm == "NN")
  {
    cat("Model w trakcie uczenia: ")

    if(typ == "bin")
    {
      tabela_bin <- data.frame(tabela, AUCW=0, SensitivityW=0, SpecificityW=0, AccuracyW=0)

      for(nr_modelu in 1:nrow(tabela_bin))
      {
        cat(paste0("\t ", nr_modelu,">"))
        
        dane_bin_NN <- dane
        dane_bin_NN[,X] <- sapply(dane_bin_NN[,X], MinMax_NN)
        dane_treningowe <- dane_bin_NN[tabela_podzialu[,tabela_bin$nr[nr_modelu]] == 1,]
        dane_treningowe[,Y] <- MinMax_NN(as.numeric(dane_treningowe[,Y]))
        dane_treningowe <- as.matrix(dane_treningowe)
        dane_walidacyjne <- dane_bin_NN[tabela_podzialu[,tabela_bin$nr[nr_modelu]] == 2,]
        dane_walidacyjne_Y <- dane_walidacyjne[,Y]
        dane_walidacyjne[,Y] <- MinMax_NN(as.numeric(dane_walidacyjne[,Y]))
        dane_walidacyjne <- as.matrix(dane_walidacyjne)

        NN_Model <- trainNN( Y, X, data = dane_treningowe, h = tabela_bin$h[nr_modelu], lr = tabela_bin$lr[nr_modelu], iter = tabela_bin$iter[nr_modelu], seed = 357, typ = typ)

        NN_dane_walidacyjne_p <- predNN(dane_walidacyjne[,X], NN_Model, typ = typ)
        dane_walidacyjne_ocena = ModelOcena(dane_walidacyjne_Y, NN_dane_walidacyjne_p[,1])
        tabela_bin[nr_modelu, "AUCW"] <- dane_walidacyjne_ocena["AUC"]
        tabela_bin[nr_modelu, "SensitivityW"] <- dane_walidacyjne_ocena["Sensitivity"]
        tabela_bin[nr_modelu, "SpecificityW"] <- dane_walidacyjne_ocena["Specificity"]
        tabela_bin[nr_modelu, "AccuracyW"] <- dane_walidacyjne_ocena["Accuracy"]
      }
      return(tabela_bin)
    }
    else if(typ == "multi")
    {
      tabela_multi <- data.frame(tabela, ACCW=0)
      
      for(nr_modelu in 1:nrow(tabela_multi))
      {
        cat(paste0("\t ", nr_modelu,">"))
        
        dane_multi_NN <- dane
        dane_multi_NN[,X] <- sapply(dane_multi_NN[,X], MinMax_NN)
        dane_treningowe <- dane_multi_NN[tabela_podzialu[,tabela_multi$nr[nr_modelu]] == 1,]
        dane_treningowe_Y_NN <- model.matrix( ~ dane_treningowe[,Y] - 1, dane_treningowe)
        dane_treningowe_Y_name <- colnames(dane_treningowe_Y_NN)
        dane_treningowe_data <- as.matrix(cbind(dane_treningowe[,X], dane_treningowe_Y_NN))
        dane_walidacyjne <- dane_multi_NN[tabela_podzialu[,tabela_multi$nr[nr_modelu]] == 2,]
        
        NN_Model <- trainNN(dane_treningowe_Y_name, X, data = dane_treningowe_data, h = tabela_multi$h[nr_modelu], lr = tabela_multi$lr[nr_modelu], iter = tabela_multi$iter[nr_modelu], seed = 123, typ = typ)

        NN_dane_walidacyjne_p <- predNN(as.matrix(dane_walidacyjne[,X]), NN_Model, typ = typ)
        NN_dane_walidacyjne_p_multi <- factor(levels(dane_multi_NN[,Y])[apply( NN_dane_walidacyjne_p, 1, which.max )], levels = levels(dane_multi_NN[,Y]))
        tabela_multi[nr_modelu, "ACCW"] <- ModelOcena(dane_walidacyjne[,Y], NN_dane_walidacyjne_p_multi)
      }
      return(tabela_multi)
    }
    else if(typ == "reg")
    {
      tabela_reg <- data.frame(tabela, MAEW=0, MSEW=0, MAPEW=0)
      
      for(nr_modelu in 1:nrow(tabela_reg))
      {
        cat(paste0("\t ", nr_modelu,">"))
        
        dane_reg_NN <- sapply(dane, MinMax_NN)
        dane_treningowe <- as.matrix(dane_reg_NN[tabela_podzialu[,tabela_reg$nr[nr_modelu]] == 1,])
        dane_walidacyjne <- as.matrix(dane_reg_NN[tabela_podzialu[,tabela_reg$nr[nr_modelu]] == 2,])
        
        NN_Model <-  trainNN(Y, X, data = dane_treningowe, h = tabela_reg$h[nr_modelu], lr = tabela_reg$lr[nr_modelu], iter = tabela_reg$iter[nr_modelu], seed = 123, typ = typ)
        
        NN_dane_walidacyjne_p <- predNN(dane_walidacyjne[,X], NN_Model, typ = typ)
        NN_dane_walidacyjne_p_reg <- MinMaxOdwrot(NN_dane_walidacyjne_p[,1], y_min = min(dane[,Y]), y_max = max(dane[,Y]))
        dane_walidacyjne_ocena <- ModelOcena(dane[tabela_podzialu[,tabela_reg$nr[nr_modelu]] == 2,Y], NN_dane_walidacyjne_p_reg)
        tabela_reg[nr_modelu, "MAEW"] <- dane_walidacyjne_ocena["MAE"]
        tabela_reg[nr_modelu, "MSEW"] <- dane_walidacyjne_ocena["MSE"]
        tabela_reg[nr_modelu, "MAPEW"] <- dane_walidacyjne_ocena["MAPE"]
      }
      return(tabela_reg)
    }
  }
}
  
