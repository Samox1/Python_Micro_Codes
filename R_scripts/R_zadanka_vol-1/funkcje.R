library(data.tree)
library(tidyverse)
library(pROC)



### KNN ###



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
    stop("Dane nie sa macierza lub ramka danych!")
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





### Drzewa decyzyjne ###

StopIfNot <- function( Y, X, data, type, depth, minobs, overfit, cf){
  
  if (!is.data.frame(data)){
    print("Typem parametru data nie jest ramka danych.")
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

AssignInfo <- function(tree,Y,X,data,type,depth, minobs, overfit, cf )
{
  tree$Y <- data[,Y]
  tree$X <- data[,X]
  tree$data <- data
  tree$type <- type
  tree$Depth <- depth
  tree$minobs <- minobs
  tree$overfit <- overfit
  tree$cf <- cf
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


Tree<- function(Y, X, data, type, depth, minobs, overfit ,cf){
  
  if(StopIfNot(Y, X, data, type, depth, minobs, overfit,cf) == FALSE){
    return(FALSE)
  }
  
  for (i in 1:length(X)) { 
    if (is.factor(data[,i])) {
      data[,i] <- as.character(data[,i])
    }
    else {
      next
    }
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
  
  PruneTree(tree, cf)
  
  AssignInfo(tree,Y,X,data,type,depth, minobs, overfit, cf)
  
  return(tree)
}


ObsPred <- function(tree, obs) {
  if (tree$isLeaf) {
    #print(tree$Prob)
    return(data.frame("Prob" = max(tree$Prob), "Class" = (tree$Class), stringsAsFactors = F))}
  
  if (is.numeric(tree$children[[1]]$BestSplit) | is.ordered(tree$children[[1]]$BestSplit)) {
    child <- tree$children[[ifelse(obs[,tree$children[[1]]$feature] > (tree$children[[1]]$BestSplit), 2, 1)]]}
  else {
    split <- tree$children[[1]]$feature
    child <- tree$children[[ifelse((obs[,tree$children[[1]]$feature] %in% split), 1, 2)]]}
  return (ObsPred(child,obs))
}


PredictTree <- function(tree, data) {
  if (is.factor(attributes(tree)$Y)) {
    res <- data.frame(matrix(0, nrow = nrow(data), ncol = 2))
    
    for (i in 1:nrow(data)) {
      res[i,] <- (ObsPred(tree, data[i, ,drop = F]))}
    colnames(res) <- c("Prob", "Class")
  }
  else{
    res <- c()
    for (i in 1:nrow(data)) {
      res[i] <- (ObsPred(tree, data[i, ,drop = F])$Class)}
  }
  return (res)
}




### Sieci neuronowe ###


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
MinMax_nn <- function( x ){
  return( ( x - min(x) ) / ( max(x) - min(x) ) )
}  
MinMaxOdwrot <- function( x, y_min, y_max ){
  return(  x * (y_max - y_min) + y_min )
} 


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




### Statystyka Modeli i Kroswalidacja ###

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
  krzywa_roc <- roc(y_tar, y_hat)
  czulosc <- rev(krzywa_roc$sensitivities)
  specyficznosc <- rev(1 - krzywa_roc$specificities)
  czulosc_op <- c(diff(czulosc), 0)
  specyficznosc_op <- c(diff(specyficznosc), 0)
  wynik <- sum(czulosc * specyficznosc_op) + sum(czulosc_op * specyficznosc_op)/2
  return(round(wynik,4))
}


Youden <- function(y_tar, y_hat)
{
  krzywa_roc <- roc(y_tar, y_hat)
  czulosc <- krzywa_roc$sensitivities 
  specyficznosc <- krzywa_roc$specificities 
  max <- 0
  for (i in 1:length(czulosc))
  {
    J <- (czulosc[i] + specyficznosc[i]-1)
    if(J > max)
    {
      max=J
    }
  }
  return(max)
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

Accuracy_multi <- function(y_tar, y_hat){
  return( sum(y_tar == y_hat) / length(y_tar) )
}

ModelOcena <- function(y_tar, y_hat)
{
  Mat <- table(y_tar, y_hat = ifelse(y_hat <= Youden(y_tar, y_hat), 0, 1))
  if(is.numeric(y_tar))
  {
    regresja <- c("MAE" = MAE(y_tar, y_hat), "MSE" = MSE(y_tar, y_hat), "MAPE" = MAPE(y_tar, y_hat))
    return(regresja)
  }
  else if(is.factor(y_tar))
  {
    miary <- c( "AUC" = AUC(y_tar, y_hat), "Czulosc" = Czulosc(Mat), "Specyficznosc" = Specyficznosc(Mat), "Jakosc" = Jakosc(Mat))
    klasyfikacja <- list(Mat, Youden(y_tar, y_hat), miary)
    return(klasyfikacja)
  }
  else
  {
    print("Niepoprawne dane")
  }
}


CrossValidTune <- function(dane, X, Y, kFold = 3, parTune, algorytm="KNN", seed = 123)
{
  set.seed(seed)
  
  dl_wektora = nrow(dane)
  podzial_zbioru <- data.frame()
  
  ramka <- as.data.frame(expand_grid(k_=c(1:kFold), parTune))
  
  # tabela z podzialem danych na treningowe i walidacyjne
  for(i in 1:kFold)
  { 
    id_trening <- sample( 1:dl_wektora, size = (1/kFold * dl_wektora)-1, replace = F )
    podzial_zbioru[id_trening,i] <- 1
    podzial_zbioru[-id_trening,i] <- 2
  }
  
  # jakiego typu klasyfikacja czy regresja
  if(is.numeric(dane[,Y]))
  {
    typ = "reg"
  }
  else if(is.factor(dane[,Y]))
  {
    if (length(levels(dane[,Y])) == 2)
    {
      typ = "bin"
    }
    else if(length(levels(dane[,Y])) > 2)
    {
      typ = "multi"
    }
  }
  
  # print(ramka)
  
  # for(id_modele in 1:nrow(ramka))
  # {
  #   dane_treningowe <- dane[podzial_zbioru[,ramka$k_[id_modele]] == 1,]
  #   dane_walidacyjne <- dane[podzial_zbioru[,ramka$k_[id_modele]] == 2,]
  #   
  #   Drzewo <- Tree(Y, X, dane_treningowe, type = ramka$type[id_modele], depth = ramka$depth[id_modele], minobs = ramka$minobs[id_modele], overfit = 'none', cf = ramka$cf[id_modele])
  #   Test_treningowy <- PredictTree(Drzewo, dane_treningowe[,X])
  #   Test_walidacyjny <- PredictTree(Drzewo, dane_walidacyjne[,X])
  #   
  #   print(ModelOcena(dane_treningowe[,Y], Test_treningowy))
  #   
  # }
  
  print("*** Podzial danych zrobiony ***")
  
  ### KNN ###
  
  if(algorytm == "KNN")
  {
    if(typ == "bin")
    {
      ramka_bin <- data.frame(ramka, AUCT=0, CzuloscT=0, SpecyficznoscT=0, JakoscT=0, AUCW=0, CzuloscW=0, SpecyficznoscW=0, JakoscW=0)
      print(ramka_bin)
      
      for(id_modele in 1:nrow(ramka_bin))
      {
        print(id_modele)
        
        dane_treningowe <- dane[podzial_zbioru[,ramka_bin$k_[id_modele]] == 1,]
        dane_walidacyjne <- dane[podzial_zbioru[,ramka_bin$k_[id_modele]] == 2,]
        
        print("Dane rozdzielone")
        
        KNN_Model <- KNNtrain(dane_treningowe[,X], dane_treningowe[,Y], ramka_bin$k[id_modele], 0, 1) 
        
        print("KNN Model nauczony")
        
        KNN_pred_Trening <- KNNpred(KNN_Model, X=dane_treningowe[,X])
        KNN_pred_Walid <- KNNpred(KNN_Model, X=dane_walidacyjne[,X])
        
        print("KNN Model Predykcja osiagnieta")
        
        Trening_Ocena = ModelOcena(as.numeric(dane_treningowe[,Y]), as.numeric(KNN_pred_Trening$Klasa))
        Walidacja_Ocena = ModelOcena(as.numeric(dane_walidacyjne[,Y]), as.numeric(KNN_pred_Walid$Klasa))

        ramka_bin$AUCT[id_modele] <- Trening_Ocena[[3]]["AUC"]
        ramka_bin$CzuloscT[id_modele] <- Trening_Ocena[[3]]["Czulosc"]
        ramka_bin$SpecyficznoscT[id_modele] <- Trening_Ocena[[3]]["Specyficznosc"]
        ramka_bin$JakoscT[id_modele] <- Trening_Ocena[[3]]["Jakosc"]

        ramka_bin$AUCT[id_modele] <- Trening_Ocena[[3]]["AUC"]
        ramka_bin$CzuloscT[id_modele] <- Trening_Ocena[[3]]["Czulosc"]
        ramka_bin$SpecyficznoscT[id_modele] <- Trening_Ocena[[3]]["Specyficznosc"]
        ramka_bin$JakoscT[id_modele] <- Trening_Ocena[[3]]["Jakosc"]

        
      }
      
      return(ramka_bin)
      
    }
    else if(typ == "multi")
    {
      ramka_multi <- data.frame(ramka, ACCT=0, ACCW=0)
      
      for(id_modele in 1:nrow(ramka_multi))
      {
        dane_treningowe <- dane[podzial_zbioru[,ramka_multi$k_[id_modele]] == 1,]
        dane_walidacyjne <- dane[podzial_zbioru[,ramka_multi$k_[id_modele]] == 2,]
        
        KNN_Model <- KNNtrain(dane_treningowe[,X], dane_treningowe[,Y], ramka_multi$k[id_modele], 0, 1) 
        
        KNN_pred_Trening <- KNNpred(KNN_Model, X=dane_treningowe[,X])
        KNN_pred_Walid <- KNNpred(KNN_Model, X=dane_walidacyjne[,X])
        
        ramka_multi$ACCT[id_modele] <- Accuracy_multi(as.numeric(dane_treningowe[,Y]), KNN_pred_Trening$Klasa)
        ramka_multi$ACCW[id_modele] <- Accuracy_multi(as.numeric(dane_walidacyjne[,Y]), KNN_pred_Walid$Klasa)
      }
      
      return(ramka_multi)
    }
    else if(typ == "reg")
    {
      
    }
    
    
  }

  
  
  ### Drzewa decyzyjne ###
  
  
  ### Sieci Neuronowe ###
  
  
  
  # if(is.numeric(dane[,Y]))
  # {
  #   regresja <- data.frame(parTune, MAET=0, MSET=0, MAPET=0, MAEW=0, MSEW=0, MAPEW=0  )
  #   # regresja %>% group_by(colnames(parTune)) %>% summarise_at(vars(contains("T", "W")), mean)
  #   return(regresja)
  # }
  # else if(is.factor(dane[,Y]))
  # {
  #   klasyfikacja_bin <- data.frame(parTune, AUCT=0, CzuloscT=0, SpecyficznoscT=0, JakoscT=0, AUCW=0, CzuloscW=0, SpecyficznoscW=0, JakoscW=0)
  #   # klasyfikacja_bin %>% group_by(colnames(parTune)) %>% summarise_at(vars(contains("T", "W")), mean)
  #   return(klasyfikacja_bin)
  # }
  # else
  # {
  #   print("Niepoprawne dane")
  # }
}


# CrossValidTune <- function(dane, X, Y, kFold = 3, parTune, algorytm="KNN", seed = 123)
# {
#   set.seed(seed)
#   
#   dl_wektora = nrow(dane)
#   
#   lista = list()
#   
#   for(i in 1:kFold)
#   { 
#     lista[[i]] <- sample(1:dl_wektora, size = dl_wektora, replace = F)
#     indeks_U <- sample( x = 1:dl_wektora, size = (1-1/kFold)*dl_wektora, replace = F )
#     
#     for( j in 1:dl_wektora)
#     {
#       for(k in 1:length(indeks_U))
#       {
#         if(indeks_U == j)
#         {
#           lista[[i]][[j]]==1
#         }
#         else
#         {
#           lista[[i]][[j]]==2
#         }
#       }
#     }
#   }
#   
#   
#   ramka <- as.data.frame(expand_grid(k=c(1:kFold), parTune))
#   
#   
#   
#   
#   if(is.numeric(dane[,Y]))
#   {
#     regresja <- data.frame(parTune, MAEt=0, MSEt=0, MAPEt=0, MAEw=0, MSEw=0, MAPEw=0)
#     return(regresja)
#   }
#   else if(is.factor(dane[,Y]))
#   {
#     klasyfikacja_bin <- data.frame(parTune, AUCT=0, CzuloscT=0, SpecyficznoscT=0, JakoscT=0, AUCW=0, CzuloscT=0, SpecyficznoscW=0, JakoscW=0)
#     return(klasyfikacja_bin)
#   }
#   else
#   {
#     print("Niepoprawne dane")
#   }
# }





# CrossValidTune <- function(dane, X, Y, kFold = 10, parTune, seed = 123, algorytm="KNN")
# {
#   set.seed(seed)
#   
#   print(Y)
#   print(X)
#   print(dane)
#   
#   if(is.numeric(dane[,Y])){
#     typ = "reg"
#   }else if(is.factor(dane[,Y])){
#     if (length(levels(dane[,Y])) == 2){
#       typ = "bin"
#     }
#     else if(length(levels(dane[,Y])) > 2){
#       typ = "multi"
#     }
#   }
#   
#   print("******************")
#   print("------------------")
#   print(paste("Algorytm: ", algorytm))
#   print(paste("Typ algorytmu: ", typ))
#   print("------------------")
#   print("******************")
#  
#   # print(parTune)
#   
#   
#   if(typ=="reg"){
#     MAE_Train_MEAN = vector()
#     MSE_Train_MEAN = vector()
#     MAPE_Train_MEAN = vector()
# 
#     MAE_Test_MEAN = vector()
#     MSE_Test_MEAN = vector()
#     MAPE_Test_MEAN = vector()
#   }
#   else if(typ=="bin"){
#     Czulosc_Train_MEAN <- vector()
#     Specyficznosc_Train_MEAN  <- vector()
#     Jakosc_Train_MEAN  <- vector()
# 
#     Czulosc_Test_MEAN <- vector()
#     Specyficznosc_Test_MEAN  <- vector()
#     Jakosc_Test_MEAN <- vector()
#   }
#   else if(typ=="multi"){
#     Jakosc_Train_MEAN <- vector()
#     Jakosc_Test_MEAN <- vector()
#   }
#   
#   
#   
#   for (i in 1:nrow(parTune)){
#     hiper <- parTune[i,]
# 
#     if(typ=="reg"){
#       MAE_Train = vector()
#       MSE_Train = vector()
#       MAPE_Train = vector()
# 
#       MAE_Test = vector()
#       MSE_Test = vector()
#       MAPE_Test = vector()
#     }
#     else if(typ=="bin"){
#       Czulosc_Train <- vector()
#       Specyficznosc_Train  <- vector()
#       Jakosc_Train  <- vector()
# 
#       Czulosc_Test <- vector()
#       Specyficznosc_Test  <- vector()
#       Jakosc_Test <- vector()
#     }
#     else if(typ=="multi"){
#       Jakosc_Train <- vector()
#       Jakosc_Test <- vector()
#     }

  #   
  #   
  #   for (j in 1:kFold){
  #     indxTest <- sample( 1:nrow(dane), size = 1/kFold * nrow(dane), replace = F )
  #     dane_Test <- dane[indxTest,] 
  #     dane_Train <- dane[-indxTest,]
  #     
  #     if(algorytm=="KNN"){                                ### --- ### --- KNN --- ### --- ### 
  #       
  #       KNNmodel <- KNNtrain( X=dane_Train[,X], y_tar=dane_Train[,y], k = hiper, XminNew=0, XmaxNew=1 )
  #       KNNpred_Train <- KNNpred(KNNmodel, X=dane_Train[,X])
  #       KNNpred_Test <- KNNpred(KNNmodel, X=dane_Test[,X])
  #       
  #       if(typ=="reg")
  #       {
  #         Train_ocena = ModelOcena(dane_Train[,y], KNNpred_Train)
  #         Test_ocena = ModelOcena(dane_Test[,y], KNNpred_Test)
  #         
  #         MAE_Train = append(MAE_Train, Train_ocena["MAE"])
  #         MSE_Train = append(MSE_Train, Train_ocena["MSE"])
  #         MAPE_Train = append(MAPE_Train, Train_ocena["MAPE"])
  #         
  #         MAE_Test = append(MAE_Test, Test_ocena["MAE"])
  #         MSE_Test = append(MSE_Test, Test_ocena["MSE"])
  #         MAPE_Test = append(MAPE_Test, Test_ocena["MAPE"])
  #       }
  #       else if(typ=="bin")
  #       {
  #         print(1)
  #         Train_ocena = ModelOcena(dane_Train[, y], as.numeric(KNNpred_Train[,1]))
  #         Test_ocena = ModelOcena(dane_Test[, y], as.numeric(KNNpred_Test[,1]))
  #         print(2)
  #         Train_miary = Train_ocena[[3]]
  #         Test_miary = Test_ocena[[3]]
  #         
  #         Czulosc_Train <- append(Czulosc_Train, Train_miary["Czulosc"])
  #         Specyficznosc_Train  <- append(Specyficznosc_Train, Train_miary["Specyficznosc"])
  #         Jakosc_Train  <- append(Jakosc_Train, Train_miary["Jakosc"])
  #         
  #         Czulosc_Test <- append(Czulosc_Test, Test_miary["Czulosc"])
  #         Specyficznosc_Test  <- append(Specyficznosc_Test, Test_miary["Specyficznosc"])
  #         Jakosc_Test <- append(Jakosc_Test, Test_miary["Jakosc"])
  #       }
  #       else if(typ=="multi")
  #       {
  #         Jakosc_Train <- append(Jakosc_Train, Trafnosc_new(dane_Train[,y], KNNpred_Train$Klasa))
  #         Jakosc_Test <- append(Jakosc_Test, Trafnosc_new(dane_Test[,y], KNNpred_Test$Klasa))
  #       }
  #       
  #     }else if(algorytm=="drzewa"){                     ### --- ### --- DRZEWA --- ### --- ### 
  #       
  #       print("Brak mozliwosci zrobienia predykcji na Drzewach!")
  #       
  #     }else if(algorytm=="NN"){                      ### --- ### --- Neural Network --- ### --- ###
  #       
  #       dane_Test <- dane[indxTest,]
  #       dane_Train <- dane[-indxTest,]
  #       
  #       X_Train = as.matrix(sapply(dane_Train[,X],MinMax))
  #       X_Test = as.matrix(sapply(dane_Test[,X],MinMax))
  #       
  #       
  #       if(typ=="bin"){
  #         
  #         Y_Train = as.matrix(as.numeric(dane_Train[,y]))
  #         Y_Test = as.matrix(as.numeric(dane_Test[,y]))
  #         
  #         NN_model_Bin <- trainNN_old( X_Train, Y_Train, h = hiper$h[1], lr = hiper$lr, iter = hiper$iter, seed = 123, typ = "binarna")
  #         NN_predict_Bin_Train <- predNN_old( X_Train, NN_model_Bin, typ = "binarna")
  #         NN_predict_Bin_Test <- predNN_old( X_Test, NN_model_Bin, typ = "binarna")
  #         
  #         Train_ocena = ModelOcena(dane_Train[, y], NN_predict_Bin_Train)
  #         Test_ocena = ModelOcena(dane_Test[, y], NN_predict_Bin_Test)
  #         Train_miary = Train_ocena[[3]]
  #         Test_miary = Test_ocena[[3]]
  #         
  #         Czulosc_Train <- append(Czulosc_Train, Train_miary["Czulosc"])
  #         Specyficznosc_Train  <- append(Specyficznosc_Train, Train_miary["Specyficznosc"])
  #         Jakosc_Train  <- append(Jakosc_Train, Train_miary["Jakosc"])
  #         
  #         Czulosc_Test <- append(Czulosc_Test, Test_miary["Czulosc"])
  #         Specyficznosc_Test  <- append(Specyficznosc_Test, Test_miary["Specyficznosc"])
  #         Jakosc_Test <- append(Jakosc_Test, Test_miary["Jakosc"])
  #         
  #       }else if(typ=="multi"){
  #         
  #         klasy = levels(dane[,y])
  #         Y_Train = model.matrix( ~ dane_Train[,y] - 1, dane_Train)
  #         Y_Test = model.matrix( ~ dane_Test[,y] - 1, dane_Test)
  #         
  #         NN_model_Multi <- trainNN_old( X_Train, Y_Train, h = hiper$h[1], lr = hiper$lr, iter = hiper$iter, seed = 123, typ = "wieloklasowa")
  #         NN_predict_Multi_Train <- predNN_old( X_Train, NN_model_Multi, typ = "wieloklasowa")
  #         NN_predict_Multi_Test <- predNN_old( X_Test, NN_model_Multi, typ = "wieloklasowa")
  #         
  #         NNpred_Train <- as.numeric(klasy[apply( NN_predict_Multi_Train, 1, which.max )])
  #         NNpred_Test <- as.numeric(klasy[apply( NN_predict_Multi_Train, 1, which.max )])
  #         
  #         Jakosc_Train <- append(Jakosc_Train, Trafnosc_new(NNpred_Train, dane_Train[,y]))
  #         Jakosc_Test <- append(Jakosc_Test, Trafnosc_new(NNpred_Test, dane_Test[,y]))
  #         
  #       }else if(typ=="reg"){
  #         
  #         Y_Train = as.matrix(MinMax(dane_Train[,y]))
  #         Y_Test = as.matrix(MinMax(dane_Test[,y]))
  #         Y_min = min(dane[,y])
  #         Y_max = max(dane[,y])
  #         
  #         NN_model_Reg <- trainNN_old( X_Train, Y_Train, h = hiper$h[1], lr = hiper$lr, iter = hiper$iter, seed = 123, typ = "regresja")
  #         NN_predict_Reg_Train <- predNN_old( X_Train, NN_model_Reg, typ = "regresja")
  #         NN_predict_Reg_Test <- predNN_old( X_Test, NN_model_Reg, typ = "regresja")
  #         
  #         NN_predict_Reg_Train_Scale <- MinMaxOdwrot(NN_predict_Reg_Train, Y_min, Y_max)
  #         NN_predict_Reg_Test_Scale <- MinMaxOdwrot(NN_predict_Reg_Test, Y_min, Y_max)
  #         
  #         Train_ocena = ModelOcena(dane_Train[,y], NN_predict_Reg_Train_Scale)
  #         Test_ocena = ModelOcena(dane_Test[,y], NN_predict_Reg_Test_Scale)
  #         
  #         MAE_Train = append(MAE_Train, Train_ocena["MAE"])
  #         MSE_Train = append(MSE_Train, Train_ocena["MSE"])
  #         MAPE_Train = append(MAPE_Train, Train_ocena["MAPE"])
  #         
  #         MAE_Test = append(MAE_Test, Test_ocena["MAE"])
  #         MSE_Test = append(MSE_Test, Test_ocena["MSE"])
  #         MAPE_Test = append(MAPE_Test, Test_ocena["MAPE"])
  #       }
  #       
  #       
  #     }
  #     
  #   }
  #   
  #   if(typ=="reg"){
  #     
  #     mae_train = append(mae_train, mean(MAE_Train))
  #     mse_train = append(mse_train, mean(MSE_Train))
  #     mape_train = append(mape_train, mean(MAPE_Train))
  #     
  #     mae_test = append(mae_test, mean(MAE_Test))
  #     mse_test = append(mse_test, mean(MSE_Test))
  #     mape_test = append(mape_test, mean(MAPE_Test))
  #   }
  #   else if(typ=="bin"){
  #     
  #     czulosc_train <- append(czulosc_train, mean(Czulosc_Train))
  #     specyficznosc_train  <- append(specyficznosc_train, mean(Specyficznosc_Train))
  #     jakosc_train  <- append(jakosc_train, mean(Jakosc_Train))
  #     
  #     czulosc_test <- append(czulosc_test, mean(Czulosc_Test))
  #     specyficznosc_test  <- append(specyficznosc_test, mean(Specyficznosc_Test))
  #     jakosc_test <- append(jakosc_test, mean(Jakosc_Test))
  #   }
  #   else if(typ=="multi"){
  #     jakosc_train <- append(jakosc_train, mean(Jakosc_Train))
  #     jakosc_test <- append(jakosc_test, mean(Jakosc_Test))
  #   }
  # }#parTune
  # 
  # if(typ=="reg"){
  #   wyniki <- data.frame(matrix(0, nrow(parTune), ncol=6))
  #   colnames(wyniki) = c("MAE_TRAIN", "MSE_TRAIN", "MAPE_TRAIN", "MAE_TEST", "MSE_TEST", "MAPE_TEST")
  #   wyniki[,1] = mae_train
  #   wyniki[,2] = mse_train
  #   wyniki[,3] = mape_train
  #   wyniki[,4] = mae_test
  #   wyniki[,5] = mse_test
  #   wyniki[,6] = mape_test
  # }
  # else if(typ=="bin"){
  #   wyniki <- data.frame(matrix(0, nrow(parTune), ncol=6))
  #   colnames(wyniki) = c("Czulosc_TRAIN", "Specyficznosc_TRAIN", "Jakosc_TRAIN", "Czulosc_TEST", "Specyficznosc_TEST", "Jakosc_TEST")
  #   # print(czulosc_train)
  #   wyniki[,1] = czulosc_train
  #   wyniki[,2] = specyficznosc_train
  #   wyniki[,3] = jakosc_train
  #   wyniki[,4] = czulosc_test
  #   wyniki[,5] = specyficznosc_test
  #   wyniki[,6] = jakosc_test
  # }
  # else if(typ=="multi"){
  #   wyniki <- data.frame(matrix(0, nrow(parTune), ncol=2))
  #   colnames(wyniki) = c("Jakosc_TRAIN", "Jakosc_TEST")
  #   wyniki[,1] = jakosc_train
  #   wyniki[,2] = jakosc_test
  # }
  # wyniki = cbind(parTune, wyniki)
  # return( wyniki )
# }








