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
  # sprawdzanie czy "X" oraz "y_tar" nie maj¹ braków danych, czy "k" jest wiêksze od 0, czy "X" jest macierz¹ lub ramk¹ danych 
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
    print(attr(Xnew, "maxOrg"))
    knn$maxOrg <- attr(Xnew, "maxOrg")
    minmaxnew <- c(XminNew, XmaxNew)
    attr(Xnew, "minmaxNew") <- Xnew
    print(attr(Xnew, "minmaxNew"))
    knn$minmaxNew <- Xnew
    knn$min<-XminNew
    knn$max<-XmaxNew
    knn$X<-Xnew
  }
  return(knn )
}

install.packages(Rcpp)
library(Rcpp)
library(dplyr)
c_gower_vec_matrix_dist = cppFunction(plugins = "openmp",
                                      'NumericVector c_gower_vec_matrix_cat_dist(IntegerMatrix x, IntegerVector x2) {
  NumericVector res(x.nrow());
  #pragma omp parallel for 
  for(int i = 0; i < x.nrow(); i++) {
    double dist = 0;
    for(int col = 0; col < x.ncol(); col++) {
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
  
  
  for(i in 1:nrow(x2)) {
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
  for( i in 1:nTrain ){
    for( j in 1:nPred ){
      odl[i,j] <- d_euklides( model$X[i,], Xnew[j,] )
    }
    n<-0
    p <- 0
    z <- 0
    
    if(any(is.na(Xnew))==FALSE&& ncol(model$X)==ncol(Xnew))
    {Xnew1 <- Xnew
    for( i in 1:ncol(Xnew1)){
      if(is.numeric(Xnew1[,i])){
        n<-n+1
        Xnew1[,i] <- MinMax(Xnew[,i], new_min = model$min, new_max = model$max)
      }else if(is.factor(X[,i]) & is.ordered(X[,i])){
        p<- p + 1
        Xnew1[,i] <- X[,i] }
      else if(is.factor(X[,i])){
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
    else return( 0)
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
    
    if( (is.factor(data[,Y]) && type=="SS") || (is.numeric(data[,Y]) && type=="Gini") || (is.numeric(data[,Y]) && type == "Entropy") == TRUE){
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





library( data.tree )

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
  { t<-mean(attributes(tree)$data[,attributes(tree)$Y])
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


#sieci neuronowe


MinMax <- function( x ){
  return( (x-min(x)) / (max(x)-min(x)) )
}

sigmoid <- function( x ){
  return( 1 / (1 + exp(-x) ) )
}

dsigmoid <- function( x ){
  return( x * (1 - x) )
}


wprzod_NOWE_K <- function( X, W1, W2, W3){
  H1 <- cbind( matrix( 1, nrow = nrow(X)), sigmoid(X %*% W1))
  H2 <- cbind( matrix( 1, nrow = nrow(X)), sigmoid(H1 %*% W2))
  lista_wprzod <- list( y_hat = sigmoid(H2 %*% W3), H1 = H1, H2 = H2)
  return(lista_wprzod)
}



wstecz_NOWE_K <- function( X, y_tar, y_hat, W1, W2, W3, H1, H2, lr){
  dy_hat <- (y_tar - y_hat) * dsigmoid(y_hat)
  dW3 <- t(H2) %*% dy_hat
  dH2<- dy_hat %*% t(W3) * dsigmoid(H2)
  dW2 <- t(H1) %*% dH2[,-1]
  dH1<- dH2[,-1] %*% t(W2) * dsigmoid(H1)
  dW1 <- t(X) %*% dH1[,-1]
  W1 <- W1 + lr * dW1
  W2 <- W2 + lr * dW2
  W3 <- W3 + lr * dW3
  lista_wag <- list(W1 = W1, W2 = W2, W3 = W3)
  return(lista_wag)
}


trainNN_NOWE_K <- function( Yname, Xnames, data, h, lr, Maxiter, seed){
  set.seed(seed)
  
  y_tar = data[,Yname]
  X <- cbind( rep( 1, nrow(data[,Xnames]) ), data[,Xnames] )
  h = unlist(h, use.names = FALSE)
  W1 <- matrix(runif(ncol(X) * h[1], -1, 1 ), nrow = ncol(X))
  W2 <- matrix(runif((h[1]+1) * h[2], -1, 1 ), nrow = h[1] + 1)
  W3 <- matrix(runif((h[2]+1) * 1, -1, 1 ), nrow = h[2] + 1)
  
  for( i in 1:Maxiter ){
    Sygnal_w_przod <- wprzod_NOWE_K(X, W1, W2, W3)
    Sygnal_w_tyl <- wstecz_NOWE_K(X, y_tar, y_hat = Sygnal_w_przod$y_hat, W1, W2, W3, H1 = Sygnal_w_przod$H1, H2 = Sygnal_w_przod$H2, lr)
    W1 <- Sygnal_w_tyl$W1
    W2 <- Sygnal_w_tyl$W2
    W3 <- Sygnal_w_tyl$W3
    cat( paste0( "\rIteracje wykonane: ", i , " / ", Maxiter) )
  }
  
  return( list( y_hat = Sygnal_w_przod$y_hat, W1 = W1, W2 = W2, W3 = W3 ) )
}


predNN_NOWE_K <- function( Xnew, Siec_NN){
  Xnew <- cbind(rep(1, nrow(Xnew)), Xnew)
  H1 <- cbind(matrix(1, nrow = nrow(Xnew)), sigmoid(Xnew %*% Siec_NN$W1))
  H2 <- cbind(matrix(1, nrow = nrow(Xnew)), sigmoid(H1 %*% Siec_NN$W2))
  return(sigmoid(H2 %*% Siec_NN$W3))
  
} 
  ## sieci neuronowe z zajeæ
  
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
  
  wyjscie <- c(0.3,0.7,0.6)
  psto <- wyjscie / sum( wyjscie )
  psto
  sum( psto)
  
  Y_iris_mat <- model.matrix( ~ Species - 1, iris )
  data.frame( iris$Species, Y_iris_mat )
  
  wprzod <- function( X, W1, W2, W3 ){
    h1 <- sigmoid( X %*% W1 )
    h2 <- sigmoid( h1 %*% W2 )
    # y_hat <- sigmoid( h2 %*% W3 ) # klasyfikacja binarna
    # y_hat <- matrix( t( apply( h2 %*% W3, 1, SoftMax ) ), nrow = nrow(X) ) # klasyfikacja wieloklasowa
    y_hat <- h2 %*% W3 # regresja
    return( list( y_hat = y_hat, H1 = h1, H2 = h2 ) )
  }
  
  wstecz <- function( X, y_tar, y_hat, W1, W2, W3, H1, H2, lr ){
    # dy_hat <- (y_tar - y_hat) * d_sigmoid( y_hat ) # klasyfikacja binarna
    # dy_hat <- (y_tar - y_hat) / nrow( X ) # klasyfikacja wieloklasowa
    dy_hat <- (y_tar - y_hat) # regresja
    dW3 <- t(H2) %*% dy_hat
    dH2<- dy_hat %*% t(W3) * d_sigmoid( H2 )
    dW2 <- t(H1) %*% dH2
    dH1<- dH2 %*% t(W2) * d_sigmoid( H1 )
    dW1 <- t(X) %*% dH1
    W1 <- W1 + lr * dW1
    W2 <- W2 + lr * dW2
    W3 <- W3 + lr * dW3
    return( list( W1 = W1, W2 = W2, W3 = W3 ) )
  }
  
  trainNN <- function( X, y_tar, h = c(5,5), lr = 0.01, iter = 10000, seed = 123 ){
    set.seed( seed )
    W1 <- matrix( runif( ncol(X) * h[1], -1, 1 ), nrow = ncol(X) )
    W2 <- matrix( runif( (h[1]) * h[2], -1, 1 ), nrow = h[1] )
    W3 <- matrix( runif( (h[2]) * ncol(y_tar), -1, 1 ), nrow = h[2] )
    error <- double( iter )
    for( i in 1:iter ){
      sygnalwprzod <- wprzod( X, W1, W2, W3 )
      sygnalwtyl <- wstecz( X, y_tar, y_hat = sygnalwprzod$y_hat, W1, W2, W3, H1 = sygnalwprzod$H1, H2 = sygnalwprzod$H2, lr )
      W1 <- sygnalwtyl$W1
      W2 <- sygnalwtyl$W2
      W3 <- sygnalwtyl$W3
      cat( paste0( "\rIteracja: ", i ) )
    }
    return( list( y_hat = sygnalwprzod$y_hat, W1 = W1, W2 = W2, W3 = W3 ) )
  }
}




