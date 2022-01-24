#Regresja
MAE <- function(a, b){
  return(mean(abs(a - b)))
}

MAE(c(1,2,3),c(2,34,3))

MSE <- function(a, b){
  return(mean((a - b)^2))
}

MAPE <- function(a, b){
  return( mean(abs((a - b)/a) ))
}

#Klasyfikacja
AUC <- function(a, b){
  roc_krzywa <- roc(a, b)
  TPR <- rev(roc_krzywa$sensitivities)
  FPR <- rev(1 - roc_krzywa$specificities)
  dFPR <- c(diff(FPR), 0)
  dTPR <- c(diff(TPR), 0)
  AUC_wynik <- sum(TPR * dFPR) + sum(dTPR * dFPR)/2
  return(AUC_wynik)
}

Youden <- function(a, b){
  roc_krzywa <- roc(a, b)
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
    miary <- c( "AUC" = AUC(y_tar, y_hat), "Czu?o??" = Czulosc(Mat), "Specyficzno??" = Specyficznosc(Mat), "Jako??" = Jakosc(Mat))
    klasyfikacja <- list(Mat, Youden(y_tar, y_hat), miary)
    return(klasyfikacja)
  }
  else{
    c("Dane niepoprawne")
  }
}


CrossValidTune <- function(dane, kFold, parTune, seed){
  set.seed(seed)
  k <- nrow(dane)
  wektor <- c()
  lista <- list()
  p <- 1
  
  for (j in 1:kFold){
    indxT <- sample( x = 1:k, size = round((1-1/kFold) * k), replace = F )
    s_indxT = sort(indxT)
    while (p<=length(s_indxT)){
      for (i in 1:k){
        if (s_indxT[p] == i){
          wektor[i] = 1
          p = p+1
        }
        else{
          wektor[i] = 2
        }
      }
    }
    
    lista[[j]] = c(wektor)
  }
  
  ramka_r <- data.frame()
  for (i in 1:(length(parTune))){
    ramka_r[i+1] = parTune[i]
  }
  
  if(is.numeric(y_tar)){
    f_reg <- ramka_r
    f_reg['MAEt'] = MAE(y_tar, y_hat)
    f_reg['MSEt'] = MSE(y_tar, y_hat)
    f_reg['MAPEt'] = MSE(y_tar, y_hat)
    return(f_reg)
  }
  else if(is.factor(y_tar)){
    f_klas <- ramka_r
    f_klas['AUCT'] = AUC(y_tar, y_hat)
    f_klas['czulosc'] = Czulosc(y_tar, y_hat)
    f_klas['Specyficznosc'] = Specyficznosc(y_tar, y_hat)
    f_klas['Jakosc'] = Jakosc(y_tar, y_hat)
    return(f_klas)
  }
}


zbior <- function( X ){
  if ((class(X[,c(length(X))]) == 'integer') & (length(unique(X[,c(length(X))])) == 2)){
    rodzaj <<- "Klasyfikacja binarna"
    print(rodzaj)
  }
  else if ((class(X[,c(length(X))]) == 'integer') & (length(unique(X[,c(length(X))])) > 2)){
    rodzaj <<- "Klasyfikacja wieloklasowa"
    print(rodzaj)
  }
  else if ((class(X[,c(length(X))]) == 'numeric') & (length(unique(X[,c(length(X))])) > 2)){
    rodzaj <<- "Regresja"
    print(rodzaj)
  }
  else{
    print("Nieprawidłowe dane")
  }
}


MinMax <- function( x, new_min = 0, new_max = 1 ){
  return( ( ( x - min(x) ) / ( max(x) - min(x) ) ) * ( new_max - new_min ) + new_min )
}

d_euklides <- function(x_i, x_n) {
  return(sqrt(sum((x_i - x_n) ^ 2)))
}

d_hamming <- function(x_i, x_n){
  p <- length(x_i)
  return (sum(x_i != x_n) / p)
}


KNNtrain<-function(X,y_tar,k, XminNew, XmaxNew){
  
  if(any(is.na(X)==TRUE | is.na(y_tar)==TRUE)){
    return("Dane nie są kompletne.")
  } 
  else if(k<=0){ 
    return("K musi być większe od 0")
  }
  else if(is.matrix(X)==FALSE & is.data.frame(X)==FALSE)
  {
    return("X nie jest macierzą ani ramką danych")
  }
  else if(ncol(Filter(is.numeric,X))!=0){
    numeric_X<-Filter(is.numeric,X)
    X_norm<-apply(numeric_X,2,function(df=X,min=XminNew,max=XmaxNew){
      return(((df-min(df))/(max(df)-min(df)))*(max-min)+min)})
    min_vec<-c()
    max_vec<-c()
    for (i in 1:ncol(numeric_X)) {
      min_vec[i]<-c(min(numeric_X[,i]))
      max_vec[i]<-c(max(numeric_X[,i]))
    }
    new_min<-c()
    new_max<-c()
    for(j in 1:ncol(X_norm)){
      new_min[j]<-c(min(X_norm[,j]))
      new_max[j]<-c(max(X_norm[,j]))
    }
    wynik<-rbind("Min"=min_vec,"Max"=max_vec,"NewMin"=new_min,"NewMax"=new_max)
    colnames(wynik)<-colnames(numeric_X)
    
    df_normal<-cbind(X_norm,Filter(is.factor,X))
    Lista_wynik<-list("X"=df_normal,"y_tar"=y_tar,"k"=k)
    return(Lista_wynik)
  }
  else{
    Lista_wynik2<-list("X"=X,"y_tar"=y_tar,"k"=k)
    return(Lista_wynik2)
  }
}


KNNpred<-function(KNNmodel,X){
  
  if(any(is.na(X)==TRUE)){
    return("Niekompletne dane")
  } 
  else if(any(colnames(KNNmodel$X)==colnames(X))!=TRUE){ 
    return("Różne nazwy zmiennych")
  }
  else if(ncol(Filter(is.numeric,X))!=0){
    x_min<-min(Filter(is.numeric,KNNmodel$X))
    x_max<-max(Filter(is.numeric,KNNmodel$X))
    x_numeric<-Filter(is.numeric,X)
    X_norm<-apply(x_numeric,2,function(df=X,min=x_min,max=x_max){
      return(((df-min(df))/(max(df)-min(df)))*(x_max-x_min)+x_min)})
  }
  
  if(ncol(Filter(is.numeric,X))!=0 & ncol(Filter(is.factor,X))==0 & ncol(Filter(is.ordered,X))==0){
    nTrain<-nrow(KNNmodel$X)
    nPred<-nrow(X_norm)
    odl<-matrix(0,nTrain,nPred)
    for(i in 1:nTrain){
      for(j in 1:nPred){
        odl[i,j]<-sqrt(sum((KNNmodel$X[i,]-X_norm[j,])^2))
      }
    }
  }
  else if(ncol(Filter(is.factor,X))!=0 & ncol(Filter(is.numeric,X))==0 & ncol(Filter(is.ordered,X))==0){
    nTrain<-nrow(KNNmodel$X)
    nPred<-nrow(X)
    odl<-matrix(0,nTrain,nPred)
    for(i in 1:nTrain){
      for(j in 1:nPred){
        odl[i, j] <-sum(ifelse(KNNmodel$X[i,] == X[j,],1,0))/ncol(X)
      }
    }
  }
  
  if(rodzaj == 'Regresja'){
    pred<-double(nPred)
    for(i in 1:nPred){
      kNaj<-order(odl[,i])
      kNaj<-kNaj[1:KNNmodel$k]
      y_hat<-mean(KNNmodel$y_tar[kNaj])
      pred[i]<-y_hat
    }
    return(pred)
  }
  else if(rodzaj == 'Klasyfikacja binarna'){
    pred<-double(nPred)
    klasa<-double(nPred)
    
    for(i in 1:nPred){
      kNaj<-order(odl[,i])
      kNaj<-kNaj[1:KNNmodel$k]
      y_hat<-names(which.max(table( KNNmodel$y_tar[kNaj]))) 
      klasa[i]<-y_hat 
      y_hat2<-prop.table(table(KNNmodel$y_tar[kNaj]))  
      pred[i]<-y_hat2
    }
    cb<-cbind('Klasa'=klasa,'P'=1-pred,'N'=pred)
    return(cb)
  }
  else {
    klasa<-double(nPred)
    for(i in 1:nPred){
      kNaj<-order(odl[,i])
      kNaj<-kNaj[1:KNNmodel$k]
      y_hat<-names(which.max(table( KNNmodel$y_tar[kNaj]))) 
      klasa[i]<-y_hat 
    }
    return(klasa)
  }
}


# Sieci neuronowe

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


zbior <- function( X ){
  if ((class(X[,c(length(X))]) == 'integer') & (length(unique(X[,c(length(X))])) == 2)){
    rodzaj <<- "Klasyfikacja binarna"
    print(rodzaj)
  }
  else if ((class(X[,c(length(X))]) == 'integer') & (length(unique(X[,c(length(X))])) > 2)){
    rodzaj <<- "Klasyfikacja wieloklasowa"
    print(rodzaj)
  }
  else if ((class(X[,c(length(X))]) == 'numeric') & (length(unique(X[,c(length(X))])) > 2)){
    rodzaj <<- "Regresja"
    print(rodzaj)
  }
  else{
    print("Nieprawidłowe dane")
  }
}

wprzod <- function( X, W1, W2, W3 ){
  
  h1 <- cbind( matrix( 1, nrow = nrow(X) ), sigmoid( X %*% W1 )  )
  h2 <- cbind( matrix( 1, nrow = nrow(X) ), sigmoid( h1 %*% W2 )  )
  
  ### WARUNEK NA REG/KLAS:
  if (rodzaj == 'Klasyfikacja binarna'){
    y_hat <- sigmoid( h2 %*% W3 ) # klasyfikacja binarna
  }
  else if (rodzaj == 'Klasyfikacja wieloklasowa'){
    y_hat <- matrix( t( apply( h2 %*% W3, 1, SoftMax ) ), nrow = nrow(X) ) # klasyfikacja wieloklasowa
  }
  else if (rodzaj == 'Regresja'){
    y_hat <- h2 %*% W3 # regresja
  }
  else{
    print("Nieprawidłowe dane")
  }
  
  return( list( y_hat = y_hat, H1 = h1, H2 = h2 ) )
}

wstecz <- function( X, y_tar, y_hat, W1, W2, W3, H1, H2, lr ){
  
  ### WARUNEK NA REG/KLAS:
  if (rodzaj == 'Klasyfikacja binarna'){
    dy_hat <- (y_tar - y_hat) * dsigmoid( y_hat ) # klasyfikacja binarna
  }
  else if (rodzaj == 'Klasyfikacja wieloklasowa'){
    dy_hat <- (y_tar - y_hat) / nrow( X ) # klasyfikacja wieloklasowa
  }
  else if (rodzaj == 'Regresja'){
    dy_hat <- (y_tar - y_hat) # regresja
  }
  else{
    print("Nieprawidłowe dane")
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

trainNN <- function( x, y_tar, h = c(5,5), lr = 0.01, iter = 10000, seed = 123 ){
  set.seed( seed )
  X <- cbind( rep( 1, nrow(X) ), x )
  
  W1 <- matrix( runif( ncol(X) * h[1], -1, 1 ), nrow = ncol(X) )
  W2 <- matrix( runif( (h[1]+1) * h[2], -1, 1 ), nrow = h[1] + 1 )
  W3 <- matrix( runif( (h[2]+1) * ncol(y_tar), -1, 1 ), nrow = h[2] + 1 )
  error <- double( iter )
  for( i in 1:iter ){
    sygnalwprzod <- wprzod(X, W1, W2, W3 )
    sygnalwtyl <- wstecz(X, y_tar, y_hat = sygnalwprzod$y_hat, W1, W2, W3, H1 = sygnalwprzod$H1, H2 = sygnalwprzod$H2, lr )
    W1 <- sygnalwtyl$W1
    W2 <- sygnalwtyl$W2
    W3 <- sygnalwtyl$W3
    cat( paste0( "\rIteracja: ", i ) )
    error[i] <- lossSS( y_tar, sygnalwprzod$y_hat )
  }
  xwartosci <- seq( 1, iter, length = 1000 )
  print( qplot( xwartosci, error[xwartosci], geom = "line", main = "Error", xlab = "Iteracje" ) )
  return( list( y_hat = sygnalwprzod$y_hat, W1 = W1, W2 = W2, W3 = W3 ) )
}

predNN <- function( xnew, nn ){
  xnew <- cbind( rep( 1, nrow(xnew) ), xnew )
  h1 <- cbind( matrix( 1, nrow = nrow(xnew) ), sigmoid( xnew %*% nn$W1 )  )
  h2 <- cbind( matrix( 1, nrow = nrow(xnew) ), sigmoid( h1 %*% nn$W2 )  )
  y_hat <- sigmoid( h2 %*% nn$W3 )
  return( y_hat )
}


# Wektory nośne

Decision <- function( X, theta, theta0 ){
  X %*% t( theta ) + theta0
}

Margin <- function( X, y, theta, theta0 ){
  y * Decision( X, theta, theta0 )
}

Cost <- function( margin, theta, C ){
  (1/2) * theta %*% t(theta) + C * sum( pmax( 0, 1 - margin ) )
}

trainSVM <- function( X, y, C = 1, lr = 0.001, maxiter = 50000 ){
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


# Drzewa decyzyjne

Entropy <- function( prob ){
  
  res <- prob * log2( prob )
  res[ prob == 0 ] <- 0
  res <- -sum( res )
  return( res )
  
}

Prob <- function( y ){
  
  res <- unname( table( y ) )
  res <- res / sum( res )
  
  return( res )
  
}

SpliNum <- function( Y, x, parentVal, splits, minobs ){
  
  n <- length( x )
  res <- data.frame( matrix( 0, length(splits), 6 ) )
  colnames( res ) <- c("InfGain","lVal","rVal","point","ln","rn")
  
  for( i in 1:length(splits) ){
    
    partition <- x <= splits[i] # %in%: x %in% c("wyzsze","podstawowe")
    ln <- sum( partition )
    rn <- n - ln
    
    if( any( c(ln,rn) < minobs ) ){
      
      res[i,] <- 0
      
    }else{
      
      lVal <- Entropy( Prob( Y[partition] ) )
      rVal <- Entropy( Prob( Y[!partition] ) )
      InfGain <- parentVal - ( lVal * ln/n  + rVal * rn/n )
      
      res[i,"InfGain"] <- InfGain
      res[i,"lVal"] <- lVal
      res[i,"rVal"] <- rVal
      res[i,"point"] <- splits[i]
      res[i,"ln"] <- ln
      res[i,"rn"] <- rn
      
    }
    
  }
  
  return( res )
  
}


SplitVar <- function( Y, x, parentVal, minobs ){
  
  s <- unique( x )
  if( length(x) == 1 ){
    
    splits <- s
    
  }else{
    
    splits <- head( sort( s ), -1 )
    
  }
  
  res <- SpliNum( Y, x, parentVal, splits, minobs )
  
  incl <- res$ln >= minobs & res$rn >= minobs & res$InfGain > 0
  res <- res[ incl, , drop = F ]
  
  best <- which.max( res$InfGain )
  # ten który daje zbilansowany podzial
  res <- res[ best, , drop = F ]
  
  return( res )
  
}

FindBestSplit <- function( Y, data, parentVal, minobs ){
  
  X <- !colnames( data ) %in% Y
  
  res <- sapply( colnames( data )[X], function(i){
    
    SplitVar( data[,Y], data[,i], parentVal, minobs )
    
  }, simplify = F )
  
  res <- do.call( "rbind", res )
  
  best <- which.max( res$InfGain )
  res <- res[ best, , drop = F ]
  
  return( res )
  
}


library( data.tree )
Tree <- function( Y, X, data, depth, minobs ){
  
  tree <- Node$new( "Root" )
  tree$Depth <- 0
  tree$Count <- nrow( data )
  tree$inf <- Entropy( Prob( data[,Y] ) )
  
  BuildTree( tree, Y, X, data, depth, minobs )
  
  return( tree )
  
}

BuildTree <- function( node, Y, X, data, depth, minobs ){
  
  node$Count <- nrow( data )
  node$Prob <- Prob( data[,Y] )
  
  bestSplit <- FindBestSplit( Y, data, node$inf, minobs )
  
  ifStop <- nrow( bestSplit ) == 0 
  
  if( node$Depth == depth | ifStop | all(  node$Prob %in% c(0,1) ) ){
    
    node$Leaf <- "*"
    return( node )
    
  }else{
    
    split_indx <- data[,rownames(bestSplit)] <= bestSplit$point
    child_frame <- split( data, split_indx )
    
    name <- sprintf( "%s <= %s", rownames(bestSplit), bestSplit$point )
    child_l <- node$AddChild( name )
    child_l$value <- split_indx
    child_l$Depth <- node$Depth + 1
    child_l$inf <- bestSplit$lVal
    
    BuildTree( child_l, Y, X, child_frame[[1]], depth, minobs )
    
    name <- sprintf( "%s >  %s", rownames(bestSplit), bestSplit$point )
    child_r <- node$AddChild( name )
    child_r$value <- split_indx
    child_r$Depth <- node$Depth + 1
    child_r$inf <- bestSplit$rVal
    
    BuildTree( child_r, Y, X, child_frame[[2]], depth, minobs )
    
  }
  
}

