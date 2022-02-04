  ##  FUNKCJE ##


## K-najblizszych sasiadow ##


KNNtrain <- function(X, y_tar, k, XminNew, XmaxNew){
  if(k > 0 && (is.matrix(X) == TRUE || is.data.frame(X) == TRUE) && all(!is.na(X)) && all(!is.na(y_tar))){
    X_norm <- matrix(0,nrow(X),ncol(X))
    minOrg <- c()
    maxOrg <- c()
    for (i in 1:ncol(X)) {
      #sprawdzam czy mam dane sa wartosciami numerycznymi 
      if(is.numeric(X[,i])){
        for (j in i:nrow(X)) {
          X_norm[j,i] <- ( (X[j,i] - min(X[,i])) / (max(X[,i]) - min(X[,i])) ) * (XmaxNew - XminNew) + XminNew
          minOrg[i] <- min(X[,i])
          maxOrg[i] <- max(X[,i])
        }
      }
      #w innych przypadkach nie dokonuje normalizacji
      else if(is.factor(X[,i])){
        X_norm[,i] <- X[,i]
        minOrg[i] <- NA
        maxOrg[i] <- NA
      }
      else{
        c("Bledne dane")
      }
    }
    attr(X_norm,"minOrg") <- minOrg
    attr(X_norm,"maxOrg") <- maxOrg
    attr(X_norm,"minmaxNew") <- c("min"=XminNew,"max"=XmaxNew)
    
    knntr <- list()
    knntr[["X"]] <- X_norm
    knntr[["y"]] <- y_tar
    knntr[["k"]] <- k
    return( knntr )
    
  }
  else 
    "Bledne dane"
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
    ilorazowa <- 0
    porzadkowa <- 0
    nominalna <- 0
    for (i in 1:ncol(X)) {
      if(is.numeric(X[,i])){
        ilorazowa <- ilorazowa + 1
        for (j in 1:nrow(X)) {
          X_norm[j,i] <- ((X[j,i] - attr(KNNmodel$X,"minOrg")[i]) / 
                            (attr(KNNmodel$X,"maxOrg")[i] - attr(KNNmodel$X,"minOrg")[i]))  * 
            (attr(KNNmodel$X,"minmaxNew")["max"] - attr(KNNmodel$X,"minmaxNew")["min"]) + 
            attr(KNNmodel$X,"minmaxNew")["min"]
        }
      }
      else if(is.factor(X[,i]) & is.ordered(X[,i])){
        porzadkowa <- porzadkowa + 1
        X_norm[,i] <- X[,i]
      }
      else if(is.factor(X[,i])){
        nominalna <- nominalna + 1
        X_norm[,i] <- X[,i]
      }
    }
    odl <- matrix(0, nrow(KNNmodel$X), nrow(X_norm))
    #w zaleznosci od typow zmiennych obliczam odpowiednia odleglosc
    #1 dla skali nominalnej obliczam odleglosc Euklidesa
    if(ilorazowa == ncol(X_norm)){ 
      for(i in 1:nrow(KNNmodel$X)){
        for(j in 1:nrow(X_norm)){
          odl[ i, j ] <- sqrt(sum( (KNNmodel$X[i,] - X_norm[j,])^2 ))
        }
      }
    }
    #2 dla skali porzadkowej 
    else if(porzadkowa == ncol(X_norm)){ 
      for(i in 1:nrow((KNNmodel$X))){
        for(j in 1:nrow(X_norm)){
          for (k in 1:ncol(X_norm)) {
            uniq <- length(unique(X_norm[,k]))
            odl[i, j] <- (sum( abs(as.numeric(KNNmodel$X[i,]) - as.numeric(X_norm[j,]))  / (uniq - 1)) )
          }
        }
      }
    }
    #dla skali nominaknej licze odleglosc hamminga
    else if(nominalna == ncol(X_norm)){ 
      for(i in 1:nrow(KNNmodel$X)){
        for(j in 1:nrow(X_norm)){
          odl[i, j] <- ( (sum(KNNmodel$X[i,] != X_norm[j,])) / ncol(X_norm) )
        }
      }
    }
    else{
      c("brak oblugi dla odleg³oœci Gowera")
      
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
      pred <- c("Brak oblugi dla klasyfikacji")
    }
    return(pred)
  }
  
}

  ##  Drzewa decyzyjne  ##

library(data.tree)
StopIfNot <- function(Y, X, data, type, depth, minobs, overfit, cf) {
  
  if (!is.data.frame(data)) {
    mes<<-("Zbior danych musi byc ramka danych!")
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
    mes <<- ("Type powinien przyjmowaÄ‡ jedna z wartosci Gini, Entropy lub SS")
    return(FALSE)} 
  else if (!(overfit %in% c("none", "prune"))) {
    mes <<- ("overfit powinien przyjmowaÄ‡ jedna z wartosci none lub prune")
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
  tree$Y <- data[,Y]
  tree$X <- data[,X]
  tree$data <- data
  tree$type <- type
  tree$Depth <- depth
  tree$minobs <- minobs
  tree$overfit <- overfit
  tree$cf <- cf
}

PruneTree <- function() {
  
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
  PruneTree <- function() {}
  AssignInfo(tree,Y,X,data,type,depth, minobs, overfit, cf )
  
  return(tree)
}



strTree <- function(tree, obs) {
  
  if (tree$isLeaf) {
    print(tree$Prob)
    return(data.frame("Prob" = max(tree$Prob), "Class" = (tree$Class), stringsAsFactors = F))
  }
  
  if (is.numeric(tree$children[[1]]$BestSplit) | is.ordered(tree$children[[1]]$BestSplit)) {
    child <- tree$children[[ifelse(obs[,tree$children[[1]]$feature] > (tree$children[[1]]$BestSplit), 2, 1)]]
  }
  
  else { 
    split <- tree$children[[1]]$feature
    child <- tree$children[[ifelse((obs[,tree$children[[1]]$feature] %in% split), 1, 2)]] 
  }
  
  return (strTree(child,obs))
  
}


PredictTree <- function(tree, data) {
  
  if (is.factor(attributes(tree)$Y)) {
    out <- data.frame(matrix(0, nrow = nrow(data), ncol = 2))
    
    for (i in 1:nrow(data)) {
      out[i,] <- (strTree(tree, data[i, ,drop = F]))
    }
    
    colnames(out) <- c("Prob", "Class")
    
  }
  
  else {
    
    out <- c()
    
    for (i in 1:nrow(data)) {
      out[i] <- (strTree(tree, data[i, ,drop = F])$Class)
    }
    
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
  exp( x ) / sum( exp( x ) )
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
  if (type != "bin" & type != "multi" & type != "reg") {
    stop("bledny typ danych")
  }
  set.seed( seed )
  X <- cbind( rep( 1, nrow(x) ), x )
  
  h = unlist(h, use.names = FALSE)
  
  W1 <- matrix( runif( ncol(X) * h[1], -1, 1 ), nrow = ncol(X) )
  W2 <- matrix( runif( (h[1]+1) * h[2], -1, 1 ), nrow = h[1] + 1 )
  W3 <- matrix( runif( (h[2]+1) * ncol(y_tar), -1, 1 ), nrow = h[2] + 1 )
  
  error <- double( iter )
  
  for( i in 1:iter ){
    sygnalwprzod <- wprzod( X, W1, W2, W3, typ=typ )
    sygnalwtyl <- wstecz( X, y_tar, y_hat = sygnalwprzod$y_hat, W1, W2, W3, H1 = sygnalwprzod$H1, H2 = sygnalwprzod$H2, lr, typ=typ )
    W1 <- sygnalwtyl$W1
    W2 <- sygnalwtyl$W2
    W3 <- sygnalwtyl$W3
    cat( paste0( "\rIteracja: ", i , " / ", iter) )
    
    error[i] <- lossSS( y_tar, sygnalwprzod$y_hat )
  }
  x_wartosci <- seq(1,MaxIter, length = 1000)
  print(qplot(x_wartosci, error, main = "Error", xlab = "Iteracje", geom = "line"))
  return( list( y_hat = sygnalwprzod$y_hat, W1 = W1, W2 = W2, W3 = W3 ) )
}

predNN <- function( xnew, nn, typ = "bin" ){
  xnew <- cbind( rep( 1, nrow(xnew) ), xnew )
  h1 <- cbind( matrix( 1, nrow = nrow(xnew) ), sigmoid( xnew %*% nn$W1 )  )
  h2 <- cbind( matrix( 1, nrow = nrow(xnew) ), sigmoid( h1 %*% nn$W2 )  )
  y_hat <- sigmoid( h2 %*% nn$W3 )
  return( y_hat )
}

  ## STATYSTYKI ##


library("pROC")
MAE <- function(y_tar, y_hat){ return(mean(abs(y_tar - y_hat)))}
MSE <- function(y_tar, y_hat){ return(mean((y_tar - y_hat)^2))}
MAPE <- function(y_tar, y_hat){ return( mean(abs((y_tar - y_hat)/y_tar) ))}
#AUC_MT = suma(czulosc* specyficznosc t - specyficznosc t-1) + suma ((specyficznosc t - specyficznosc t-1)*(czulosc t - czulosc t-1))
AUC_MT <- function(y_tar, y_hat){
  k_roc <- roc(y_tar, y_hat)
  #dla poprawnosci wynikow czulosc i specyficznosc sortuje rosnaco
  sensit<- sort(k_roc$"sensitivities", F) 
  specif<- sort(1 - k_roc$"specificities", F) 
  diff_specif <- c(diff(sensit), 0)
  diff_sensit <- c(diff(specif), 0) 
  AUC_wynik <- sum(sensit * diff_sensit) + sum(diff_specif* diff_sensit)/2
  return(AUC_wynik)
}
#J = argt Czu³oœæ + Specyficznoœæ - 1
J <- function(y_tar, y_hat){
  k_roc <- roc(y_tar, y_hat)
  sensit <- k_roc$"sensitivities" 
  specif<- k_roc$"specificities"
  max_J <- 0
  for (i in 1:length(sensit)) {
    J <- (sensit[i] + specif[i]-1)
    if(J > max_J){
      max_J=J
    }
  }
  return(max_J)
}
Mat <- table(y_tar, y_hat = ifelse(y_hat <= J(y_tar, y_hat), 0, 1))
Czulosc <- function(Mat){ return((Mat[1] / (Mat[1] + Mat[3])))}
Specyficznosc <- function(Mat){ return((Mat[4] / (Mat[4] + Mat[2])))}
Jakosc <- function(Mat){ return(((Mat[1] + Mat[4]) / (Mat[1] + Mat[2] + Mat[3] + Mat[4])))}

ModelOcena <- function(y_tar, y_hat){
  if(is.numeric(y_tar)){
    regresja <- c("MAE" = MAE(y_tar, y_hat), "MSE" = MSE(y_tar, y_hat), "MAPE" = MAPE(y_tar, y_hat))
    return(regresja)
  }
  else if(is.factor(y_tar)){
    miary <- c( "AUC" = AUC(y_tar, y_hat), "Czu³oœæ" = Czulosc(Mat), "Specyficznoœæ" = Specyficznosc(Mat), "Jakoœæ" = Jakosc(Mat))
    klasyfikacja <- list(Mat, Youden(y_tar, y_hat), miary)
    return(klasyfikacja)
  }
  else{
    c("Dane niepoprawne")
  }
}


  ##  Kroswalidacja ##

CrossValidTune <- function(dane, kFold,parTune, seed){
  set.seed(seed)
  lista <- list()
  for (i in 1:kFold){
    #dziele probe na zbior testpwy i walidacyjny
    indxT<- sample(1:nrow(dane),
                   size = (1-1/kFold)*nrow(dane),
                   replace = F) 
    indxV <- (1:nrow(dane))[-indxT]
    lista[[i]] <- sample(1:nrow(dane),size = nrow(dane),replace = F) 
    for (j in 1:nrow(dane)) {
      for(k in 1:length(indxV)){
        if(j==indxV[k]){
          lista[[i]][[j]] = 2
          break
        }
        else{
          lista[[i]][[j]] = 1
        }
      }
    }
  }
  
  k <- rep(c(1:kFold), times=length(parTune))
  wyniki <- data.frame(k, parTune)
  
  if(is.numeric(dane[,1])){
    regresja_wyniki <- data.frame(wyniki, MAEt=0, MSEt=0, MAPEt=0, MAEw=0, MSEw=0, MAPEw=0  )
    return(regresja_wyniki)
  }
  else if(is.factor(dane[,1])){
    klasyfikacja_wyniki <- data.frame(wyniki, AUCT=0, Czu³oœæT=0, SpecyficznoœæT=0, JakoœæT=0,
                                      AUCW=0, SpecyficznoœæW=0, MAPEW=0, JakoœæW=0)
    return(klasyfikacja_wyniki)
  }
  else{
    c("Bledne dane!")
  }
  
}





