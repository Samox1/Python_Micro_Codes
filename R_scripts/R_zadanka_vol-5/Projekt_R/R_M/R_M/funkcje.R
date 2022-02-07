library(dplyr)
library(pROC)
library(parallel)
library(doParallel)
library(foreach)
library(iterators)
library(data.tree)
library(caret)
library(rpart)
library(nnet)
library(ggplot2)



# Sieci Neuronowe

sigmoid <- function( x ) {
  return( 1 / (1 + exp( -x ) ) )
}

dsigmoid <- function( x ) {
  return( x * (1 - x) )
}

ReLu <- function( x ) {
  return( ifelse( x <= 0, 0, x ) )
}

dReLu <- function( x ) {
  return( ifelse( x <= 0, 0, 1 ) )
}

lossSS <- function( y_tar, y_hat ) {
  return( 1/2 * sum( ( y_tar - y_hat )^2 ) )
}

SoftMax <- function(x) {
  exp( x ) / sum( exp( x ) )
}

MinMax_bez_przedzialu <- function( x ) {
  return( ( x - min(x) ) / ( max(x) - min(x) ) )
}  

MinMaxOdwrot <- function( x, y_min, y_max ) {
  return(  x * (y_max - y_min) + y_min )
}  


forward_prop <- function(X, W, type){
  H <- list()

  H[[1]] <- cbind( matrix( 1, nrow = nrow(X) ), sigmoid( X %*% W[[1]] ))
  
  if(length(W) > 2){
    for(i in 1:(length(W)-2)){
      H[[i+1]] <- cbind( matrix( 1, nrow = nrow(X) ), sigmoid( H[[i]] %*% W[[i+1]] )  )
    }
  }
  
  if(type == 'bin'){
    y_hat <- sigmoid( H[[length(H)]] %*% W[[length(W)]] )
  }else if(type == 'multi'){
    y_hat <- matrix( t( apply( H[[length(H)]] %*% W[[length(W)]], 1, SoftMax ) ), nrow = nrow(X))
  }else if(type == 'reg'){
    y_hat <- ( H[[length(H)]] %*% W[[length(W)]] ) 
  }
  
  return(list(y_hat = y_hat, H = H))
}#wprzod



backword_prop <- function(Y, X, Y_hat, W, H, lr, type){

  if (type =="bin"){
    dy_hat <- (Y - Y_hat) * dsigmoid(Y_hat)
  }
  else if (type == "multi"){
    dy_hat <- (Y - Y_hat) / nrow(X)       
  } 
  else if (type == "reg"){
    dy_hat <- (Y - Y_hat)
  }
  
  dH = list()
  dW = list()
  
  dW[[length(W)]] <- t(H[[length(H)]]) %*% dy_hat
  dH[[length(H)]] <- dy_hat %*% t(W[[length(W)]]) * dsigmoid(H[[length(H)]])
  
  if(length(W)>2){
    for(i in (length(W)-1):2){
      dW[[i]]   <- t(H[[i-1]]) %*% dH[[i]][, -1]
      dH[[i-1]] <- dH[[i]][, -1] %*% t(W[[i]]) * dsigmoid (H[[i - 1]])
    }
  }
  dW[[1]] <- t(X) %*% dH[[1]][, -1]
  
  for(i in 1:length(W)){
    W[[i]] <- W[[i]] + lr * dW[[i]]
  }
  
  return(W)
}#wstecz

trainNN<- function( Yname, Xnames, data, h, lr, iter, seed, type){

  set.seed(seed)
  h <- unlist(h, use.names = FALSE)
  X <- (data[, Xnames])
  X <- as.matrix(cbind( rep( 1, nrow(X) ), X )) #WSP
  y_tar <- as.matrix(data[, Yname])
  
  W = list()
  W[[1]] <- matrix( runif( ncol(X) * h[1], -1, 1 ), nrow = ncol(X) )
  
  if(length(h)>1){
    for(i in 2:(length(h))){
      # W_in lists
      W[[i]] <- matrix( runif( (h[i-1]+1) * h[i], -1, 1 ), nrow = h[i-1] + 1 )
    }
  }
  
  W[[length(h)+1]] <- matrix( runif( (h[length(h)]+1) * ncol(y_tar), -1, 1 ), nrow = h[length(h)] + 1 )
  
  # error <- double( iter )
  
  for( i in 1:iter ){
    sygnalwprzod <- forward_prop( X, W , type)
    sygnalwtyl   <- backword_prop( y_tar, X, Y_hat = sygnalwprzod$y_hat, W, H = sygnalwprzod$H, lr , type)
    W <- sygnalwtyl

    cat( paste0( "\rIteracja: ", i ) )
    # error[i] <- lossSS( Y, sygnalwprzod$y_hat )
  }
  # xwartosci <- seq( 1, iter, length = 1000 )
  # print( qplot( xwartosci, error[xwartosci], geom = "line", main = "Error", xlab = "Iteracje" ) )
  
  return(list( y_hat = sygnalwprzod$y_hat, Wagi = W ))
} #trainNN



predNN <- function(xnew, NN, type){
  xnew <- cbind(rep(1, nrow(xnew)), xnew)

  H <- list()
  H[[1]] <- cbind(matrix(1, nrow = nrow(xnew)), sigmoid(xnew %*% NN$W[[1]]))
  
  if(length(NN$W) > 2){
    for (i in 2:(length(NN$W)-1)){
      H[[i]] <- cbind(matrix(1, nrow = nrow(xnew)), sigmoid(H[[i-1]] %*% NN$W[[i]] ))
    }
  }

  y_hat <- sigmoid( H[[length(H)]] %*% NN$W[[length(NN$W)]] )
  
  if (type =="bin"){
    y_hat <- sigmoid( H[[length(H)]] %*% NN$W[[length(NN$W)]])                 
  }
  else if (type == "multi"){
    y_hat <- matrix(t(apply( H[[length(H)]] %*% NN$W[[length(NN$W)]], 1, SoftMax)), nrow = nrow(xnew))
  } 
  else if (type == "reg"){
    y_hat <- H[[length(H)]] %*% NN$W[[length(NN$W)]] 
  }
  
  
  if(is.factor(NN$y_hat)){
    
  }
  
  return(y_hat)
}



# Drzewa decyzyjne

StopIfNot <- function(Y,X,data,type,depth,minobs,overfit,cf) {
  if (!is.data.frame(data)) {
    print("Dane nie sa ramka danych!")
    return (FALSE)
  }
  else if (any(!X %in% colnames(data) | !Y %in% colnames(data))) {
    print("Braki danych, sprawdz dane wejsciowe!")
    return (FALSE)
  }
  else if (anyNA(data[,Y]) | anyNA(data[,X])) {
    print(" Wartosci zmiennych Y lub X nie istnieja w zbiorze danych!")
    return (FALSE)
  }
  else if (depth<0 | minobs<0) {
    print("Niedopuszczalne wartosci depth lub minobs, mniejsze od zera!")
    return (FALSE)
  }
  else if (type != "Gini" & type != "Entropy" & type != "SS") {
    print("Typ niemozliwy do wykonania")
    return (FALSE)
  }
  else if (overfit != "prune" & overfit != "none") {
    print("Overfit przyjmuje niedopuszczalne wartosci")
    return (FALSE)
  }
  else if (cf<=0 | cf>0.5) {
    print("Niedopuszczalny przedzial cf ")
    return (FALSE)
  }
  else if (is.factor(data[,Y]) & type == "SS") {
    print("Niedopuszczalna kombinacja parametrow! Type SS, gdy Y jest faktorem.")
    return (FALSE)
  }
  else if (!is.factor(data[,Y]) & (type == "Entropy" | type == "Gini")) {
    print("Niedopuszczalna kombinacja parametrow! Type Gini lub Entropy, gdy dane nie sa faktorem")
    return (FALSE)
  }
  else {
    #RETURN("Operacja mozliwa do wykonania!")
    #print("Operacja mozliwa do wykonania!"
    return (TRUE)
  }
} #StopIfNot

Prob <- function(y_tar) {
  result <- unname(table(y_tar))
  result <- result/sum(result)
  return(result)
}

Entropy <- function(prob){
  if (any(prob > 1 | prob < 0) == TRUE) {
    stop("Miara prawdopodobienstwa MUSI zawierac sie w przedziale [0,1]! Nie zawiera sie, wprowadz jeszcze raz")
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
    stop("Miara prawdopodobienstwa MUSI zawierac sie w przedziale [0,1]! Nie zawiera sie, wprowadz jeszcze raz")
  }
  else {
    result <- 1 - sum(prob^2)
    return (result)
  }
}

SS <- function(y) {
  if (is.factor(y)) {
    stop("Niemozliwe do wykonania. Dziala jedynie dla regresji. Wprowadzono factor, SS jest tylko do regresji")
  }
  else {
    y_1 <- sum(y)/length(y)
    y_2 <- (y-y_1)^2
    result <- sum(y_2)
    return (result)
  }
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
  else if (type == "SS") {
    SS <- SS(data[,Y])
    tree$inf <- SS
  }
  else {
    stop("wrong type!")
  }
  return (tree)
}#AssignInitialMeasures

AssignInfo <- function(tree,Y,X,data,type,depth, minobs, overfit, cf ) {
  attr(tree, "Y") <- data[,Y]
  attr(tree, "X") <- data[,X]
  attr(tree, "data") <- data
  attr(tree, "type") <- type
  attr(tree, "depth") <- depth
  attr(tree, "minobs") <- minobs
  attr(tree, "overfit") <- overfit
  attr(tree, "cf")<- cf
  return (tree)
}


SplitNum <- function(Y, x, parentVal, splits, minobs, type) {
  
  n <- length( x )
  res <- data.frame( matrix( 0, length(splits), 6 ) )
  colnames( res ) <- c("InfGain","lVal","rVal","point","ln","rn")
  
  for( i in 1:length(splits) ) {
    
    if (is.numeric(x)) {
      partition <- x <= splits[i] # %in%: x %in% c("wyzsze","podstawowe")
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
    } #else
  } #for
  return(res)
} #SplitNum


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
}# SpliVar

FindBestSplit <- function(Y, X, data, parentVal, type, minobs) {
  res <- sapply(X, function(i) {
    
    # if (is.numeric(data[,i]) | is.ordered(data[,i])) {
    #   d <- as.numeric(paste(data[,i]))
    # }
    if (is.numeric(data[,i])) {
      d <- data[,i]
    }
    else if (is.ordered(data[,i])) {
      d <- as.numeric(paste(data[,i]))
    }
    else {
      if (is.factor(data[,Y])) {
        temp <- data[data[,Y] == 1,]
        a <- prop.table(table(temp[,i]))
        a <- sort(a)
        d <- (paste(factor(data[,i], levels = names(a), ordered = TRUE)))
      }
      else {
        a <- tapply(data[,Y], data[,i], mean)
        a <- sort(a)
        d <- (paste(factor(data[,i], levels = names(a), ordered = TRUE)))
      }
    }
    SplitVar(data[,Y], d, parentVal, minobs, type)
  }, simplify = F ) #function res
  
  res <- do.call("rbind", res)
  
  best <- which.max(res$InfGain)
  res <- res[best, , drop = F]
  
  return (res)
} #FindBestSplit



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
  
  bestSplit <- FindBestSplit(Y, X, data, node$inf, type, minobs) #POLACZENIE FindBestSplit
  
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
} #BuildTree


Tree <- function(Y, X, data, type, depth, minobs, overfit = "none", cf = "0.2") {
  
  if (StopIfNot(Y, X, data, type, depth, minobs, overfit,cf) == FALSE) {
    return(FALSE)
  }
  
  #STRING W FACTOR
  # for (i in 1:length(X)) {
  #   if (is.factor(data[,i])) {
  #     data[,i] <- as.character(data[,i])
  #   }
  #   else {
  #     next
  #   }
  # }
  
  print("KAPPA")
  
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
  
  BuildTree(tree, Y, X, data, depth, type, minobs)  #BuildTree
  
  if(overfit == 'prune')
  {
    PruneTree(tree = tree, cf = cf)
  }
  
  AssignInfo(tree,Y,X,data,type,depth, minobs, overfit, cf ) #AssignTree
  
  return(tree)
} #Tree


obsPred <- function(tree, obs) {
  
  if (tree$isLeaf) {
    # print(tree$Prob)
    return(c(as.numeric(as.character(tree$Prob)), "Class" = tree$Class))
  }
  if (is.numeric(tree$children[[1]]$BestSplit) | is.ordered(tree$children[[1]]$BestSplit)) {
    
    child <- tree$children[[ifelse(obs[,tree$children[[1]]$feature] > (tree$children[[1]]$BestSplit), 2, 1)]]
  }
  else {
    split <- tree$children[[1]]$feature
    child <- tree$children[[ifelse((obs[,tree$children[[1]]$feature] %in% split), 1, 2)]]
  }
  
  return (obsPred(child,obs))
} #obsPred


PredictTree <- function(tree, data) {
  
  if (is.factor(attributes(tree)$Y)) {
    out <- c()
    
    for (i in 1:nrow(data)){
      out <- rbind(out, (obsPred(tree, data[i, ,drop = F])))
    }
    
    # out[,-ncol(out)] <- as.numeric(out[,-ncol(out)])
    
  }
  else {
    out <- c()
    
    for (i in 1:nrow(data)) {
      out[i] <- as.numeric(obsPred(tree, data[i, ,drop = F])["Class"])
    }
  }
  return (out)

} #PredictTree





# KNN

MinMax <- function( x, new_min = 0, new_max = 1 ){
  return( ( ( x - min(x) ) / ( max(x) - min(x) ) ) * ( new_max - new_min ) + new_min )
}
# set.seed(123)
# y <- runif(100, min = 1, max = 2)
# summary(y) # wywołuje informacje statystki dla wszystkich wartości 
# summary( MinMax(y)) # sprawdzamy MinMax pracując na y
# summary( MinMax(y, 10, 25)) # sprawdzamy MinMax pracując na innej skali od 10 do 25

#NORMALIZACJA ZScore
ZScore <- function(x) {
  return ((x - mean(x))/sd(x))  
}



#wyszukanie mody dla zbioru, dominanta
getMode <- function(x) {
  distinct <- unique(x)
  distinct_max <- distinct[which.max(tabulate(match(x,distinct)))]
  return (distinct_max)
}

#wyszukanie unikat
getDistinct <- function(x) {
  return (unique(x))
}


#wzory wyklad
#skala ilorazowa
d_euklides <- function( x_i, x_n ){
  return( sqrt( sum( ( x_i - x_n )^2 ) ) )
}

#skala porzadkowa
d_czebyszew <- function(x_i, x_n) {
  return (max(abs(x_i - x_n)))
}

d_interval <- function(moc, x_i, x_n) {
  return (sum(abs(x_i - x_n)/(moc - 1)))
}

#skala nominalna
d_hamming <- function(x_i, x_n) {
  return (sum(x_i != x_n)/length(x_i))
}

#Gower <- MIESZANE
d_gower <- function(dane,x_i,x_n) {
  if (length(x_i) != length(x_n)) {
    stop("Nieprawidlowe dlugosci wektorow ")
  }
  else {
    wynik <- 0
    for (i in 1:length(x_i)) {
      if (all(sapply(x_i[i],class) == "numeric") | any(sapply(x_i[i],class) == "ordered")) {
        odl <- abs(as.numeric(paste(x_i[i])) - as.numeric(paste(x_n[i])))/(max(as.numeric(paste(dane[[names(x_i[i])]]))) - as.numeric(paste(min(dane[[names(x_n[i])]]))))
        wynik <- wynik + odl
      }
      else {
        odl <- sum(x_i[i] != x_n[i])
        wynik <- wynik + odl
      }
    }
    return (as.numeric(wynik)/length(x_i))
  }
}


KNNtrain <- function(X, y_tar, k = 2, XminNew = 0, XmaxNew = 1){
  if(k <=0 ){
    stop("Analiza nie jest możliwa do wykonania. Liczba sasiadow niepoprawnie wprowadzona, mniejsza lub rowna 0")
  }
  else if(!is.matrix(X) & !is.data.frame(X)){
    stop("Analiza nie jest mozliwa do wykonania. Wektor wejsciowy nie jest ani macierza, ani ramka danych.")
  }
  else if(anyNA(X) | anyNA(y_tar)){
    stop("Analiza nie jest mozliwa do wykonania. Brakujace dane. Uzupelnij zbior danych" )
  }
  
  # else if(nrow(X) != length(y_tar)){
  #   stop("Ilosc Wartosci prognozowanych jest rozna od ilosci zmiennych objasniajacych")
  # }
  
  else{
    macierz <- matrix(0, nrow = nrow(X), ncol = ncol(X))
    ramka_danych <- data.frame(macierz)
    
    minOrg <- c()
    maxOrg <- c()
    minmaxNew <- c()
    
    for (i in 1:length(X)) {
      if (all(sapply(X[i], class) == "numeric")) {
        minOrg <- append(minOrg, min(X[i]))
        maxOrg <- append(maxOrg, max(X[i]))
        X[i] <- as.vector(unlist(MinMax(X[i], XminNew,XmaxNew)))
        minmax <- c(c(min(X[i]),max(X[i])))
        names(minmax) <- c( paste( colnames(X[i]), "min", sep="_"), paste(colnames(X[i]), "max", sep="_"))
        minmaxNew <- append(minmaxNew, minmax)
      }
      else if (all(sapply(X[i], class) == "factor")) {
        maxOrg <- append(maxOrg,max(as.vector(as.numeric(unlist(X[i])))))
        minOrg <- append(minOrg,min(as.vector(as.numeric(unlist(X[i])))))
        X[i] <- factor(as.vector(unlist(X[i])))
      }
      else {
        maxOrg <- append(maxOrg,max(as.vector(unlist(X[i]))))
        minOrg <- append(minOrg,min(as.vector(unlist(X[i]))))
        X[i] <- (X[i])
      }
    }
    
    names(maxOrg) <- colnames(X)
    names(minOrg) <- colnames(X)
    attr(X,paste("minOrg")) <- minOrg
    attr(X,paste("maxOrg")) <- maxOrg
    attr(X,paste("minmaxNew")) <- minmaxNew
    knn_lista <- list("X" = X, "y_tar" = y_tar, "k" = k)
    
    return (knn_lista)
  }
  
}#KNNtrain



KNNpred <- function(KNNmodel, X, Ncores = 1){
  nazwyKolumn <- colnames(KNNmodel) == colnames(X)
  if(any(is.na(X))){
    stop("Brak danych! Przewidywanie niemozliwe do wykonania,")
  }
  #  Dodanie warunku
  else if(all(nazwyKolumn) == FALSE){
    stop("Kolumny niepoprawnie")
  }
  else{#poprawne wartosci
    colTypes <- unique(unlist(lapply(X, class)))
    
    cores <- detectCores()
    if(Ncores > cores){
      stop("Wprawdza liczbe rdzeni jeszcze raz, jest za duza dla tego komputera!")
    }
    
    cl <- makeCluster(Ncores, outfile = "")
    registerDoParallel(cl)
    
    clusterExport(cl,"d_euklides")
    clusterExport(cl,"getMode")
    clusterExport(cl,"getDistinct")
    clusterExport(cl,"d_hamming")
    clusterExport(cl,"d_gower")
    clusterExport(cl,"d_czebyszew")
    clusterExport(cl,"d_interval")
    clusterExport(cl,"KNNmodel",envir = environment())
    clusterExport(cl,"X",envir = environment())
    
    
    # SKALA PORZADKOWA
    if(isTRUE(all.equal(c("ordered", "factor"), colTypes))) {
      #porzadkowa
      KNNmodel$X <- sapply(KNNmodel$X, as.numeric)
      X <- sapply(X, as.numeric)
      y <- KNNmodel$y
      
      
      if (is.factor(y)) { 
        print("KLASYFIKACJA dla skali porzadkowej")
        nTrain <- nrow(KNNmodel$X)        
        nPred <- nrow(X)
        
        clusterExport(cl, "nTrain", envir = environment())
        clusterExport(cl, "nPred", envir = environment())
        
        odl <- parApply(cl, KNNmodel$X, 1, function(x) 
          sapply(1:nPred, function(j) d_czebyszew(x, X[j,])))
        
        pred <- double(nPred)
        out <- c()##################################
        clusterExport(cl,"pred",envir = environment())
        clusterExport(cl,"out",envir = environment())
        
        out <- (parSapply(cl, 1:nPred, function(i) {
          kNaj <- order(odl[,i])
          kNaj <- kNaj[1:KNNmodel$k]
          pred[i] <- getMode(KNNmodel$y[kNaj])
          tab <- as.data.frame(table(KNNmodel$y[kNaj]))
          p <- data.frame(tab, "p" = tab$Freq/sum(tab$Freq))
          pr <- rbind(p$p)
          out <- rbind(out,as.vector(pr))
        }))
        
        out <- as.data.frame(t(out))
        out <- out[1:length(unique(KNNmodel$y))]
        colnames(out) <- lapply(1:length(unique(KNNmodel$y)), function(i) 
          paste(sort(unique(KNNmodel$y))[i],sep = "_"))
        out$max <- apply(out, 1, max)
        pred <- colnames(out)[max.col(out, ties.method = "first")]
        drops <- c("max")
        out <- out[ , !(names(out) %in% drops)]
        
        return (data.frame("y_tar" = KNNmodel$y,out,y_hat = pred))
      }#is factor     
      
      
      else {
        print("REGRESJA dla skali porzadkowej")
        nTrain <- nrow(KNNmodel$X)
        nPred <- nrow(X)
        
        clusterExport(cl,"nTrain",envir = environment())
        clusterExport(cl,"nPred",envir = environment())
        
        odl <- parApply(cl, KNNmodel$X, 1, function(x) 
          sapply(1:nPred, function(j) d_czebyszew(x, X[j,])))
        
        out <- (parLapply(cl, 1:nPred, function(i) {
          kNaj <- order(odl[,i])
          kNaj <- kNaj[1:KNNmodel$k]
          pred <- mean(KNNmodel$y[kNaj])
        }))
        
        return (unlist(out))
      }#porzadkowa, nie facto,r KLASYFIKACJA
    }#SKALA PORZADKOWA
    
    #SKALA ILORAZOWA
    else if(isTRUE(all.equal("numeric",colTypes))) {
      
      y <- KNNmodel$y
      clusterExport(cl,"y",envir = environment())
      
      #KLASYFIKACJA SKALI ILORAZOWEJ
      if (is.factor(y)) {
        print("Dzialania dla skali ilorazowej, odlegloscia euklidesa, zagadneinie KLASYFIKACJI")
        nTrain <- nrow(KNNmodel$X)
        nPred <- nrow(X)
        
        clusterExport(cl, "nTrain", envir = environment())
        clusterExport(cl, "nPred", envir = environment())
        
        odl <- parApply(cl, KNNmodel$X, 1, function(x) 
          sapply(1:nPred, function(j) d_euklides(x, X[j,])))
        
        pred <- double(nPred)
        out <- c()
        clusterExport(cl,"pred",envir = environment())
        clusterExport(cl,"out",envir = environment())
        
        out <- (parSapply(cl, 1:nPred, function(i) {
          kNaj <- order(odl[,i])
          kNaj <- kNaj[1:KNNmodel$k]
          pred[i] <- getMode(KNNmodel$y[kNaj])
          tab <- as.data.frame(table(KNNmodel$y[kNaj]))
          p <- data.frame(tab, "p" = tab$Freq/sum(tab$Freq))
          pr <- rbind(p$p)
          out <- rbind(out,as.vector(pr))
        }))
        
        
        out <- as.data.frame(t(out))
        out <- out[1:length(unique(KNNmodel$y))]
        colnames(out) <- lapply(1:length(unique(KNNmodel$y)), function(i) paste(sort(unique(KNNmodel$y))[i],sep = "_"))
        out$max <- apply(out, 1, max)
        pred <- colnames(out)[max.col(out, ties.method = "first")]
        drops <- c("max")
        out <- out[ , !(names(out) %in% drops)]
        
        return (data.frame("y_tar" = KNNmodel$y,out,y_hat = pred))
      }# KLASYFIKACJA DLA SKALI ILORAZOWEJ
      
      #REGRESJA na skali ilorazowej
      else{
        print("Dla skali ilorazowej, zagadnienie regresji, odleglosc euklidesa")
        nTrain <- nrow(KNNmodel$X)
        nPred <- nrow(X)
        
        clusterExport(cl,"nTrain",envir = environment())
        clusterExport(cl,"nPred",envir = environment())
        
        
        odl <- parApply(cl, KNNmodel$X, 1, function(x) 
          sapply(1:nPred, function(j) d_euklides(x, X[j,])))
        
        out <- (parLapply(cl, 1:nPred, function(i) {
          kNaj <- order(odl[,i])
          kNaj <- kNaj[1:KNNmodel$k]
          pred <- mean(KNNmodel$y[kNaj])
        }))
        
        return (unlist(out))
      }#REGRESJA na skali ilorazowej
    }# SKALA ILORAZOWA
    
    #SKALA NOMINALNA
    else if(isTRUE(all.equal("factor",colTypes))) {
      y <- KNNmodel$y
      
      #KLASYFIKACJA skali nominalnej
      if (is.factor(y)) {
        print("Klasyfikacja skali nominalnej, odleglosc Hamminga")
        nTrain <- nrow(KNNmodel$X)
        nPred <- nrow(X)
        
        clusterExport(cl,"nTrain",envir = environment())
        clusterExport(cl,"nPred",envir = environment())
        
        odl <- parApply(cl, KNNmodel$X, 1, function(x) 
          sapply(1:nPred, function(j) d_hamming(x, X[j,])))
        
        pred <- double(nPred)
        out <- c()
        clusterExport(cl,"pred",envir = environment())
        clusterExport(cl,"out",envir = environment())
        
        out <- (parSapply(cl, 1:nPred, function(i) {
          kNaj <- order(odl[,i])
          kNaj <- kNaj[1:KNNmodel$k]
          pred[i] <- getMode(KNNmodel$y[kNaj])
          tab <- as.data.frame(table(KNNmodel$y[kNaj]))
          p <- data.frame(tab, "p" = tab$Freq/sum(tab$Freq))
          pr <- rbind(p$p)
          out <- rbind(out,as.vector(pr))
        }))
        
        out <- as.data.frame(t(out))
        out <- out[1:length(unique(KNNmodel$y))]
        colnames(out) <- lapply(1:length(unique(KNNmodel$y)), function(i) paste(sort(unique(KNNmodel$y))[i],sep = "_"))
        out$max <- apply(out, 1, max)
        pred <- colnames(out)[max.col(out, ties.method = "first")]
        drops <- c("max")
        out <- out[ , !(names(out) %in% drops)]
        
        return (data.frame("y_tar" = KNNmodel$y,out,y_hat = pred))
      }# KLASYFIKACJA SKALI NOMINALNEJ
      
      #REGRESJA skala nominalna
      else {
        print("Regresja dla skali nominalnej, odleglosc Hamminga")
        nTrain <- nrow(KNNmodel$X)
        nPred <- nrow(X)
        
        clusterExport(cl,"nTrain",envir = environment())
        clusterExport(cl,"nPred",envir = environment())
        
        odl <- parApply(cl, KNNmodel$X, 1, function(x) 
          sapply(1:nPred, function(j) d_hamming(x, X[j,])))
        
        out <- (parLapply(cl, 1:nPred, function(i) {
          kNaj <- order(odl[,i])
          kNaj <- kNaj[1:KNNmodel$k]
          pred <- mean(KNNmodel$y[kNaj])
        }))
        
        return (unlist(out))
      }#REGRESJA skala nominalna
    }#SKALA NOMINALNA
    
    #SKALA MIESZANA
    else{
      y <- KNNmodel$y
      
      #KLASYFIKACJA mieszana
      if (is.factor(y)) {
        print("Zagadnienie KLASYFIKACJI dla skali mieszanej przy uzyciu odleglosci GOWERA")
        nTrain <- nrow(KNNmodel$X)
        nPred <- nrow(X)
        
        clusterExport(cl,"nTrain",envir = environment())
        clusterExport(cl,"nPred",envir = environment())
        clusterExport(cl,"y",envir = environment())
        
        odl <- parApply(cl, KNNmodel$X, 1, function(x) 
          sapply(1:nPred, function(j)
            d_gower(KNNmodel$X, x, X[j,])))
        
        pred <- double(nPred)
        out <- c()
        clusterExport(cl,"pred",envir = environment())
        clusterExport(cl,"out",envir = environment())
        
        out <- (parSapply(cl, 1:nPred, function(i) {
          kNaj <- order(odl[,i])
          kNaj <- kNaj[1:KNNmodel$k]
          pred[i] <- getMode(KNNmodel$y[kNaj])
          tab <- as.data.frame(table(KNNmodel$y[kNaj]))
          p <- data.frame(tab, "p" = tab$Freq/sum(tab$Freq))
          pr <- rbind(p$p)
          out <- rbind(out,as.vector(pr))
        }))
        
        out <- as.data.frame(t(out))
        out <- out[1:length(unique(KNNmodel$y))]
        colnames(out) <- lapply(1:length(unique(KNNmodel$y)), function(i) paste(sort(unique(KNNmodel$y))[i],sep = "_"))
        out$max <- apply(out, 1, max)
        pred <- colnames(out)[max.col(out, ties.method = "first")]
        drops <- c("max")
        out <- out[ , !(names(out) %in% drops)]
        
        return (data.frame("y_tar" = KNNmodel$y,out,y_hat = pred))
      }# KLASYFIKACJA skala mieszana
      
      #REGRESJA MIESZANA
      else {
        print("Zagadnienie REGRESJI dla skali mieszanej przy uzyciu odleglosci GOWERA")
        nTrain <- nrow(KNNmodel$X)
        nPred <- nrow(X)
        
        clusterExport(cl,"nTrain",envir = environment())
        clusterExport(cl,"nPred",envir = environment())
        
        odl <- parApply(cl, KNNmodel$X, 1, function(x) 
          sapply(1:nPred, function(j) d_gower(KNNmodel$X, x, X[j,])))
        
        out <- (parLapply(cl, 1:nPred, function(i) {
          kNaj <- order(odl[,i])
          kNaj <- kNaj[1:KNNmodel$k]
          pred <- mean(KNNmodel$y[kNaj])
        }))
        
        return (unlist(out))
      }# Regresja dla mieszanej
    }# SKALA MIESZANA
  }# else zadania
  
  stopCluster(cl)
  
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
  
}#KNNpred






# Statystyka i kroswalidacja


MAE <- function(y_t, y_h){
  return (mean( abs(y_t - y_h)))
}

MSE <- function(y_t, y_h){
  return (mean((y_t - y_h)^2))
}

MAPE <- function(y_t, y_h){
  return (mean(abs((y_t - y_h)/y_t)))
}

AUC  <- function(y_t, y_h){
  krzywa_roc <- roc(y_t, y_h, quiet = TRUE)
  
  y <- rev(krzywa_roc$sensitivities)
  x <- rev(1 - krzywa_roc$specificities)
  
  # diff difference czyli r??niczka [0,1] dla ROC(t)
  dy <- c(diff(y), 0)
  dx <- c(diff(x), 0)
  
  # pole trapezu ( a + b) * h) / 2
  suma_trapezow <- sum(y * dx) + sum(dy * dx)/2
  return(suma_trapezow)
}


Youden_index <- function(y_tar, y_hat){
  roc_wyzn <- roc(y_tar, y_hat, quiet = TRUE)
  
  y <- roc_wyzn$sensitivities #Czulosc
  x <- roc_wyzn$specificities #Specyficznosc
  
  # J = argt Czu?o?? + Specyficzno?? - 1
  J_max <- 0
  for ( i in 1:length(y)){
    J <- y[i] + x[i] -1
    if(J > J_max) {
      J_max = J
    }
  }
  return(J_max)
}

Czulosc <- function(Mat){# TP / (TP+FN)
  wynik_cz <- (Mat[1] / (Mat [ 1 ] + Mat[3]))
  return (wynik_cz)
}

Specyficznosc <- function (Mat){ # TN / (FP + TN)
  wynik_s <- Mat[4] / (Mat[2] + Mat[4])
  return(wynik_s)
}

Jakosc <- function(Mat){ # TP + TN / WSZYSTKO
  wynik_j <- sum(diag(Mat))/sum(Mat)  
  return(wynik_j)
}

Trafienia <- function(y_tar, y_hat){
  wynik_trafienia <- sum(as.numeric(y_tar) == as.numeric(y_hat)) / length(y_tar) 
  return(wynik_trafienia)
}


ModelOcena <- function (y_tar, y_hat){                                                    
  if (is.numeric(y_tar))
  { 
    regresja_wynik <- c("MAE" = MAE(y_tar, y_hat),
                        "MSE" = MSE(y_tar, y_hat),
                        "MAPE" = MAPE( y_tar, y_hat)) 
    return(regresja_wynik)
    
  }
  else if(is.factor(y_tar) & nlevels(y_tar) == 2)
  {
    
    Mat <- table(y_tar, y_hat = ifelse(y_hat <= Youden_index(y_tar, y_hat), 0, 1))
    klasyfikacja_wynik <- list( Mat,
                                Youden_index(y_tar, y_hat),
                                c("AUC" = AUC(y_tar, y_hat),
                                  "Czulosc" = Czulosc(Mat),
                                  "Specyficznosc" = Specyficznosc( Mat),
                                  "Jakosc" = Jakosc(Mat))) 
    return(klasyfikacja_wynik)
  }
  else if(is.factor(y_tar) & nlevels(y_tar) > 2)
  {
    y_hat <- factor(y_hat, levels = levels(y_tar))
    
    klasyfikacja_wielo_wynik <- list("Trafienia" = Trafienia(y_tar, y_hat), "Mat" = table(y_tar, y_hat))
    
    # print(table(y_tar, y_hat))
    
    return(klasyfikacja_wielo_wynik)
  }
  else
  { 
    return("Niepoprawne wprowadzenie danych, problem nie jest problemem regresi lub klasyfikacji. Wybierz inny zbior danych.")
  }
  
}#ModelOcena







CrossValidTune <- function(dane_Y, dane_X, dane, kFold, parTune, seed)
{
  set.seed(seed) #losowe
  n = nrow(dane) # dlugosc wektora wchodzacego w sklad listy
  wektor <- c() #pusty wektor przypisywania
  lista <- list()
  
  for ( i in 1:kFold){                                                              #dziele probe na zbior testpwy i walidacyjny
    index <- sample( x = 1:n, size = round((1-1/kFold) * n), replace = F )          #bez powtorzen, tylko raz wybieramy obiekt
    
    index_sort = sort(index, decreasing = FALSE) #sort niemalej?co
    lista[[i]] <- sample(1:nrow(dane),size = nrow(dane),replace = F)
    
    for(j in 1:n){
      for(k in 1:length(index_sort)){
        if(j == index_sort[k]){
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
  
  print(wyniki)                                                                                             # OBEJRZEC CO TO WYRZUCA
  
  if(is.numeric(dane[,dane_Y])){
    wyniki_regresja <- data.frame(wyniki, MAEt=0, MSEt=0, MAPEt=0, MAEw=0, MSEw=0, MAPEw=0)
    
    
    
    return(wyniki_regresja)
  }
  else if(is.factor(dane[,dane_Y]) & nlevels(dane[,dane_Y]) == 2){
    wyniki_klasyfikacja <- data.frame(wyniki, AUCT=0, CzuloscT=0, SpecyficznoscT=0, JakoscT=0,
                                      AUCW=0, SpecyficznoscW=0, SpecyficznoscW=0, JakoscW=0)
    
    
    
    return(wyniki_klasyfikacja)
  }
  else if(is.factor(dane[,dane_Y]) & nlevels(dane[,dane_Y]) > 2){
    wyniki_klasyfikacja_wielo <- data.frame(wyniki, JakoscT=0, JakoscW=0)
    
    
    
    return(wyniki_klasyfikacja_wielo)
  }
  else{
    return("WPROWADZENIE NIEPOPRAWNYCH DANYCH!")
  }
  
  
}#funkcja glownaCrossValidTune






