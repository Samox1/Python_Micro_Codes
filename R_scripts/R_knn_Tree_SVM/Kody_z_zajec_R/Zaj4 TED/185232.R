# Zadanie 1
Entropy <- function(prob){
  res <- prob * log2(prob)
  res[prob==0] <- 0
  res <- -sum(res)
  return(res)
}

SS <- function(n,Y){
  res <- (Y-(1/n) * sum (Y))^2
  res <- sum (res)
  return(res)
}

Gini <- function(prob){
  res <- prob^2
  res <- sum(res)
  return (res)
}


StopIfNot <- function(Y,X,data,type,depth,minobs,overfit,cf){
  if(
    if (is.data.frame(data) == FALSE){
      print("Dane nie sa ramka danych")
    }
    if (is.null(data$Y)|is.null(data$X)){
      print("Brak zmiennej Y lub X")
    }
    if (any(is.na(X) == TRUE | is.na(Y)==TRUE)){
      print( "Istnieja braki danych" )
    }
    if (depth <= 0 && minobs <= 0){
      print("Glebokosc lub minimalna liczba obserwacji mniejsza od 0")
    }
    if (type != "Gini"|type != "Entropy"|type != "SS"){
      print("Nieprawidlowa wartosc zmiennej 'type'")
    }
    if (overfit != "none" | overfit != "prune"){
      print("Nieprawidlowa wartosc zmiennej 'overfit'")
    }
    if (cf %in% c(0,0.5) == FALSE){
      print( "Zmienna 'cf' ma nieprawidlowa wartosc")
    }
    if (type == "SS" && is.factor(Y) ){
      print( "Warunek nie zostal spelniony" )
    }
  ) {
    return (FALSE)
  }
  else {
    return (TRUE)
  }
}

library(data.tree)

# Zadanie 2
AssignInitialMeasures <- function(tree,Y,data,type,depth){
  wynik = 0
  depth <- 0
  tree$Depth <- depth
  if (type=="Gini"){
    wynik = Gini(data$X)
  }
  else if (type=='Entropy'){
    wynik = Entropy(data$X)
  }
  else if (type=='SS'){
    wynik = SS(nrow(data$X),data$X)
  }
  return (wynik)
}

# Zadanie 3
AssignInfo <- function(tree,Y,X,data,type,depth,minobs,overfit,cf){
  attr(tree, 'Y') <- Y
  attr(tree, 'X') <- X
  attr(tree, 'data') <- data
  attr(tree, 'type') <- type
  attr(tree, 'depth') <- depth
  attr(tree, 'overfit') <- overfit
  attr(tree, 'cf') <- cf
}

# Zadanie 4
FindBestSplit <- function(Y,X,data,parentVal,type,depth,minobs){
  K <- !colnames(data) %in% Y
  res <- sapply(colnames(data)[K], function(i){
    SplitVar(data[,Y],data[,i],parentVal,minobs)
  },simplify=F)
  res <- do.call("rbind",res)
  best <- which.max(res$infGain)
  res <- res[best, ,drop=F]
  return(res)
}

# Zadanie 5
Tree <- function(Y,X,data,type,depth,minobs,overfit,cf){
  if (StopIfNot(Y,X,data,type,depth,minobs,overfit,cf) == FALSE) {
    StopIfNot(Y,X,data,type,depth,minobs,overfit,cf)
    return(0)
  }
  
  else{
    tree <- Node$new("Root")
    AssignInitialMeasures(tree,Y,data,type,depth)
    BuildTree <- function(){}
    PruneTree<-function(){}
    AssignInfo(tree,Y,X,data,type,depth,minobs,overfit,cf)
    return(tree)
  }
}

# Zadanie 6
BuildTree <- function(node,Y,X,data,depth,minobs){
  node$Count <- nrow(data)
  node$Prob <- Prob(data[,Y])
  bestSplit <- FindBestSplit(Y,X,data,node$inf,type,depth,minobs)
  ifStop <- nrow(bestSplit) == 0 
  if( node$Depth == depth | ifStop | all(node$Prob %in% c(0,1))){
    node$Leaf <- "*"
    return(node)
  }else{
    
    split_indx <- data[,rownames(bestSplit)] <= bestSplit$point
    child_frame <- split( data, split_indx )
    
    name <- sprintf( "%s <= %s", rownames(bestSplit),bestSplit$point)
    child_l <- node$AddChild(name)
    child_l$value <- split_indx
    child_l$Depth <- node$Depth + 1
    child_l$inf <- bestSplit$lVal
    
    BuildTree( child_l, Y, X, child_frame[[1]], depth, minobs )
    
    name <- sprintf( "%s >  %s", rownames(bestSplit),bestSplit$point)
    child_r <- node$AddChild(name)
    child_r$value <- split_indx
    child_r$Depth <- node$Depth + 1
    child_r$inf <- bestSplit$rVal
    
    BuildTree(child_r,Y,X,child_frame[[2]],depth,minobs)
  }
}