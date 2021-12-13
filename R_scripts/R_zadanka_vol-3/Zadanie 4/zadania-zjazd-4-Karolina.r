StopIfNot <- function(Y, X, data, type, depth, minobs, overfit, cf){
  
  if( (class(data)=="data.frame") == FALSE){
    cat("Dane to nie ramka danych", sep="\n")
  }
  if(is.null(data$Y)|is.null(data$X) ){
    cat("'X' lub 'Y' nie wystepuje w tabeli", sep="\n")
  }
  if(any(is.na(X) == TRUE | is.na(Y)==TRUE)){
    cat("Y lub X ma braki danych", sep="\n")
  }
  
  if(depth <= 0 | minobs <= 0){
    cat("depth lub minobs jest mniejsze od 0", sep="\n")
  }
  if((type == "Gini" || type == "SS" || type=="Entropy") == FALSE){
    cat("type nie jest  Gini,  Entropy , SS", sep="\n")
  }
  if((overfit == "none" || overfit =="prune") == FALSE){
    cat("overfit nie jest none ,nie jest  prune", sep="\n")
  }
  if((cf >0 && cf <=0.5) == FALSE){
    cat("cf nie nalezy do przedzialu (0,0.5]", sep="\n")
  }
  if( (class(Y)=="factor" && type=="SS") || (class(Y)=="numeric" && type=="Gini") || (class(Y)=="numeric" && type == "Entropy")==TRUE){
    cat("kombinacja parametrow Y i type nie ma sensu", sep="\n")
  
    }
   
  else {
    return ( TRUE )
  }
  return(FALSE)
}
data(cars)
colnames(cars)[1:2] <- c("X", "Y")
str(as.factor(cars$Y))
StopIfNot( Y=cars$Y,X=cars$X, data=cars, type ="SS", depth =1,minobs = 1,overfit = "none", cf = 0.5)
StopIfNot(Y=c(1,2,3,4,5,6), X=(c(1,2,1,1,2,2),c(null,1,2,9,8,7)), data=data.frame(Y=c(1,2,3,4,5,6), X=(c(1,2,1,1,2,2),c(null,1,2,9,8,7))), type="Gini", depth=-1, minobs=2, overfit="none", cf=1)
#zadanie2
Prob <- function( y ){
  res <- unname( table( y ) )
  res <- res / sum( res )
  return( res )
}
Prob( zbiorD$y )
Prob( zbiorD$y[1:6] )
Prob( zbiorD$y[1:5] )

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

AssignInitialMeasures <- function(tree,Y,data,type,depth){
  wynik = 0
  depth <- 0
  tree$depth <- depth
  if (type=="Gini"){
    wynik = Gini(data$X)
    tree$Gini<-wynik
  }
  else if (type=='Entropy'){
    wynik = Entropy(data$X)
    tree$Entropy<-wynik
  }
  else if (type=='SS'){
    wynik = SS(nrow(data$X),data$X)
    tree$SS<-wynik
  }
  return (tree)
}
AssignInitialMeasures(tree='1',Y=cars$Y,data=cars,type="Entropy",depth=3)

# Zadanie 3

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

AssignInfo(tree='1', Y=cars$Y, X=cars$X, data=cars, type="SS", depth=2, minobs=1, overfit=4, cf=0.4)


#Zadanie 4
zbiorD <- data.frame( y = factor( c(rep(1,5), rep(2,5) ) ), x1 = rnorm(10) )
zbiorD$x2 <- ifelse( zbiorD$y == 1, zbiorD$x1 + 1, zbiorD$x1 + 10 )
zbiorD$x2[c(3,8)] <- c(14,2)
SplitNum <- function( Y, x, parentVal, splits, minobs ){
  n <- length( x )
  res <- data.frame( matrix( 0, length( splits ), 6 ) )
  colnames( res ) <- c("InfGain","lVal","rVal","point","ln","rn")
  for( i in 1:length( splits ) ){
    partition <- x <= splits[ i ]
    ln <- sum( partition )
    rn <- n - ln
    if( any( c(ln,rn) < minobs ) ){
      res[i,] <- 0
    }else{
      lVal <- Entropy( Prob( Y[partition] ) )
      rVal <- Entropy( Prob( Y[!partition] ) )
      InfGain <- parentVal - ( ln/n * lVal + rn/n * rVal )
      res[i,"InfGain"] <- InfGain
      res[i,"lVal"] <- lVal
      res[i,"rVal"] <- rVal
      res[i,"point"] <- splits[ i ]
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
    splits <- head( sort( s ),-1 )
  }
  res <- SplitNum( Y, x, parentVal, splits, minobs )
  incl <- res$ln >= minobs & res$rn >= minobs & res$InfGain > 0 
  res <- res[ incl, , drop = F ]
  best <- which.max( res$InfGain )
  # ten, ktory  daje zbilansowany podzial                                <- tutaj nie wiem jak poprawiæ 
  res <- res[ best, , drop = F ]
  return( res )
}
SplitVar( Y = zbiorD$y , x = zbiorD$x2, parentVal = 1, minobs = 2 )

FindBestSplit <- function( Y, Xnames, data, parentVal, minobs ){
  res <- sapply( Xnames, function( i ){
    SplitVar( Y = data[,Y] , x = data[,i], parentVal = parentVal, minobs = minobs )
  }, simplify = F )
  res <- do.call( rbind, res )
  best <- which.max( res$InfGain )
  res <- res[ best, , drop = F ]
  return( res )
}
FindBestSplit( Y = "y", Xnames = c("x1","x2"), data = zbiorD, parentVal = 1, minobs = 2 )

#zadanie 5
library( data.tree )
"Y", "X", "data", "type", "depth", "minobs", "overfit", "cf"
Tree <- function( Y, Xnames, data, depth, minobs,overfit,cf ){
  tree <- Node$new( "Root" )
  tree$Depth <- 0
  tree$Count <- nrow( data )
  tree$Val <- Entropy( Prob( data[,Y] ) )
  if(StopIfNot (Y=Y, X=Xnames, data=data, type='Gini', depth=depth, minobs=minobs,overfit =overfit, cf = cf)==FALSE){
    break
  }
  AssignInitialMeasures(tree,Y=Y,data=data,type="Gini",depth=depth)
  BuildTree( tree, Y, Xnames, data, depth, minobs )
  PruneTree<-function(){}
  AssignInfo(tree, Y=Y, X=Xnames, data=data, type="Gini", depth=depth, minobs=minobs, overfit=overfit, cf=cf)
  return( tree )
}
# tree
# str( tree )  
#Zadanie 6
BuildTree <- function( node, Y, Xnames, data, depth, minobs ){
  node$Count <- nrow( data )
  node$Prob <- Prob( data[,Y] )
  node$Class <- levels( data[,Y] )[ which.max(node$Prob) ]
  bestSplit <- FindBestSplit( Y, Xnames, data, node$Val, minobs )
  ifStop <- nrow( bestSplit ) == 0 
  if( node$Depth == depth | ifStop | all( node$Prob %in% c(0,1) ) ){
    node$Leaf <- "*"
    return( node )
  }
  splitIndx <- data[, rownames(bestSplit) ] <= bestSplit$point
  childFrame <- split( data, splitIndx )
  namel <- sprintf( "%s <= %s",  rownames(bestSplit), bestSplit$point )
  childL <- node$AddChild( namel )
  childL$Depth <- node$Depth + 1
  childL$Val <- bestSplit$lVal
  BuildTree( childL, Y, Xnames, childFrame[["TRUE"]], depth, minobs )
  namer <- sprintf( "%s >  %s",  rownames(bestSplit), bestSplit$point )
  childR <- node$AddChild( namer )
  childR$Depth <- node$Depth + 1
  childR$Val <- bestSplit$rVal
  BuildTree( childR, Y, Xnames, childFrame[["FALSE"]], depth, minobs )
}
colnames(zbiorD)<-c("y","x1","x2")
Drzewko <- Tree( Y = "y", Xnames = c("x1","x2"), data = zbiorD, depth = 3, minobs = 1,overfit ='none',cf=0.3)
print( Drzewko, "Count", "Class", "Prob", "Leaf" )

