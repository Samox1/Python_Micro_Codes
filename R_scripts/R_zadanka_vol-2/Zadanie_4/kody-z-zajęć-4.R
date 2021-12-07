set.seed( 666 )
zbiorD <- data.frame( y = factor( c(rep(1,5), rep(2,5) ) ), x1 = rnorm(10) )
zbiorD$x2 <- ifelse( zbiorD$y == 1, zbiorD$x1 + 1, zbiorD$x1 + 10 )
zbiorD$x2[c(3,8)] <- c(14,2)

library( rpart  ) #CART
rpart( formula = y~x2, data = zbiorD, minsplit = 1, maxdepth = 3, cp = 0 )

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
Entropy( c(0,1) )
Entropy( c(0.5,0.5) )
Entropy( c(0.7,0.3) )

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
SplitNum( Y = zbiorD$y , x = zbiorD$x2, parentVal = 1, splits = head(sort(unique(zbiorD$x2)),-1), minobs = 2 )

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
  # ten, ktory  daje zbilansowany podzial
  res <- res[ best, , drop = F ]
  return( res )
}
SplitVar( Y = zbiorD$y , x = zbiorD$x2, parentVal = 1, minobs = 2 )

FindBestSplit <- function(Y, Xnames, data, parentVal, minobs){
  res <- sapply(Xnames, function(i){
    SplitVar(Y = data[,Y] , x = data[,i], parentVal = parentVal, minobs = minobs)
  }, simplify = F)
  res <- do.call(rbind, res)
  best <- which.max(res$InfGain)
  res <- res[best, , drop = F]
  return(res)
}
FindBestSplit( Y = "y", Xnames = c("x1","x2"), data = zbiorD, parentVal = 1, minobs = 2 )

library( data.tree )

Tree <- function( Y, Xnames, data, depth, minobs ){
  tree <- Node$new( "Root" )
  tree$Depth <- 0
  tree$Count <- nrow( data )
  tree$Val <- Entropy( Prob( data[,Y] ) )
  BuildTree( tree, Y, Xnames, data, depth, minobs )
  return( tree )
}
# tree
# str( tree )  

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

Drzewko <- Tree( Y = "y", Xnames = c("x1","x2"), data = zbiorD, depth = 3, minobs = 1)
print( Drzewko, "Count", "Class", "Prob", "Leaf" )

library( rpart  ) #CART
rpart( formula = y~x1+x2, data = zbiorD, minsplit = 1, maxdepth = 3, cp = 0 )