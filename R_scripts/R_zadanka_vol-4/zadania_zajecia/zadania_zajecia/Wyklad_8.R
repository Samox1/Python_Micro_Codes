set.seed( 666 )
zbiorD <- data.frame( y = factor( c( rep(1,5), rep(2,5) ) ), x1 = rnorm(10) )
zbiorD$x2 <- ifelse( zbiorD$y == 1, zbiorD$x1 + 1, zbiorD$x1 + 10 )
zbiorD$x2[c(3,8)] <- c(14,2)

library(rpart)
rpart( formula = y~x2, data = zbiorD, minsplit = 1, maxdepth = 3, cp = 0 )

Entropy <- function( prob ){
  result <- prob * log2( prob )
  result[ prob == 0 ] <- 0
  result <- -sum( result )
  return( result )
}
Entropy( c(0,1) )
Entropy( c(0.5,0.5) )
Entropy( c(0.7,0.3) )

table( c(1,2,5,23,2) )
Prob <- function( y_tar ){
  result <- unname( table( y_tar ) )
  result <- result / sum( result )
  return( result )
}
Prob( zbiorD$y )
Prob( zbiorD$y[1:6] )
Prob( zbiorD$y[1:5] )

install.packages( data.tree )
library( data.tree )

Tree <- function( Yname, Xnames, data, depth = 5, minobs = 5, splitPoint ){ # splitPoint - do usuniecia pozniej
  # Tworzenie korzenia
  tree <- Node$new("Root")
  tree$Depth <- 0
  tree$Count <- nrow(data)
  # Funkcja budujaca rekurencyjnie drzewo
  BuildTree( tree, Yname, Xnames, data, depth, minobs, splitPoint )
  return( tree )
}

BuildTree <- function( node, Yname, Xnames, data, depth, minobs, splitPoint ){
  node$Count <- nrow( data )
  node$Prob <- Prob( data[,Yname] )
  # splitPoint_ <- BestSplit()
  tab <- table( data[,Xnames] <= splitPoint )
  ifStop <- dim(tab) == 1 | node$Depth == depth | all( node$Prob %in% c(0,1) ) | any( tab < minobs )
  if( ifStop ){
    node$Leaf <- "*"
    return( node )
  }else{
    split_indx <- data[,Xnames] <= splitPoint
    child_frame <- split( data, split_indx )
    name_l <- sprintf( "%s <= %s", Xnames, splitPoint )
    child_l <- node$AddChild( name_l )
    child_l$value <- splitPoint
    child_l$Depth <- node$Depth + 1
    BuildTree( child_l, Yname, Xnames, child_frame[["TRUE"]], depth, minobs, splitPoint - 1 )
    name_r <- sprintf( "%s >  %s", Xnames, splitPoint )
    child_r <- node$AddChild( name_r )
    child_r$value <- splitPoint
    child_r$Depth <- node$Depth + 1
    BuildTree( child_r, Yname, Xnames, child_frame[["FALSE"]], depth, minobs, splitPoint - 1 )
  }
}

Drzewko <- Tree( "y", "x2", zbiorD, depth = 3, minobs = 1, splitPoint = 10 )
Drzewko
print( Drzewko, "Count", "Prob", "Leaf" )

rpart( formula = y~x2, data = zbiorD, minsplit = 1, maxdepth = 3, cp = 0 )
