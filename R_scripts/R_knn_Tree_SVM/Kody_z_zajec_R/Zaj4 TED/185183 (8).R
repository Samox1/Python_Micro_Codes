Entropy <- function( prob ){
  
  res <- prob * log2( prob )
  res[ prob == 0 ] <- 0
  res <- -sum( res )
  return( res )
  
}

SS <- function( n, Y ){
  
  res <- ( Y - ( 1 / n ) * sum ( Y ) ) ^ 2
  res <- sum ( res )
  return( res )
  
}

Gini <- function( prob ){
  
  res <- prob^2
  res <- sum( res )
  return ( res )
  
}


StopIfNot <- function( Y, X, data, type, depth, minobs, overfit, cf ){
  if(
    if ( is.data.frame( data ) == FALSE){
      print( "Zmienna 'data' nie jest ramk¹ danych" )
    }
    if ( is.null(data$Y) | is.null(data$X) ){
      print( "Brak zmiennej Y lub X" )
    }
    if ( any( is.na( X ) == TRUE | is.na( Y ) == TRUE) ){
      print( "Istniej¹ braki danych" )
    }
    if ( depth <= 0 && minobs <= 0 ){
      print( "G³êbokoœæ lub minimalna liczba obserwacji mniejsza od 0")
    }
    if ( type != "Gini" | type != "Entropy" | type != "SS" ){
      print( "Nieprawid³owa wartoœæ zmiennej 'type'" )
    }
    if ( overfit != "none" | overfit != "prune"){
      print( "Nieprawid³owa wartoœæ zmiennej 'overfit'" )
    }
    if( cf  %in% c(0,0.5) == FALSE){
      print( "Zmienna 'cf' ma nieprawid³ow¹ wartoœæ")
    }
    if( type == "SS" && is.factor(Y) ){
      print( "Inny warunek nie zosta³ spe³niony" )
    }
  ) {
    return ( FALSE )
  }
  else {
    return ( TRUE )
  }
}

library( data.tree )

AssignInitialMeasures <- function( tree, Y, data, type, depth ){
  
  wynik = 0
  depth <- 0
  tree$Depth <- depth
  if ( type == "Gini" ){
    wynik = Gini( data$X )
  }
  else if ( type == 'Entropy' ){
    wynik = Entropy( data$X )
  }
  else if ( type == 'SS' ){
    wynik = SS( nrow( data$X ), data$X )
  }
  
  return ( wynik )
}

AssignInfo <- function( tree, Y, X, data, type, depth, minobs, overfit, cf){
    
  attr(tree, 'Y') <- Y
  attr(tree, 'X') <- X
  attr(tree, 'data') <- data
  attr(tree, 'type') <- type
  attr(tree, 'depth') <- depth
  attr(tree, 'overfit') <- overfit
  attr(tree, 'cf') <- cf

}

Tree <- function( Y, X, data, type, depth, minobs, overfit, cf ){
  
  if (StopIfNot( Y, X, data, type, depth, minobs, overfit, cf ) == FALSE) {
    
    StopIfNot( Y, X, data, type, depth, minobs, overfit, cf )
    return( 0 )
    
  }
  
  else{
    
    tree <- Node$new( "Root" )
    AssignInitialMeasures( tree, Y, data, type, depth )
    BuildTree <- function(){}
    PruneTree<-function(){}
    AssignInfo( tree, Y, X, data, type, depth, minobs, overfit, cf )
    return ( tree )
    
  }
  
}