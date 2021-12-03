# Plik proszę nazwać numerem swojego indeksu.



### Zad. 1 ###

# Zadanie 1:
# a) Stwórz funkcję "StopIfNot" przyjmującą nastęujące parametry: "Y", "X", "data", "type", "depth", "minobs", "overfit", "cf".
# b) Funkcja powinna sprawdzać czy nauka modelu jest możliwa do wykonania, tj:
#    - czy "data" jest ramką danych,
#    - czy wszystkie wymienione zmienne ("Y", "X") istnieją w "data",
#    - czy zmienna "Y" oraz zmienne "X" w tabeli "data" nie ma braków danych,
#    - czy "depth" oraz "minobs" są większe od 0,
#    - czy "type" przyjmuje watrtość "Gini", "Entropy", "SS",
#    - czy "overfit" przyjmuje watrtość "none" lub "prune",
#    - czy "cf" jest w przedziale (0,0.5],
#    - czy możliwe kombinacje parametrów mają sens, np. "type = SS" kiedy "Y" jest faktorem.
# c) W przypadku niespełniania któregoś z warunków, funkcja powinna wyświetlić w konsoli, czego dotyczy problem.
# d) Funkcja zwraca "TRUE", jeżeli nauka jest możliwa, w przeciwnym wypadku "FALSE". 


StopIfNot <- function( Y, X, data, type, depth, minobs, overfit, cf ){
  
  if( is.data.frame( data ) == FALSE)
  {
    message( "Zmienna 'data' nie jest ramka danych" )
    return ( FALSE )
  }
  else if( all(Y %in% colnames(data)) == FALSE | all(X %in% colnames(data)) == FALSE)
  {
    message( "Brak zmiennej Y lub X" )
    return ( FALSE )
  }
  else if( any( is.na( data[,Y] ) == TRUE | is.na( data[,X] ) == TRUE) )
  {
    message( "Istnieja braki danych" )
    return ( FALSE )
  }
  else if( type != "Gini" && type != "Entropy" && type != "SS" )
  {
    message( "Nieprawidlowa wartosc zmiennej 'type'" )
    return ( FALSE )
  }
  else if( depth <= 0 | minobs <= 0 )
  {
    message( "Glebokosc lub minimalna liczba obserwacji rowna lub mniejsza od 0")
    return ( FALSE )
  }
  else if( overfit != "none" && overfit != "prune")
  {
    message( "Nieprawidlowa wartosc zmiennej 'overfit'" )
    return ( FALSE )
  }
  else if( cf <= 0 | cf > 0.5 )
  {
    message( "Zmienna 'cf' nie zawiera sie w przedziale 0.0 - 0.5")
    return ( FALSE )
  }
  else if( (type == "SS" && is.factor(data[,Y])) || (type == "Gini" && !is.factor(data[,Y])) || (type == "Entropy" && !is.factor(data[,Y])))
  {
    message( "Kombinacja danych i 'type' jest nieprawidlowa" )
    return ( FALSE )
  }
  else 
  {
    return ( TRUE )
  }
}

# test 
dane <- data.frame(Y=as.factor(c(1:10)), X1=c(3:12), X2=c(5:14))
StopIfNot("Y", c("X1","X2"), dane, "Gini", 2, 5, "prune", 0.25)
StopIfNot("Y", c("X1","X2"), dane, "Entropy", 2, 5, "prune", 0.25)
StopIfNot("Y", c("X1","X2"), dane, "SS", 2, 5, "prune", 0.25)
# test



### Zad. 2 ###

# Zadanie 2:
# a) Stwórz funkcję "AssignInitialMeasures" przyjmującą nastęujące parametry: "tree", "Y", "data", "type", "depth".
# b) Funkcja powinna na podstawie parametrów wejściowych przypisywać do obiektu "tree" (czyli korzenia) wartości początkowe:
#    - "depth" = 0.
#    - w zależności od "type" wartość miary Gini, Entropy, SS dla calej populacji (bo to korzeń).

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

SS <- function( n, Y ){
  res <- ( Y - (( 1 / n ) * sum ( Y )) ) ^ 2
  res <- sum ( res )
  return( res )
}

Gini <- function( prob ){
  res <- prob^2
  res <- sum( res )
  return ( res )
}


AssignInitialMeasures <- function( tree, Y, data, type, depth ){
  
  depth <- 0
  tree$Depth <- depth
  
  if ( type == "Gini" ){
    probability <- Prob(data[,Y])
    wynik <- Gini( probability )
    tree$inf <- wynik
  }
  else if ( type == 'Entropy' ){
    probability <- Prob(data[,Y])
    wynik <- Entropy( probability )
    tree$inf <- wynik
  }
  else if ( type == 'SS' ){
    wynik <- SS( nrow( data[,Y] ), data[,Y] )
    tree$inf <- wynik
  }
  
  return (tree)
}

# test
tree_test <- Node$new("Root")                                # Struktura testowa
tree_test <- AssignInitialMeasures(tree_test, "Y", dane, "Gini", 3)
print(tree_test$Depth)
print(tree_test$inf)
# test



### Zad. 3 ###

# Zadanie 3:
# a) Stwórz funkcję "AssignInfo" przyjmującą nastęujące parametry: "tree", "Y", "X", "data", "type", "depth", "minobs", "overfit", "cf".
# b) Funkcja powinna na podstawie parametrów wejściowych przypisywać do obiektu "tree" (jako attrybuty obiektu) wartości owych parametrów.

AssignInfo <- function( tree, Y, X, data, type, depth, minobs, overfit, cf){
  
  attr(tree, 'Y') <- Y
  attr(tree, 'X') <- X
  attr(tree, 'data') <- data
  attr(tree, 'type') <- type
  attr(tree, 'depth') <- depth
  attr(tree, 'minobs') <- minobs
  attr(tree, 'overfit') <- overfit
  attr(tree, 'cf') <- cf
  
  return(tree)
}

# test
tree_test <- AssignInfo(tree_test, "Y", c("X1","X2"), dane, "Gini", 2, 5, "prune", 0.25)
print(attributes(tree_test))
# test



### Zad. 4 ###

# Zadanie 4:
# a) Stwórz funkcję "Tree" przyjmującą nastęujące parametry: "Y", "X", "data", "type", "depth", "minobs", "overfit", "cf".
# b) Jest to rozwinięcie funkcji ze slajdu nr 19. Funckja powinna po kolei wywoływać pozostałe funkcje:
#    - "StopIfNot", jeżeli zwracana wartość to "FALSE" to kończymy działanie całej funkcji (zwracamy obiekt niewidzialny),
#    - tworzenie obiektu "tree",
#    - "AssignInitialMeasures",
#    - "BuildTree", na tę chwilę ta funkcja jest pusta BuildTree<-function(){},
#    - "PruneTree", na tę chwilę ta funkcja jest pusta PruneTree<-function(){},
#    - "AssignInfo".
# c) Funkcja powwina zwracać obiekt "tree".


library(data.tree)


# Funkcja BuildTree dla testu czy wszystko dziala

# BuildTree <- function( node, Y, X, data, depth, minobs, splitP ){
# 
#   node$Count <- nrow( data )
#   node$Prob <- Prob( data[,Y] )
# 
#   # splitP <- BestSplit()
#   tab <- table( data[,X] <= splitP )
#   ifStop <- dim( tab ) == 1 | any( tab < minobs )
# 
#   if( node$Depth == depth | ifStop | all(  node$Prob %in% c(0,1) ) ){
# 
#     node$Leaf <- "*"
#     return( node )
# 
#   }else{
# 
#     split_indx <- data[,X] <= splitP
#     child_frame <- split( data, split_indx )
# 
#     name <- sprintf( "%s <= %s", X, splitP )
#     child_l <- node$AddChild( name )
#     child_l$value <- split_indx
#     child_l$Depth <- node$Depth + 1
# 
#     BuildTree( child_l, Y, X, child_frame[[1]], depth, minobs, splitP - 1 )
# 
#     name <- sprintf( "%s >  %s", X, splitP )
#     child_r <- node$AddChild( name )
#     child_r$value <- split_indx
#     child_r$Depth <- node$Depth + 1
# 
#     BuildTree( child_r, Y, X, child_frame[[2]], depth, minobs, splitP - 1 )
# 
#   }
# }


BuildTree_pass <- function(tree){return(tree)}

PruneTree_pass <- function(tree){return(tree)}


Tree <- function( Y, X, data, type, depth, minobs, overfit, cf ){
  
  if(StopIfNot(Y, X, data, type, depth, minobs, overfit, cf))
  {
    tree <- Node$new("Root")
    
    AssignInitialMeasures(tree = tree, Y, data, type, depth)
    
    BuildTree_pass(tree)
    # BuildTree(tree, Y, X, data, depth, minobs, 10 )
    
    PruneTree_pass(tree)
    
    AssignInfo(tree, Y, X, data, type, depth, minobs, overfit, cf)
    
    return(tree)
  }
  else{
    return(invisible(tree))
  }
  
}

# test
set.seed(666)
zbiorD <- data.frame( y = factor( c( rep(1,5), rep(2,5) ) ), x1 = rnorm(10) )
zbiorD$x2 <- ifelse( zbiorD$y == 1, zbiorD$x1 + 1, zbiorD$x1 + 10 )
zbiorD$x2[c(3,8)] <- c(14,2)
zbiorD

Drzewo_testowe <- Tree("y", "x2", zbiorD, "Entropy", 3, 1, "none", 0.25)
print(Drzewo_testowe,"Count","Prob","Leaf", "Depth")
print(attributes(Drzewo_testowe))
# test



