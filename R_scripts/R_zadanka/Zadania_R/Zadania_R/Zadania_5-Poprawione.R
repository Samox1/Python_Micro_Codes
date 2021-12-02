# Plik proszę nazwać numerem swojego indeksu.
# 
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
# 
# Zadanie 2:
# a) Stwórz funkcję "AssignInitialMeasures" przyjmującą nastęujące parametry: "tree", "Y", "data", "type", "depth".
# b) Funkcja powinna na podstawie parametrów wejściowych przypisywać do obiektu "tree" (czyli korzenia) wartości początkowe:
#    - "depth" = 0.
#    - w zależności od "type" wartość miary Gini, Entropy, SS dla calej populacji (bo to korzeń).
#   
# Zadanie 3:
# a) Stwórz funkcję "AssignInfo" przyjmującą nastęujące parametry: "tree", "Y", "X", "data", "type", "depth", "minobs", "overfit", "cf".
# b) Funkcja powinna na podstawie parametrów wejściowych przypisywać do obiektu "tree" (jako attrybuty obiektu) wartości owych parametrów.
#   
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




### OCENA - 50%



### Zad. 1 ###

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

dane <- data.frame(Y=as.factor(c(1:10)), X1=c(3:12), X2=c(5:14))
StopIfNot("Y", c("X1","X2"), dane, "Gini", 2, 5, "prune", 0.25)
StopIfNot("Y", c("X1","X2"), dane, "Entropy", 2, 5, "prune", 0.25)
StopIfNot("Y", c("X1","X2"), dane, "SS", 2, 5, "prune", 0.25)


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
    tree$Gini <- wynik
  }
  else if ( type == 'Entropy' ){
    probability <- Prob(data[,Y])
    wynik <- Entropy( probability )
    tree$Entropy <- wynik
  }
  else if ( type == 'SS' ){
    wynik <- SS( nrow( data[,Y] ), data[,Y] )
    tree$SS <- wynik
  }
  
  return ( wynik )
}



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
  attr(tree, 'overfit') <- overfit
  attr(tree, 'cf') <- cf
  
  return(tree)
}




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



### -------------------- TUTAJ ---------------------- ###


library( data.tree )

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