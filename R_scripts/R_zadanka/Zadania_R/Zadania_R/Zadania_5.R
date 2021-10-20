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
      print( "Zmienna 'data' nie jest ramk? danych" )
    }
    if ( is.null(data$Y) | is.null(data$X) ){
      print( "Brak zmiennej Y lub X" )
    }
    if ( any( is.na( X ) == TRUE | is.na( Y ) == TRUE) ){
      print( "Istniej? braki danych" )
    }
    if ( depth <= 0 && minobs <= 0 ){
      print( "G??boko?? lub minimalna liczba obserwacji mniejsza od 0")
    }
    if ( type != "Gini" | type != "Entropy" | type != "SS" ){
      print( "Nieprawid?owa warto?? zmiennej 'type'" )
    }
    if ( overfit != "none" | overfit != "prune"){
      print( "Nieprawid?owa warto?? zmiennej 'overfit'" )
    }
    if( cf  %in% c(0,0.5) == FALSE){
      print( "Zmienna 'cf' ma nieprawid?ow? warto??")
    }
    if( type == "SS" && is.factor(Y) ){
      print( "Inny warunek nie zosta? spe?niony" )
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