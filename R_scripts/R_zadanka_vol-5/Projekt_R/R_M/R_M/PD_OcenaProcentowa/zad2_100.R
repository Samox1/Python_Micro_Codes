#PD2
# Zadanie 1:
# a) Stwórz funkcję przyjmującą nastęujące parametry: "dane", "Ynazwa", "XnazwyList", "Nrdzeni", "metoda".
# b) Funkcja operując na zbiorze "dane" powinna tworzyć model regresji liniowej dla Ynazwa w odniesieniu do zmiennych XnazwyList.
#    W najprostszej postaci dla danych z "Zadanie 0" są to modele: ("Y~x1", "Y~x2", "Y~x3" etc.).
#    To jakiej postaci model powinien być zbudowany, definiowane jest przez parametr "XnazwyList", przyjmujący obiekt typu lista.
#    Lista ma tyle elementów, ile modeli będzie zbudowanych. Każdy element listy jest wektorem nazw zmiennych "x".
# 
# Przykład: "list(x1,c(x1,x5,x7))" buduje dwa modele 1) "Y~x1" oraz 2) "Y~x1+x5+x7".
# c) Funkcja powinna budować każdą kombinację modeli równolegle.
# d) W zależności od przekazanego argumentu do "metoda", funkcja wykorzystywać powinna albo równoleglą wersję "lapply",
#    albo równoleglą wersję pętli "for".
# e) Każda równoległa pętla powinna zwracać informacje o nazwach zmiennej/zmiennych (pierwsza kolumna tekstowa)
#    i oszacowaniach parametrów (druga kolumna numeryczna).
# f) Funkja powinna zwracać wyniki w formie listy.
# g) Nazwa funkcji to "ModelParallel".

#FUNKCJA 
library("parallel") # do ncores, clusters
library("doParallel")
library("foreach")
library("iterators")

ModelParallel <- function( dane, Ynazwa, XnazwyList, Nrdzeni, metoda ) {
  
  cl <- makeCluster ( Nrdzeni )
  
  dane <- as.data.frame ( dane )
  coeff <- vector( "list", length( XnazwyList ) )
  
  if( metoda == "for" ) {
    
    coeff <- foreach ( i = 1 : length ( XnazwyList ) ) %dopar% {
      f = as.formula ( paste ( Ynazwa, paste ( unlist ( XnazwyList[[i]] ), collapse = '+' ), sep = "~" ) )
      model.lm <- lm ( f, data = dane )
      wynik <- as.data.frame ( coefficients ( model.lm ) ) #coef is a generic function which extracts model coefficients from objects returned by modeling functions. coefficients is an alias for it.
      coeff[[i]] <- wynik 
    }
  } else if ( metoda == "lapply" ) {
    
    funkcja <- function( i, dane, Ynazwa ) {
      f = as.formula( paste ( Ynazwa, paste ( unlist ( XnazwyList[[i]] ), collapse = '+' ), sep = "~" ) )
      model.lm <- lm( f, data = dane )
      coeff [[i]] <- as.data.frame ( coefficients ( model.lm ) )
    }
    
    coeff <- parLapply ( cl, 1 : length ( XnazwyList ), funkcja, dane, Ynazwa )
  }
  
  return( coeff )
  
  stopCluster( cl )
}

#gc()





# Zadanie 0:
#bez funkcji
macierz <- matrix(0,1000,1001)
colnames(macierz)<- c("Y",paste("x",1:1000,sep="_"))
#macierz

set.seed(555)
wektor <- sample(1:100,1000,replace=T)
#wektor

macierz[,1] <- wektor
#macierz

set.seed(555)
w_pom <- rnorm(550)
#w_pom <-sample(1:100, 550, replace = T)
m_pom <- matrix( w_pom, 1000, 1001 )
#m_pom

for( i in 2:1001)#col
{
  for( j in 1:1000)#row
  {
    macierz [j, i] <- macierz[j,1]+m_pom[j,i]
  }
}
#macierz

# FUNKCJA DLA MATRIX
matrixR <- function(row,col){
  macierz <- matrix(0,row,col)
  coln <- rep('NULL',times=col)
  for (i in 1:col){
    if (i==1){
      coln[i] <- "Y"
    }
    else{
      nazwa = sprintf("x_%d",i-1)
      coln[i] <- nazwa
    }
  }
  
  set.seed(555)
  for (i in 1:col){
    for (j in 1:row){
      macierz[j,i] <- round(rnorm(1),2)
    }
  }
  colnames(macierz) <- coln
  return(macierz)
}

# matrixR(1000,1001)
# macierz <- matrixR(1000,1001)
# macierz

# macierz <- matrix(0,50,11)
# colnames(macierz)<- c("Y",paste("x",1:10,sep="_"))
# macierz
# 
# set.seed(555)
# wektor <- sample(1:100,50,replace=T)
# wektor
# 
# macierz[,1] <- wektor
# macierz
# 
# set.seed(555)
# w_pom <- rnorm(550)
# m_pom <- matrix(w_pom,50,11)
# m_pom
# 
# for( i in 2:11)#col
#   { 
#   for( j in 1:50)#row
#     { 
#     macierz [j, i] <- macierz[j,1]+m_pom[j,i]
#   }
# }
# macierz




