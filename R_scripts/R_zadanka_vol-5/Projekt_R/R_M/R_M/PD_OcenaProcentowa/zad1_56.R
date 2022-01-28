# Plik proszę nazwać numerem swojego indeksu.
# Plik powinien zawierać tylko definicję funkcji z Zadania 1-4.
# 
# Zadanie 1:
# a) Stwórz w C++ własną implementację funkcji "matrix" przekształcającą dowolny wektor na macierz o wymiarach "n x k".
# b) Funkcja przyjmuje następujące parametry: wektor, liczba wierszy, liczba kolumn, wskazanie czy dane wstawine są wierszami czy kolumnami, nazwy wymiarów.
# c) Funkcja powinna zwracać macierz.
# d) Funkcja będzie testowana na wektorach numerycznych i tekstowych.
# e) Nazwa funkcji to "matrixCpp".


#install.packages("Rcpp")
library("Rcpp")
cppFunction({'NumericMatrix matrixCpp(NumericVector v, int n, int k, bool sort )
{
  int licz = 0;
  NumericMatrix macierz(n,k);
  if(sort == 0)
  {
    for(int i = 0;i < macierz.cols();i++ )
    {
      for(int j = 0;j < macierz.rows();j++)
      {
        macierz(j,i)=v[licz];
        licz++;
      }
    }
  }
  else
  {
    for(int i = 0; i < macierz.rows();i++)
      {
      for(int j = 0;j < macierz.cols();j++ )
        {macierz(i,j)=v[licz];
         licz++;
        }
      }
   }
 return(macierz);
}
'})


macierz1 <- matrix(1:12,3,4,1)
rownames(macierz1) <- c(paste("x",1:nrow(macierz1),sep = "_"))
colnames(macierz1)<- c(paste("y",1:ncol(macierz1),sep="_"))
macierz1

macierz2 <- matrix(1:12,4,3,0)
rownames(macierz2) <- c(paste("x",1:nrow(macierz2),sep = "_"))
colnames(macierz2)<- c(paste("y",1:ncol(macierz2),sep="_"))
macierz2

#WYWOLANIE matrixCpp z opisaniem wymiar�W
m_wynik <- matrix(matrixCpp(1:12,3,4,1),3,4,1)
rownames(m_wynik) <- c(paste("x",1:nrow(m_wynik),sep = "_"))
colnames(m_wynik)<- c(paste("y",1:ncol(m_wynik),sep="_"))
m_wynik


set.seed(666)
wart <- sample(1:100, 10000, replace = T)
matrixCpp(wart,5,5,1)

#WYWOLANIE matrixCpp z opisaniem wymiar�W
m_wynik2 <- matrix(matrixCpp(wart,5,5,1),5,5,1)
rownames(m_wynik2) <- c(paste("x",1:nrow(m_wynik2),sep = "_"))
colnames(m_wynik2)<- c(paste("y",1:ncol(m_wynik2),sep="_"))
m_wynik2

litery <- c(LETTERS)
litery <- as.double(litery)

matrixCpp(litery,5,5,1)
# library("Rcpp")
# cppFunction({'NumericMatrix matrixCpp(NumericVector c, int n, int k, bool sort, char rows, char columns )
# { 
#   int licz = 0;
#   NumericMatrix macierz(n,k);
#   List dim = List::create(CharacterVector::create("a", "b", "c", "d"),
#                           CharacterVector::create("1", "2", "3"));
#   macierz.attr("dimnames") = dim;
#   if(sort == 0)
#   { 
#     for(int i = 0;i < macierz.cols();i++ )
#     {
#       for(int j = 0;j < macierz.rows();j++)
#       {
#         macierz(j,i)=c[licz];
#         licz++;
#       }
#     }
#   }
#   else
#   {
#     for(int i = 0; i < macierz.rows();i++)
#       {
#       for(int j = 0;j < macierz.cols();j++ )
#         {macierz(i,j)=c[licz];
#          licz++;
#         }
#       }
#    }
#  return(macierz);
# }
# '})
#matrixCpp(1:12,4,3,0,"a","b")


# Zadanie 2:
# a) Stwórz w C++ własną implementację funkcji "cumsum" zwracającą sumy skumulowane.
# b) Funkcja przyjmuje następujące parametry: wektor.
# c) Funkcja powinna zwracać wektor.
# d) Funkcja będzie testowana na wektorach numerycznych i tekstowych.
# e) Nazwa funkcji to "cumsumCpp".


library("Rcpp") 
cppFunction({
'
  NumericVector cumsumCpp( NumericVector v )
  {
   double a = 0;
   NumericVector wektor(v.size());
   for(int i=0; i < v.size(); i++)
    {
        a += v[i];
       wektor[i] = a;
   }
   return(wektor);
  }
'
})
W_tekst <- c("a", "b", "c")
W_tekst
faktor <- factor(W_tekst)
faktor

cumsum(W_tekst) #wlasciwy brak mozliwosci sumowania tekstu
cumsum(faktor) # brak mozliwosci laczenia faktorow

W_num <- c(1:4)
cumsum(W_num)
cumsumCpp(1:4)
cumsumCpp(faktor)

# Zadanie 3:
# a) Stwórz w C++ własną implementację funkcji "rev" zwracającą wektor z elementami o odwróconej kolejności.
# b) Funkcja przyjmuje następujące parametry: wektor.
# c) Funkcja powinna zwracać wektor.
# d) Funkcja będzie testowana na wektorach numerycznych i tekstowych.
# e) Nazwa funkcji to "revCpp".

#library("Rcpp") 
# cppFunction({
#      '
#    template<typename T>
#    T* revCpp( T* v ){
#     int len = v.size();
#     
#     T v_rev[len];
#     for(int i=0; i < v.size(); i++)
#      {
#          v_rev[i] = v[len-i-1];
#       }
#     return(v_rev);
#    }
#  '
#    })



#FUNKCJA P�TLI WHILE
# cppFunction({
#     '
#     NumericVector revCpp( NumericVector v ){
#     
#      NumericVector v_rev(v.size());
#      int i = v.size();
#      int j = 0;
#      
#      while(i>=0){
#      v_rev[v.size()-i-1]=v[i];
#      i--;
#      }
#     return(v_rev);
#     }
#   '
#   })

#FUNKCJA PETLI FOR do sprawdzania czasu
library("Rcpp")
cppFunction({
  '
  NumericVector revCpp( NumericVector v ){
   int len = v.size();

   NumericVector v_rev(v.size());
   for(int i=0; i < v.size(); i++)
    {
        v_rev[i] = v[len-i-1];
     }
   return(v_rev);
  }
'
})

wektor <- 1:5
rev(wektor)
revCpp(1:5)

rev(c("a","b","c"))
revCpp(c("a","b","c")) #blad

W_tekst <- c("a", "b", "c")
W_tekst
faktor <- factor(W_tekst)
faktor
revCpp(c("a","b","c"))


W_num <- c(1:4)
cumsum(W_num)

faktor2 <- factor(c("a","b"))
faktor2
revCpp(faktor2)

#Wnioski czasowe zad3: p�tla FOR dla R ma lepsze wyniki czasowe

# Zadanie 4:
# a) Stwórz w R własną implementację oparatora "%*%" mnożącego dwie macierze.
# b) Funkcja przyjmuje następujące parametry: macierz1, macierz2.
# c) Funkcja powinna zwracać macierz, a jeżeli pomnożenie macierzy nie jest możliwe to komunikat "Opracja niemożliwa".
# d) Funkcja będzie testowana na macierzach o różnych wymiarach.
# e) Nazwa funkcji to "matmultR".

m1 <- rbind(c(1,2),c(3,4))
m2 <- cbind(c(1,2),c(3,4))
m1
m2

m3 <- matrix(1:6,2)
m4 <- matrix(1:12,3,4) 
m3
m4

m5 <- matrix(1:10, 4, 5)
m6 <- matrix(1:10, 4, 5)
m5
m6

matmultR <- function(macierz1, macierz2){
  n_col_m1 <- ncol(macierz1)
  n_row_m1 <- nrow(macierz1)
  n_row_m2 <- nrow(macierz2)
  n_col_m2 <- ncol(macierz2)
  if((n_col_m1 != n_row_m2)) 
  {
    return("Opcja niemożliwa")
  }
  else
    {
    m_wynik = matrix(0,n_row_m1, n_col_m2)
    colnames(m_wynik)<- c(paste("x",1:n_col_m2,sep="_"))
    rownames(m_wynik)<- c(paste("y",1:n_row_m1,sep="_"))
    sum = 0
    for (i in 1:nrow(m_wynik))
      {
      for (j in 1:ncol(m_wynik))
        {
        for (k in 1:n_col_m1)
          {
          sum = sum + macierz1[i,k]*macierz2[k,j]
          m_wynik[i,j] = sum
          }
        sum = 0
        }
      }
      return(m_wynik)
     }

}

matmultR(m1, m2)
spr1 <- m1%*%m2
spr1

matmultR(m3, m4)
spr2 <- m3%*%m4
spr2

matmultR(m5, m6)
spr3 <- m5%*%m6 #OK zwracany błąd mnożenia macierzy 
 
gc()

# Zadanie 5:
# a) Dokonaj przetestowania wydajności opracowanych powyżej funkcji na losowo stworzonych wektorach.
# b) Porównaj wydajność własnych funkcji z funkcjami wbudowanymi.
# c) Dokonaj profilowania opracowanych funkcji w celu znalezienia ew. wąskich gardeł.

# los1 <- c(1:10)
# los2 <- c('a','b','c','d')
# los3 <- c(1)
# 
# install.packages("rbenchmark")
# library(rbenchmark)
# 
# install.packages("microbenchmark")
# library(microbenchmark)
# 
# install.packages("profvis")
# library("profvis")
# install.packages("ggplot2")
# library("ggplot2")
# 
# A1 <- matrix(runif(9,1,10000), 2, 3, T, list(c("x1","x2"),c("y1","y2","y3")))
# A1
# A2 <- matrix(runif(6,1,10000), 3, 2, T, list(c("x1","x2","x3"),c("y1","y2")))
# A2
# 
# A1%*%A2
# matmultR(A1,A2)
# microbenchmark(matmultR(A1,A2), A1%*%A2) #WNIOSKI lepsza funkcja wbudowana
# benchmark(matmultR(A1,A2), A1%*%A2) #WNIOSKI szybsze wyniki dla funkcji wbudowanej widoczne przy czasie, ktory minal - elapsed
# 
# profvis({
#    macierz1 <- matrix(runif(6,1,10000), 2, 3)
#    macierz2  <- matrix(runif(6,1,10000), 3, 2)
#   
#    matmultR <- function(macierz1, macierz2){
#      n_col_m1 <- ncol(macierz1)
#      n_row_m2 <- nrow(macierz2)
#      
#      if((n_col_m1 == n_row_m2))
#      {
#        m_wynik = matrix(0,nrow(macierz1), ncol(macierz2))
#        sum = 0
#        for (i in 1:nrow(m_wynik))
#        {
#          for (j in 1:ncol(m_wynik))
#          {
#            for (k in 1:ncol(macierz1))
#            {
#              sum = sum + macierz1[i,k]*macierz2[k,j]
#              m_wynik[i,j] = sum
#            }
#            sum = 0
#          }
#        }
#        return(m_wynik)
#      }
#      else 
#      {
#        return("opcja niemożliwa")
#      }
#      
#    }
#    matmultR(macierz1, macierz2)
# })
# 
# 
