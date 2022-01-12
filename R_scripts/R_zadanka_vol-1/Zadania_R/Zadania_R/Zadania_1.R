# Plik proszę nazwać numerem swojego indeksu.
# Plik powinien zawierać tylko definicję funkcji z Zadania 1-4.
# 
# Zadanie 1:
# a) Stwórz w C++ własną implementację funkcji "matrix" przekształcającą dowolny wektor na macierz o wymiarach "n x k".
# b) Funkcja przyjmuje następujące parametry: wektor, liczba wierszy, liczba kolumn, wskazanie czy dane wstawine są wierszami czy kolumnami, nazwy wymiarów.
# c) Funkcja powinna zwracać macierz.
# d) Funkcja będzie testowana na wektorach numerycznych i tekstowych.
# e) Nazwa funkcji to "matrixCpp".
# 
# Zadanie 2:
# a) Stwórz w C++ własną implementację funkcji "cumsum" zwracającą sumy skumulowane.
# b) Funkcja przyjmuje następujące parametry: wektor.
# c) Funkcja powinna zwracać wektor.
# d) Funkcja będzie testowana na wektorach numerycznych i tekstowych.
# e) Nazwa funkcji to "cumsumCpp".
# 
# Zadanie 3:
# a) Stwórz w C++ własną implementację funkcji "rev" zwracającą wektor z elementami o odwróconej kolejności.
# b) Funkcja przyjmuje następujące parametry: wektor.
# c) Funkcja powinna zwracać wektor.
# d) Funkcja będzie testowana na wektorach numerycznych i tekstowych.
# e) Nazwa funkcji to "revCpp".
# 
# Zadanie 4:
# a) Stwórz w R własną implementację oparatora "%*%" mnożącego dwie macierze.
# b) Funkcja przyjmuje następujące parametry: macierz1, macierz2.
# c) Funkcja powinna zwracać macierz, a jeżeli pomnożenie macierzy nie jest możliwe to komunikat "Opracja niemożliwa".
# d) Funkcja będzie testowana na macierzach o różnych wymiarach.
# e) Nazwa funkcji to "matmultR".
# 
# Zadanie 5:
# a) Dokonaj przetestowania wydajności opracowanych powyżej funkcji na losowo stworzonych wektorach.
# b) Porównaj wydajność własnych funkcji z funkcjami wbudowanymi.
# c) Dokonaj profilowania opracowanych funkcji w celu znalezienia ew. wąskich gardeł.


install.packages('rbenchmark')
install.packages('microbenchmark')
install.packages('profvis')
library(rbenchmark)
library(microbenchmark)
library(Rcpp)


### OCENA - 44%

# Zad 1

cppFunction(code = {
  '
  NumericMatrix matrixCpp(NumericVector x, int a, int b, bool isrow=0){
    int k=0;
    NumericMatrix matr(a, b);
    int mc = matr.ncol();
    int mn = matr.nrow();

    if (isrow == 0){
      for(int i=0; i<mc; i++){
        for(int j=0; j<mn; j++){
          matr(j,i)=x[k];
          k++;
        }
      }
    }
    else{
      for(int i=0; i<mn; i++){
        for(int j=0; j<mc; j++){
          matr(i,j)=x[k];
          k++;
        }
      }
    }
    return(matr);
  }
  '
})

matrixCpp(1:20, 4, 5, 0)
matrix(
  data = 1:20,
  nrow = 4,
  ncol = 5,
  byrow = F
)
benchmark(matrix(
  data = 1:20,
  nrow = 4,
  ncol = 5,
  byrow = F
) ,
matrixCpp(1:20, 4, 5, 0))
microbenchmark(matrix(
  data = 1:20,
  nrow = 4,
  ncol = 5,
  byrow = F
) ,
matrixCpp(1:20, 4, 5, 0))

# Zad 2

cppFunction(code = {
  '
  NumericVector cumsumCpp(NumericVector x){
    char a = 0;
    NumericVector y(x.size());
    for(int i = 0; i < x.size(); i++){
      a += x[i];
      y[i] = a;
    }
    return y;
  }
  '
})

cumsumCpp(1:5)
x = 1:10
benchmark(cumsum(x) , cumsumCpp(x))
microbenchmark(cumsum(x) , cumsumCpp(x))

# Zad 3

cppFunction(code = {
  '
  CharacterVector revCpp(CharacterVector x) {
    CharacterVector a(x.size());
    int n = x.size();

    for (int i=0, j=n-1; i<n; i++, j--){
      String temp = x[j];
      a[i] = temp;
    }
    return a;
  }
  '
})

revCpp(1:5)
revCpp(c('a', 'b'))
benchmark(rev(x) , revCpp(x))
microbenchmark(rev(x) , revCpp(x))

# Zad 4

matmultR <- function(macierz_1, macierz_2) {
  if (ncol(macierz_1) == nrow(macierz_2)) {
    wynik <- matrix(nrow = nrow(macierz_1), ncol = ncol(macierz_2))
    for (i in 1:nrow(macierz_1)) {
      for (j in 1:ncol(macierz_2)) {
        x <- 0
        for (k in 1:length(macierz_2[, j])) {
          x <- x + macierz_1[i, ][k] * macierz_2[, j][k]
        }
        wynik[i, j] <- x
      }
    }
    return(wynik)
  } else {
    print('Ne mo?na wykona? operacji')
  }
}

m1 <- matrix(1:6, 2, 3)
m2 <- matrix(1:6, 3, 2)
matmultR(m1, m2)

m1 %*% m2

benchmark(m1 %*% m2 , matmultR(m1, m2))
microbenchmark(m1 %*% m2 , matmultR(m1, m2))

# Zad 5
#Pod ka?dym z zada? jest por?wnanie wydajno?ci funkcji
