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
# c) Funkcja powinna zwracać macierz, a jeżeli pomnożenie macierzy nie jest możliwe to komunikat "Operacja niemożliwa".
# d) Funkcja będzie testowana na macierzach o różnych wymiarach.
# e) Nazwa funkcji to "matmultR".
# 
# Zadanie 5:
# a) Dokonaj przetestowania wydajności opracowanych powyżej funkcji na losowo stworzonych wektorach.
# b) Porównaj wydajność własnych funkcji z funkcjami wbudowanymi.
# c) Dokonaj profilowania opracowanych funkcji w celu znalezienia ew. wąskich gardeł.



library(rbenchmark)
library(microbenchmark)
library(Rcpp)
library(profvis)



### Zad. 1 ###
# Bug - jesli RObject dostanie cos na wzor "c(1:10)" to nie wie co to za typ, dziala dopiero z wektorami stworzonymi w inny sposob - mozna to udowodnic sprawdzajac typ wewnatrz 'revCpp'

cppFunction(code = {
  '
  RObject matrixCpp(RObject x, int mrow, int mcol, bool byrow=0)
  {
  
    if(is<CharacterVector>(x))
    {
        //Rcout << 2;
        CharacterVector v = as<CharacterVector>(x);
        CharacterMatrix matr(mrow, mcol);
 
        int k=0;
    
        if (byrow == 0)
        {
          for(int i=0; i<mcol; i++)
          {
            for(int j=0; j<mrow; j++)
            {
              matr(j,i)=v[k];
              k++;
            }
          }
        }
        else
        {
          for(int i=0; i<mrow; i++)
          {
            for(int j=0; j<mcol; j++)
            {
              matr(i,j)=v[k];
              k++;
            }
          }
        }
        
        return matr;
    }
    else
    {
        //Rcout << 1;
        NumericVector v = as<NumericVector>(x);
        NumericMatrix matr(mrow, mcol);
        
        int k=0;

        if (byrow == 0)
        {
          for(int i=0; i<mcol; i++)
          {
            for(int j=0; j<mrow; j++)
            {
              matr(j,i)=v[k];
              k++;
            }
          }
        }
        else
        {
          for(int i=0; i<mrow; i++)
          {
            for(int j=0; j<mcol; j++)
            {
              matr(i,j)=v[k];
              k++;
            }
          }
        }
    
        return matr;
        
    }

  }
  '
})




### Zad. 2 ###

cppFunction(code = {
  '
  RObject cumsumCpp(RObject x)
  {
  
    if(is<CharacterVector>(x))
    {
        //Rcout << 2;
        CharacterVector v = as<CharacterVector>(x);
        int n = v.size();
        CharacterVector y(n);
        
        String a = "";
    
        for(int i = 0; i < n; i++)
        {
          a += v[i];
          y[i] = a;
        }
        return y;
    }
    else
    {
        //Rcout << 1;
        NumericVector v = as<NumericVector>(x);
        int n = v.size();
        NumericVector y(n);
        
        double a = 0;
    
        for(int i = 0; i < n; i++)
        {
          a += v[i];
          y[i] = a;
        }
        return y;
    }

  }
  '
})




### Zad. 3 ###

cppFunction(code = {
  '
  RObject revCpp(RObject x) 
  {
  
    if(is<CharacterVector>(x))
    {
        //Rcout << 2;
        CharacterVector v = as<CharacterVector>(x);
        int n = v.size();
        CharacterVector b(n);
    
        for (int i=0, j=n-1; i<n; i++, j--)
        {
          b[i] = v[j];
        }
        return b;
    }
    else
    {
        //Rcout << 1;
        NumericVector v = as<NumericVector>(x);
        int n = v.size();
        NumericVector a(n);
    
        for (int i=0, j=n-1; i<n; i++, j--)
        {
          a[i] = v[j];
        }
        return a;
    }
  }
  '
})




### Zad. 4 ###

matmultR <- function(macierz_1, macierz_2) {

  if (ncol(macierz_1) == nrow(macierz_2)) {
    wynik <- matrix(nrow = nrow(macierz_1), ncol = ncol(macierz_2))
    for (i in 1:nrow(macierz_1)) {
      for (j in 1:ncol(macierz_2)) {
        x <- 0
        for (k in 1:length(macierz_2[, j])) {
          x <- macierz_1[i, ][k] * macierz_2[, j][k]
        }
        wynik[i, j] <- x
      }
    }
    return(wynik)
  } else {
    print('Operacja niemożliwa')
  }
  
}




### Zad. 5

# Przygotowanie malych zbiorow, dla wizualizacji dzialania
liczby <- (1:10)
znaki <- c('Ax','G','cgds','Z','xXXXx','kappa','likeR','C++','2021','qwerty')

# Przygotowanie losowych zbiorow danych
set.seed(123)
vec_test <- sample(seq(1,100, by=0.5), size=10000000, replace = TRUE)

dict_alpha <- c('A', 'B', 'C', 'D', 'E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z','a','b','c','d','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z')
dict_alpha_test <- sample(dict_alpha, size = 20000, replace = TRUE)


# === Zad. 1)

liczby_1_cpp <- matrixCpp(liczby,2,5, byrow = 0)
znaki_1_cpp <- matrixCpp(znaki,2,5, byrow = 1)

microbenchmark( matrix(data = vec_test, nrow = 10000, ncol = 1000, byrow = F), matrixCpp(vec_test, 10000, 1000, 0))
microbenchmark( matrix(data = dict_alpha_test, nrow = 1000, ncol = 20, byrow = F), matrixCpp(dict_alpha_test, 1000, 20, 0))

profvis({
  matrixCpp(vec_test, 10000, 1000, 0)
  matrix(data = vec_test, nrow = 10000, ncol = 1000, byrow = F)
})


# === Zad. 2)

liczby_2_cpp <- cumsumCpp(liczby)
znaki_2_cpp <- cumsumCpp(znaki)

microbenchmark(cumsum(vec_test) , cumsumCpp(vec_test))
microbenchmark(cumsum(dict_alpha_test) , cumsumCpp(dict_alpha_test))

profvis({
  cumsumCpp(vec_test)
  cumsum(vec_test)
})


# === Zad. 3) 

liczby_3_cpp <- revCpp(liczby)
znaki_3_cpp <- revCpp(znaki)

microbenchmark(rev(vec_test) , revCpp(vec_test))
microbenchmark(rev(dict_alpha_test) , revCpp(dict_alpha_test))

profvis({
  revCpp(vec_test)
  rev(vec_test)
})


# === Zad. 4)

m1 <- matrix(sample(seq(1,100, by=0.5), size=5000, replace = TRUE), 10, 500)
m2 <- matrix(sample(seq(1,100, by=0.3), size=5000, replace = TRUE), 500, 10)

a4_R <- matmultR(m1,m2)

microbenchmark(m1 %*% m2 , matmultR(m1, m2))


profvis({
  matmultR <- function(macierz_1, macierz_2) {
    
    if (ncol(macierz_1) == nrow(macierz_2)) {
      wynik <- matrix(nrow = nrow(macierz_1), ncol = ncol(macierz_2))
      for (i in 1:nrow(macierz_1)) {
        for (j in 1:ncol(macierz_2)) {
          x <- 0
          for (k in 1:length(macierz_2[, j])) {
            x <- macierz_1[i, ][k] * macierz_2[, j][k]
          }
          wynik[i, j] <- x
        }
      }
      return(wynik)
    } else {
      print('Operacja niemożliwa')
    }
  }
  
  matmultR(m1,m2)
  m1 %*% m2
})


