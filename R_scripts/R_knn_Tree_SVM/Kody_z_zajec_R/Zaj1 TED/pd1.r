install.packages(Rcpp)
library(Rcpp)




# Zadanie 2:
# a) Stw?rz w C++ w?asn? implementacj? funkcji "cumsum" zwracaj?c? sumy skumulowane.
# b) Funkcja przyjmuje nast?puj?ce parametry: wektor.
# c) Funkcja powinna zwraca? wektor.
# d) Funkcja b?dzie testowana na wektorach numerycznych i tekstowych.
# e) Nazwa funkcji to "cumsumCpp".
cumsum(1:50)
cumsum(letters) #ma by? komunikat z?e wartosci tekstowe

cppFunction({'
  NumericVector cumsumCpp( NumericVector x ){
    int n = x.size();
    double wynik = 0.0;
    
    NumericVector v(n);

    for( int i = 0; i < n; ++i ){
      wynik += x[i];
      v[i] = wynik;
    }
    return( v );
  }
'})

cumsumCpp(1:50);
cumsum(1:50);

# Zadanie 3:
# a) Stw?rz w C++ w?asn? implementacj? funkcji "rev" zwracaj?c? wektor z elementami o odwr?conej kolejno?ci.
# b) Funkcja przyjmuje nast?puj?ce parametry: wektor.
# c) Funkcja powinna zwraca? wektor.
# d) Funkcja b?dzie testowana na wektorach numerycznych i tekstowych.
# e) Nazwa funkcji to "revCpp".

cppFunction({'
  NumericVector revCpp( NumericVector x ){
    int n = x.size();
    NumericVector v(n);

    for( int i = 0; i < n; ++i ){
      v[i] = x[n-1-i];
    }
    return( v );
  }
'})

revCpp(1:5)

# Zadanie 4:
# a) Stw?rz w R w?asn? implementacj? oparatora "%*%" mno??cego dwie macierze.
# b) Funkcja przyjmuje nast?puj?ce parametry: macierz1, macierz2.
# c) Funkcja powinna zwraca? macierz, a je?eli pomno?enie macierzy nie jest mo?liwe to komunikat "Opracja niemo?liwa".
# d) Funkcja b?dzie testowana na macierzach o r??nych wymiarach.
# e) Nazwa funkcji to "matmultR".

cppFunction({'
  NumericMatrix matmultR( NumericMatrix macierz1, NumericMatrix macierz2 ){
    int k1 = macierz1.ncol();
    int w1 = macierz1.nrow();
    
    int k2 = macierz2.ncol();
    int w2 = macierz2.nrow();
    
    if(k1 != w2) { throw std::range_error("Operacja niemo?liwa"); }
    
    NumericMatrix macierz3( w1, k2);
    
    for( int i = 0; i < w1; ++i ){
      NumericMatrix::Row referencjaDoAktualnegoWierszaMacierzy3 = macierz3(i, _ );
      
      
      NumericVector aktualnyWierszMacierzy1 = macierz1(i, _);
      
      NumericVector wierszDoWstawienia(k2);
      
      for( int j = 0; j < k2; ++j ){
        NumericVector aktualnaKolumnaMacierzy2 = macierz2(_, j);
        
        double suma = 0.0;
        
        for(int m = 0; m < w2; ++m){
          suma += aktualnyWierszMacierzy1[m] * aktualnaKolumnaMacierzy2[m];
        }
        
        wierszDoWstawienia[j] = suma;
      }
      
      referencjaDoAktualnegoWierszaMacierzy3 = wierszDoWstawienia;
    }
    
    return( macierz3 );
  }
'})




library(rcpp)
cppFunction({'NumericMatrix matmultR(NumericMatrix m1, NumericMatrix m2){
    int c1 = m1.ncol();
    int c2 = m2.ncol();
    int r1 = m1.nrow();
    int r2 = m2.nrow();
    if(c1!=r2) { throw std::range_error("Nie można wykonać mnożenia"); 
    NumericMatrix m3(r1,c2);
    for(int i=0;i<r1;i++){
      NumericMatrix::Row actual_Row_M3 = m3(i, _ );
      NumericVector actual_Colmn_M1=m1(i,_);
      NumericVector insert_row(c2);
      for(int j=0;j<c2;j++){
        NumericVector actual_Colmn_M2 = m2(_,j);
        double suma=0.0;
        for(int m=0;m<r2;m++){
          suma += actual_Colmn_M1[m] * actual_Colmn_M2[m];
        }
        insert_row[j] = suma;
      }
      actual_Row_M3 = insert_row;
    }
    return (m3);
  }
'})




# Zadanie 5:
# a) Dokonaj przetestowania wydajno?ci opracowanych powy?ej funkcji na losowo stworzonych wektorach.
# b) Por?wnaj wydajno?? w?asnych funkcji z funkcjami wbudowanymi.
# c) Dokonaj profilowania opracowanych funkcji w celu znalezienia ew. w?skich garde?.