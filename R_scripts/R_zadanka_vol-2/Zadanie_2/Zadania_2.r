# Plik proszę nazwać numerem swojego indeksu.
# Plik powinien zawierać tylko definicję funkcji z Zadanie 1-3.
# 
# Zadanie 0:
# a) Stwórz macierz rozmiaru "1000x1001". 
#    Przypisz nazwę "Y" do pierwszej kolumny. Przypisz nazwy od "x_1" do "x_1000" do następnych kolumn.
# b) Wstaw losowe wartości z wektora od 1 do 100 w kolumnę "Y". set.seed = (555).
# c) Wstaw do kolumn od "x_1" do "x_1000" wartości zgodne z nastepujacym schematem 
#    "x_i = Y + wartość losowa z rozkładu normalnego". set.seed = (555).
# 
# Zadanie 1:
# a) Stwórz funkcję przyjmującą nastęujące parametry: "dane", "Ynazwa", "XnazwyList", "Nrdzeni", "metoda".
# b) Funkcja operując na zbiorze "dane" powinna tworzyć model regresji liniowej dla Ynazwa w odniesieniu do zmiennych XnazwyList.
#    W najprostszej postaci dla danych z "Zadanie 0" są to modele: ("Y~x1", "Y~x2", "Y~x3" etc.).
#    To jakiej postaci model powinien być zbudowany, definiowane jest przez parametr "XnazwyList", przyjmujący obiekt typu lista.
#    Lista ma tyle elementów, ile modeli będzie zbudowanych. Każdy element listy jest wektorem nazw zmiennych "x".
#    Przykład: "list(x1,c(x1,x5,x7))" buduje dwa modele 1) "Y~x1" oraz 2) "Y~x1+x5+x7".
# c) Funkcja powinna budować każdą kombinację modeli równolegle.
# d) W zależności od przekazanego argumentu do "metoda", funkcja wykorzystywać powinna albo równoleglą wersję "lapply",
#    albo równoleglą wersję pętli "for".
# e) Każda równoległa pętla powinna zwracać informacje o nazwach zmiennej/zmiennych (pierwsza kolumna tekstowa) 
#    i oszacowaniach parametrów (druga kolumna numeryczna).
# f) Funkja powinna zwracać wyniki w formie listy.
# g) Nazwa funkcji to "ModelParallel".
# 
# Zadanie 2:
# a) Stwórz funkcję "ModelOcena" przyjmującą nastęujące parametry: "y_tar" (rzeczywista), "y_hat" (prognoza).
# b) Funkcja w pierwszym kroku powinna rozpoznawać czy mamy do czynienia z problemem regresji czy klasyfikacji. 
#    y_tar: regresja -> numeric, klasyfikacja -> factor.
# c) W zależności od problemu funkcja szacować powinna różnego rodzaju błędy:
#    Regresja: MAE, MSE, MAPE.
#    Klasyfikacja: AUC (szacowanie metodą trapezów), macierz klasyfikacji (punkt odcięcia wyznaczany jest poprzez index Youdena), 
#                  Czułość, Specyficzność, Jakość.
# d) Dla czytelności kodu, wszystkie miary powinny być liczone w oparciu o zewnętrzne funkcje (dowolne nazwy), 
#    których definicje znajdować się powinny przed definicją funkcji "ModelOcena". 
# e) Funkja powinna zwracać wyniki w formie: 
#    Regresja: nazwany wektor (MAE, MSE, MAPE) o trzech elementach.
#    Klasyfikacja: nazwana lista (Mat, J, Miary) o trzech elementach:
#                  Mat = macierz klasyfikacji, w wierszach znajdują się wartości "y_tar" a w kolumnach wartości "y_hat",
#                        nazwy wierszy i kolumn muszą być zgodne z dostępnymi etykietami klas.
#                  J = wartość indexu Youdena,
#                  Miary = nazwany wektor o elementach AUC, Czułość, Specyficzność, Jakość.
# f) Funkcja będzie testowana tylko dla klasyfikacji binarnej i regresji.
# 
# Zadanie 3:
# a) Stwórz funkcję "CrossValidTune" przyjmującą nastęujące parametry: "dane", "kFold", "parTune", "seed". 
#    W skrócie: funkcja powinna krosswalidacyjnie tunować parametry danego algorytu. 
# b) Funkcja powinna w pierwszym kroku stworzyć listę przechowującą informację, które obserwacje posłużą jako zbiór treningowy,
#    a które wejdą do zbioru walidacyjnego. Ilość elementów listy zdefiniowana jest przez parametr "kFold" (liczba podzbiorów walidacyjnych).
#    Każdy element listy jest wektorem o tej samej długości, równej nrow("dane").
#    Każdy z wektorów zawiera albo liczbę 1 (obserwacja jest w zbiorze treningowym) albo 2 (obserwacja jest w zbiorze walidacyjnym). 
#    Przykład: list( c(1,2,1,1), c(2,1,1,2) ) - oznacza, że mamy doczynienia z 2-krotną walidacją na zbiorze z 4 obserwacjami, 
#    gdzie dla pierwszej iteracji tylko jeden element jest w podzbiorze walidacyjnym.
#    Losowanie rozpoczyna się od ustawienia ziarna na wartość "seed".
# c) W kolejnym kroku funkcja powinna stworzyć ramkę danych, w której przechowywane będą wyniki oraz kombinacje parametrów.
#    Liczba wierszy i kolumn zależy od zagadnienia (klasyfikacja, regresja) oraz od liczby tunowanych parametrów "parTune" i "kFold":
#    Przykład: "parTune" = data.frame( a = c(1,2), b = c(1,1) ) - oznacza, że algorytm ma 2 parametry do tunowania,
#              Dla "kFold" = 2 oraz "parTune", kombinacja parametrów to data.frame( "kFold" = c(1,2,1,2), a = c(1,2), b = c(1,1) ).
#              Kolejne kolumny tabeli wynikowej powinny stanowić miary uzyskane dzięki funkcji "ModelOcena".
#              Regresja: MAEt, MSEt, MAPEt, MAEw, MSEw, MAPEw - ozanczają miary dla zbioru treningowego i walidacyjnego.
#                        Finalnie tabele jest rozmiaru 4x9.
#              Klasyfikacja: AUCT, CzułośćT, SpecyficznośćT, JakośćT, AUCW, SpecyficznośćW, MAPEW, JakośćW - j.w.
#                            Finalnie tabele jest rozmiaru 4x11.
# d) W ostatnim kroku funkcja powinna budować w pętli model predykcyjny dla danej kombincaji parametrów i uzupełniać tabelę wynikową.      
#    Z racji tego, że nie stworzyliśmy na razie żadnego algorytmu ta część powinna działać następująco:
#    Każda pętla tworzy dwa podzbiory zdefiniowane przez wektor znajdujący się w liście z pkt b) dla danej kombinacji.
#    Do kolumn z miarami jakości wstawiane są wartości równe 0.
# e) Funkcja zwraca tabelę wynikową.