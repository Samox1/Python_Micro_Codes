# Plik proszę nazwać numerem swojego indeksu.
# Plik powinien zawierać tylko definicję funkcji z Zadanie 1-2.
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
# Zadanie 2:s
# a) Zasymulujmy środowisko pracy: Zakładamy, że komputer ma tylko 50MB RAM.
# b) Stwórz funkcję przyjmującą nastęujące parametry: "dane", "filtr".
# c) Funkcja powinna w pierwszym kroku sprawdzać rozmiar obiektu "dane". 
#    Jeżeli "dane" mają rozmiar większy niż dostępna pamięć RAM, obiekt powinien zostać w odpowiedni sposób przetworzony.
# d) Funkcja powinna następnie filtrować wiersze w obiekcie "dane" poprzez parametr "filtr", 
#    przyjmujący warunek w formie tekstowej o postaci: "Nazwa_zmiennej operator wartość (%,|) ...".
#    Przykład: "x5 >= 0.5" lub "x8 %in% c(0.3,0.6)" lub "x2 >= 0.5 & x3 < 0.1".
#    Podpowiedź: z racji tego, że "filtr" jest tekstem, należy przetowrzyć go na wyrażenie.
# e) Funkcja powinna zwracać wyfiltrowany obiekt "dane" oraz działać SZYBKO.
# f) Nazwa funkcji to "Filtr".
# 
# Zadanie 3:
# a) Przetestuj działanie opracowanych funkcji na tabeli z "Zadanie 0".