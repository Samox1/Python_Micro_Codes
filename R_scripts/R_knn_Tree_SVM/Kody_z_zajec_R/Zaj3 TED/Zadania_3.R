# Plik proszę nazwać numerem swojego indeksu.
# 
# Zadanie 1:
# a) Stwórz funkcję "KNNtrain" przyjmującą nastęujące parametry: "X", "y_tar", "k", "XminNew", "XmaxNew".
# b) Funkcja w pierwszym kroku powinna sprawdzać czy analiza jest możliwa do wykonania tj:
#    czy "X" oraz "y_tar" nie mają braków danych, czy "k" jest większe od 0, czy "X" jest macierzą lub ramką danych.
# c) W drugim kroku funkcja dokonuje normalizacji zmiennych z "X" do przedziału "XminNew", "XmaxNew". Jest to możliwe dla zmiennych na skali ilorazowej.
#    Nowa tabela powinna posiadać 3 atrubuty informujące o (każdy jest wektorem):
#      - wartościach minimalnych attr(*,"minOrg") dla każdej zmiennej,
#      - wartościach maksymalnych attr(*,"maxOrg") dla każdej zmiennej,
#      - nowych wartościch minimalnych i maksymalnych attr(*,"minmaxNew").
# d) W kolejnym kroku w obiekcie typu lista o trzech elementach "X","y","k", funkcja umieszcza odpowiednie obiekty.
# e) Funkcja powinna zwracać listę z pkt d).
# f) Funkcja będzie testowana tylko dla klasyfikacji binarnej i regresji.
# ***) Dla ambitnych: Funkcja może przyjmować dodatkowo parametr "metoda = brute". Oznacza to, że domyślnie nie następuje faza treningowa, 
#      lecz w przypadku przekazania argumentu "kdtree", funkcja przeorganizuję tabelę "X" w pomocniczą strukturę k-d tree. 
#      Taki zabieg będzie wpływał na sposób wykonania Zadania 2. Ta część może być napisana w C/C++. 
#      Struktura k-d tree jest możliwa do stworzenia dla zmiennych na skali ilorazowej.
# 
# Zadanie 2:
# a) Stwórz funkcję "KNNpred" przyjmującą nastęujące parametry: "KNNmodel", "X".
# b) Funkcja w pierwszym kroku powinna sprawdzać czy predykcja jest możliwa do wykonania tj:
#    czy "X" nie ma braków danych, czy wszystkie potrzebne zmienne/kolumny istnieją zarówno w "KNNmodel$X" jak i w "X".
# c) Następnie funkcja powinna normalizaowć dane z "X" uwzględniając informacje zawarte w atrybutach obiektu "KNNmodel$X".
# d) W zależności od typu zmiennych w obiektach "KNNmodel$X" i "X" odległość pomiędzy obserwacjami powinna być liczona odległością:
#    Euklidesa, Hamminga lub Gowera.
# e) Funkcja powinna następnie rozpoznawać czy mamy do czynienia z problemem regresji czy klasyfikacji, 
#    a następnie dokonywać odpowiedniej agregacji wyników (średnia lub głosowanie większościowe).
# f) Funkcja powinna zwracać:
#    - dla regresji: wektor z wartościami przewidywanymi.
#    - dla klasyfikacji: nazwaną ramkę danych o rozmiarze "n x k+1", np. dla wersji binarnej o etykietach "P", "N",
#      tabela wygląda następująco: data.frame( P = c(0.3,0.6), N = c(0.7,0.4), Klasa = c("N","P") ), 
#      tj. pierwsze dwie kolumny zawierają prawdopodobieństwo przynależności danej obserwacji do danej klasy (nazwy kolumn są ważne),
#      ostatnia kolumna wskazuję klasę - jest to factor o poziomach takich jak originalna zmienna "y_tar" z Zadania 1. 
# g) Funkcja będzie testowana dla problemu regresji i klasyfikacji binarnej na zbiorze danych zawierającym:
#    - tylko zmienne na skali ilorazowej.
#    - tylko zmienne na skali porządkowej.
#    - tylko zmienne na skali nominalnej.
#    - zmienne na skali mieszanej.
# ***) Dla ambitnych: W zależności od sposoby wykonania Zadania 1, funkcja wyszukuje najbliższych sąsiadów na podstawie struktury k-d tree.
#   