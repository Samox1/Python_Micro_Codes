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
# 
# Zadanie 2:
# a) Stwórz funkcję "AssignInitialMeasures" przyjmującą nastęujące parametry: "tree", "Y", "data", "type", "depth".
# b) Funkcja powinna na podstawie parametrów wejściowych przypisywać do obiektu "tree" (czyli korzenia) wartości początkowe:
#    - "depth" = 0.
#    - w zależności od "type" wartość miary Gini, Entropy, SS dla calej populacji (bo to korzeń).
#
#   
# Zadanie 3:
# a) Stwórz funkcję "AssignInfo" przyjmującą nastęujące parametry: "tree", "Y", "X", "data", "type", "depth", "minobs", "overfit", "cf".
# b) Funkcja powinna na podstawie parametrów wejściowych przypisywać do obiektu "tree" (jako attrybuty obiektu) wartości owych parametrów.
#
#
# Zadanie 4:
# a) Stwórz funkcję "FindBestSplit" przyjmującą nastęujące parametry: "Y", "X", "data", "parentVal", "type", "minobs".
# b) Funkcja powinna zwracać tabelę z wynikami najlepszego możliwego podziału, zawierjącą:
#    - "infGain" - zysk informacyjny dla podziału, 
#    - "lVal" - miarę niejednorodności dla lewego węzła, 
#    - "rVal" - miarę niejednorodności dla prawego węzła,
#    - "point" - punkt (lub zbiór punktów dla zmiennych kategorycznych) podzału,
#    - "Ln" - liczbę obserwacji w lewym węźle, 
#    - "Rn" - liczbę obserwacji w prawym węźle.   
#
#
# Zadanie 5:
# a) Stwórz funkcję "Tree" przyjmującą nastęujące parametry: "Y", "X", "data", "type", "depth", "minobs", "overfit", "cf".
# b) Jest to rozwinięcie funkcji ze slajdu nr 19. Funckja powinna po kolei wywoływać pozostałe funkcje:
#    - "StopIfNot", jeżeli zwracana wartość to "FALSE" to kończymy działanie całej funkcji (zwracamy obiekt niewidzialny),
#    - tworzenie obiektu "tree",
#    - "AssignInitialMeasures",
#    - "BuildTree",
#    - "PruneTree", na tę chwilę ta funkcja jest pusta PruneTree<-function(){},
#    - "AssignInfo".
# c) Funkcja powwina zwracać obiekt "tree".
#
# 
# Zadanie 6:
# a) Dokonaj integracji opracowanej funkcji "FindBestSplit" z funkcjami "Tree" oraz "BuildTree".
