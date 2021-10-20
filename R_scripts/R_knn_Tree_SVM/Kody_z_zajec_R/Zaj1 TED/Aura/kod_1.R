6+5
5+7 7+7
5+7;7+7


#komentarz ctr+shift+c

#pakiety
install.packages("rpart")
library("rpart")

help("rpart")


#typy danych
# # dane typu atomowego
# -logiczny TRUE; FALSE lub T;F
typeof(T)
class(F)
str(F)
# -numeryczny: dla liczb całkowitych integer, double zmiennoprzecinkowe
# -complex (złożóne) liczby urojone
# -character czyli tekst
# -specjalne typy danych np braki danych, plus minus nieskończoność itd

#wekry c() od combine, laczy ten sam typ w wektory
c(T,F)

#dl wektra czyli liczba elementow, funkja lenght()
length(c(T,F,F,F))

#rep() o repeat powtarzanie elemenow wektora
rep(c(1,2),each=3)
rep(c(1,2),times=3)
rep(c(1,2),times=3,each=2) #each jest wazniejszy niz times
#wynik: 1 1 1 2 2 2, a bez each (inaczej times) wynik:1 2 1 2 1 2
#each daje mi to ze po kolei elementy wekktora sa powtarzane



# # złożonego (trzymaja różne typy danych)
# -macierze matrix()
# -tablice array()
# -data frame (ramka danych)
# -szeregi czasowe (time series)
# -factory (obiekt typu factor)


# # rekurencyjnego
# - listy
# - funkcje


#typeof() zwraca typ danych
#class() zwraca klase obiektow
# str() zwraca strukture danego obiektu, zwraca info jakie sa wartosci w srodku tego obiektu



