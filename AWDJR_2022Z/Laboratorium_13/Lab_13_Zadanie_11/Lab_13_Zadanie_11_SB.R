### Laboratorium nr 13 - Zadanie 11
# Autor: Szymon Baczyński
# Data:  23.01.2022

rm(list = ls())

library(ggplot2)
library(caret)

set.seed(111)


# 1) Wczytaj zbiór danych iris z biblioteki datasets i przeskaluj wektory numeryczne w tej ramce danych 
#     odejmując wartość średnią i dzieląc przez odchylenie standardowe.

dane_iris <- data.frame(datasets::iris)
head(dane_iris)
summary(dane_iris)

iris.scale <- scale(dane_iris[,1:4])
iris.scale <- cbind(data.frame(iris.scale), Species = dane_iris[,5])
head(iris.scale)
summary(iris.scale)



# 2) Wykonaj wielokrotnie analizę skupień dla przeskalowanych danych oraz K∈{1,2,…,10}. Dla każdego K oblicz statystykę odstępu GS(K).

random_points <- function(n, K, x1range, x2range, x3range, x4range) {
  x1 <- runif(n, x1range[1], x1range[2])
  x2 <- runif(n, x2range[1], x2range[2])
  x3 <- runif(n, x3range[1], x3range[2])
  x4 <- runif(n, x4range[1], x4range[2])
  cl <- kmeans (cbind(x1,x2,x3,x4), K)
  return (cl$tot.withinss)
}

n = nrow(iris.scale)

K <- 1:10
iris.scale.rand <- sapply (K, function(k) mean (sapply (1:20, function(i) random_points(n, k, range(iris.scale[,1]), range(iris.scale[,2]), range(iris.scale[,3]),range(iris.scale[,4]) ))))
iris.scale.data <- sapply(K, function(k) mean (sapply (1:20, function(i) kmeans(iris.scale[,1:4], k)$tot.withinss)))



# 3) Wykonaj wykresy ⟨Wrand(K)⟩, ⟨Wdata(K)⟩ oraz GS(K).

W.df <- data.frame (K = K,
                    W.rand = iris.scale.rand,
                    W.data = iris.scale.data,
                    GS = log(iris.scale.rand/iris.scale.data))

g <- ggplot(W.df)

g + theme_bw() + 
  geom_point ( aes (x=K, y=W.rand, color="losowe"), size=3) +
  geom_point ( aes (x=K, y=W.data, color="oryginalne"), size=3) +
  labs (x = "K", y="W", color="dane")

g + theme_bw() +  geom_point ( aes (x=K, y=GS), size=3) +
  labs (x="K", y=expression(lnW[rnd]-lnW[data]))



# 4) Wykonaj klastrowanie hierarchiczne przeskalowanych danych. Na podstawie rysunku dendrogramu oceń najbardziej prawdopodobną liczbę skupień.

hc <- hclust (dist (iris.scale[,1:4]))
plot(hc, hang = -1)



# 5) Wykonaj macierz pomyłek i oblicz dokładność klasyfikacji dla K=3.

iris.scale.kmeans3 <- kmeans(iris.scale[,1:4], 3)

iris.scale.predict <- factor(iris.scale.kmeans3$cluster, labels = levels(iris.scale$Species))
iris.scale.true <- as.factor(iris.scale$Species)
iris.scale.cm <- confusionMatrix(iris.scale.predict, iris.scale.true)
print(iris.scale.cm)


