### Laboratorium nr 11 - Zadanie 9
# Autor: Szymon Baczyński
# Data:  09.01.2022

rm(list = ls())

library(caret)

# 1) Wczytaj zbiór danych iris z biblioteki datasets i zapoznaj się z nim. 
#   Przeskaluj wektory numeryczne w tej ramce danych odejmując wartość średnią 
#   i dzieląc przez odchylenie standardowe (można użyć funkcji scale() z biblioteki MASS)

dane_iris <- data.frame(datasets::iris)
head(dane_iris)
summary(dane_iris)

iris.scale <- scale(dane_iris[,1:4])
iris.scale <- cbind(data.frame(iris.scale), Species = dane_iris[,5])
head(iris.scale)
summary(iris.scale)



# 2) Wykonaj liniową analizę dyskryminacyjną na całym zbiorze

iris.lda <- lda(Species ~ . , data = iris.scale)
print(iris.lda)



# 3) Zrzutuj wszystkie obserwacje (oryginalnie czterowymiarowe) na 
#   płaszczyznę dwuwymiarową określoną przez proste wyznaczone w LDA.

iris.proj <- as.matrix(iris.scale[,1:4]) %*% iris.lda$scaling
iris.proj <- data.frame(iris.proj, klasa=dane_iris$Species)



# 4) Dokonaj wizualizacji tak przetransformowanych obserwacji w pakiecie ggplot2

g <- ggplot(iris.proj, aes(x=LD1, y=LD2)) + geom_point(aes(color=klasa), size=3)
plot(g)



# 5) Sprawdź skuteczność klasyfikacji LDA wykonując dowolną walidację krzyżową w pakiecie caret.

iris.lda.caret <- train(Species ~ ., data=iris.scale, method="lda",
                        trControl = trainControl(method = "cv"))

print(iris.lda.caret)
