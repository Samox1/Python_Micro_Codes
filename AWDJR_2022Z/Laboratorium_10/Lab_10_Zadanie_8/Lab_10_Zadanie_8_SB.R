### Laboratorium nr 10 - Zadanie 8
# Autor: Szymon Baczyński
# Data:  02.01.2022

rm(list = ls())


library(caret)
library(carData)
library(ROCit)


# 1) Wczytaj zbiór danych Greene z biblioteki carData i zapoznaj się z nim. 
#   Podziel zbiór na podzbiór uczący (80%) i testowy (20%).

dane <- data.frame(carData::Greene)
print(summary(dane))

# set.seed(1024)
dane.samples <- createDataPartition(dane$decision, p = 0.8, list = FALSE)
dane.train <- dane[dane.samples,]
dane.test <- dane[-dane.samples,]

summary(dane.train)
summary(dane.test)



# 2) GLM = x(judge, nation), y(decision)

model.glm2col <- train(decision ~ judge + nation, data = dane.train, method = "glm")
summary(model.glm2col)
# Komentarz: statystycznie wazne dane sa w kolumnie "judge"

model.glm1col <- train(decision ~ judge, data = dane.train, method = "glm")
summary(model.glm1col)



# 3) Przy pomocy funkcji predict() wykonaj predykcję zmiennej decision dla obserwacji znajdujących się w zbiorze testowym

dane.predict2col <- predict(model.glm2col, newdata=dane.test, type = "prob")[,2]
summary(dane.predict2col)

dane.predict1col <- predict(model.glm1col, newdata=dane.test, type = "prob")[,2]
summary(dane.predict1col)



# 4) Sporządź krzywą ROC i znajdź optymalny punkt podziału. 
#   Odpowiedz na pytanie dlaczego kształt krzywej ROC jest inny przy każdym uruchomieniu skryptu.

rocit.obj <- rocit(score=dane.predict2col,class=dane.test$decision)
summary(rocit.obj)
plot(rocit.obj)
best.yi.index <- which.max(rocit.obj$TPR-rocit.obj$FPR)
best.cutoff.2col <- rocit.obj$Cutoff[best.yi.index]
best.tpr <- rocit.obj$TPR[best.yi.index]
best.fpr <- rocit.obj$FPR[best.yi.index]
print(sprintf("Optimal Point (2col) = %.2f (TPR = %.3f, FPR = %.3f)", best.cutoff.2col, best.tpr, best.fpr))


rocit.obj <- rocit(score=dane.predict1col,class=dane.test$decision)
summary(rocit.obj)
plot(rocit.obj)
best.yi.index <- which.max(rocit.obj$TPR-rocit.obj$FPR)
best.cutoff.1col <- rocit.obj$Cutoff[best.yi.index]
best.tpr <- rocit.obj$TPR[best.yi.index]
best.fpr <- rocit.obj$FPR[best.yi.index]
print(sprintf("Best Cutoff (1col) = %.2f (TPR = %.3f, FPR = %.3f)", best.cutoff.1col, best.tpr, best.fpr))

# Komentarz: 
# Krzywa ROC jest za każdym razem inna ze względu na brak ustawienia ziarna generatora licz losowych



# 5) Przyjmując prawdopodobieństwo progowe z pkt. 4 wykonaj macierz pomyłek oraz podaj skuteczność klasyfikacji. 
#   W komentarzu oceń jakość klasyfikatora

dane.result.2col <- as.factor(ifelse(dane.predict2col>best.cutoff.2col, "yes", "no"))
dane.conf2col <- confusionMatrix(dane.result.2col, dane.test$decision, positive = "yes")
#print(dane.conf2col)

dane.result.1col <- as.factor(ifelse(dane.predict1col>best.cutoff.1col, "yes", "no"))
dane.conf1col <- confusionMatrix(dane.result.1col, dane.test$decision, positive = "yes")
print(dane.conf1col)

# Komentarz: 
# Klasyfikator dziala lepiej / gorzej w zależności od jakości zestawu uczącego (losowanie próbki bez ustawionego ziarna),
# na próbie kilkunastu uruchomień skryptu "Accuracy" osiąga wartości od 0.55 do 0.8 (dla modelu tylko dla kolumny "judge")

print("Porownanie modeli 'judge + nation' i 'judge' ")
print(cbind("judge + nation" = dane.conf2col$overall, "judge" = dane.conf1col$overall))
print(cbind("judge + nation" = dane.conf2col$byClass, "judge" = dane.conf1col$byClass))

