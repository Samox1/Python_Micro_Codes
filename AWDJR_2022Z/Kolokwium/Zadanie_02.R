### KOLOKWIUM - Zadanie 2
# Autor: Szymon Baczyński
# Data:  31.01.2022


rm(list = ls())


## Do zadań należy wykorzystać zbiór danych umieszczony w pliku breast_cancer.txt. 
## Zawiera on 699 obserwacji 11 zmiennych opisujących różne parametry komórek nowotworu piersi. 
## Pierwsza zmienna jest identyfikatorem pacjenta, kolejnych dziewięć jest typu numerycznego o wartościach całkowitych od 1 do 10. 
## Zmienna Class określa, czy nowotwór został oceniony jako łagodny czy złośliwy. Opisy kolumny są następujące:


library(caret)
library(ROCit)



# 1) Wczytaj dane z pliku i upewnij się, że kolumny są odpowiedniego typu. 
#   Usuń rekordy zawierające brakujące obserwacje.

dane <- read.csv("breast_cancer.txt", sep = " ", stringsAsFactors = TRUE)
summary(dane)
dane <- na.omit(dane)
summary(dane)



# 2) Wykonaj regresję logistyczną traktując zmienną Class jako wynikową, a dziewięć zmiennych numerycznych jako predyktory. 
#   Określ, które zmienne są (nie)istotne statystycznie.

model.glm_all <- train(Class ~ . , data = dane[2:11], method = "glm")
summary(model.glm_all)

# Komentarz:
# Statystycznie wazne zmienne to: Cl.thickness, Marg.adhesion, Bare.nuclei, Bl.cromatin (+ Normal.nucleoli)



# 3) Podziel zbiór danych na próbę uczącą i testową w stosunku 1:1. Na próbie uczącej wytrenuj regresję logistyczną, 
#   a następnie dla próby testowej wyznacz prawdopodobieństwa tego, że nowotwór jest złośliwy.

dane.samples <- createDataPartition(dane$Class, p = 0.5, list = FALSE)
dane.train <- dane[dane.samples,]
dane.test <- dane[-dane.samples,]

summary(dane.train)
summary(dane.test)


model.glm <- train(Class ~ Cl.thickness + Marg.adhesion + Bare.nuclei + Bl.cromatin + Normal.nucleoli, data = dane.train[2:11], method = "glm")
summary(model.glm)

dane.predict <- predict(model.glm, newdata=dane.test[2:11], type = "prob")[,2]
summary(dane.predict)



# 4) Wykonaj rysunek krzywej ROC i znajdź optymalny Indeks Youdena i odpowiadający mu optymalny próg.

rocit.obj <- rocit(score=dane.predict, class=dane.test$Class)
summary(rocit.obj)
plot(rocit.obj)
best.yi.index <- which.max(rocit.obj$TPR-rocit.obj$FPR)
best.cutoff <- rocit.obj$Cutoff[best.yi.index]
best.tpr <- rocit.obj$TPR[best.yi.index]
best.fpr <- rocit.obj$FPR[best.yi.index]
print(sprintf("Optimal Point = %.2f (TPR = %.3f, FPR = %.3f)", best.cutoff, best.tpr, best.fpr))



# 5) Wykorzystaj znaleziony próg do zamiany prawdopodobieństw na klasy, 
#   a następnie wykonaj i wyświetl macierz pomyłek oraz dokładność klasyfikatora.

dane.result <- as.factor(ifelse(dane.predict > best.cutoff, "malignant", "benign"))
dane.conf <- confusionMatrix(dane.result, dane.test$Class, positive = "malignant")
print(dane.conf)



# 6) Dla pełnych danych przeprowadź 10-krotną walidację krzyżową z trzema powtórzeniami. 
#   Testy przeprowadź dla trzech metod: drzew losowych, LDA i QDA. Wyświetl dokładność każdej z metod.

model.tree_cv <- train(Class ~ Cl.thickness + Marg.adhesion + Bare.nuclei + Bl.cromatin + Normal.nucleoli , data = dane, method="rpart", 
                      trControl = trainControl(method = "cv"), tuneLength = 3)
print(model.tree_cv)


model.lda_cv <- train(Class ~ Cl.thickness + Marg.adhesion + Bare.nuclei + Bl.cromatin + Normal.nucleoli , data = dane, method="lda", 
                       trControl = trainControl(method = "cv"), tuneLength = 3)
print(model.lda_cv)


model.qda_cv <- train(Class ~ Cl.thickness + Marg.adhesion + Bare.nuclei + Bl.cromatin + Normal.nucleoli , data = dane, method="qda", 
                      trControl = trainControl(method = "cv"), tuneLength = 3)
print(model.qda_cv)

