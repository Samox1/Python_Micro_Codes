### Laboratorium nr 12 - Zadanie 10
# Autor: Szymon Baczyński
# Data:  16.01.2022

rm(list = ls())

library(rpart)
library(rpart.plot)
library(MASS)
library(caret)


# 1) Wczytaj zbiór danych z pliku train.csv i zapoznaj się z nim. 
#     Rekordami tego zbioru są informacje o pasażerach statku Titanic. 
#     Zastanów się, które zmienne (kolumny) mogą być użyte do budowy drzewa klasyfikacyjnego. 
#     Sprawdź, czy w jakiś kolumnach nie występują brakujące obserwacje. 
#     Jeśli tak, to zastąp je średnią z kolumny.

dane <- read.csv("train.csv", stringsAsFactors = TRUE)
dane$Survived <- as.factor(dane$Survived)
summary(dane)

# sprawdzenie gdzie sa wartosci NA i puste stringi
summary(is.na(dane))
summary(dane == "")

# wypelnienie "Wieku" przy pomocy sredniej
dane$Age[is.na(dane$Age)] <- mean(dane$Age, na.rm = TRUE)
summary(dane)

# usuniecie rekordow z pusta kolumna "Embarked" (sztuk 2)
dane <- dane[dane$Embarked != "",]
print(summary(dane))

# lvl_Emb <- levels(dane$Embarked)
# dane$Embarked[dane$Embarked == ""] <- sample(lvl_Emb[lvl_Emb != ""])
# summary(dane)

hist(as.numeric(dane$Survived))
hist(dane$Pclass)
hist(as.numeric(dane$Sex))
hist(dane$Age)
hist(dane$SibSp)
hist(dane$Parch)
hist(dane$Fare)
hist(as.numeric(dane$Embarked))



# 2) Podziel zbiór na PU i podzbiór testowy przy pomocy funkcji createDataPartition() z parametrem p=0.75. 
#     Zbuduj drzewo klasyfikacyjne w oparciu o próbę uczącą.


dane.samples <- createDataPartition(dane$Survived, p = 0.75, list = FALSE)
dane.train <- dane[dane.samples,]
dane.test <- dane[-dane.samples,]

summary(dane.train)
summary(dane.test)


tree_1 <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch, dane.train)
print(tree_1)


# drugi wariant drzewa z dodatkowymi 2 kolumnami
tree_2 <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, dane.train)
tree_2



# 3) Wykonaj rysunek drzewa klasyfikacyjnego.

rpart.plot(tree_1, type = 1, extra = 1)

rpart.plot(tree_2, type = 1, extra = 1)



# 4) Przetestuj drzewo przy pomocy próby testowej. 
#     Wykonaj macierz pomyłek i sprawdź skuteczność klasyfikatora.

tree_1.predict <- predict(tree_1, newdata = dane.test, type = "class")
tree_1.true <- dane.test$Survived
tree_1.CM <- confusionMatrix(tree_1.predict, tree_1.true, positive = "1")
print(tree_1.CM)


tree_2.predict <- predict(tree_2, newdata = dane.test, type = "class")
tree_2.true <- dane.test$Survived
tree_2.CM <- confusionMatrix(tree_2.predict, tree_2.true, positive = "1")
tree_2.CM



# 5) Wykonaj walidację krzyżową drzewa klasyfikacyjnego.

tree_1_cv <- train(Survived ~ Pclass + Sex + Age + SibSp + Parch, data = dane.train, method="rpart",
         trControl = trainControl(method = "cv"))
print(tree_1_cv)


tree_2_cv <- train(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = dane.train, method="rpart",
                   trControl = trainControl(method = "cv"))
tree_2_cv




