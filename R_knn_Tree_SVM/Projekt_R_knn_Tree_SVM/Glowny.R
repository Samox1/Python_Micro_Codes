rm(list=ls())

library(caret)
library(rpart)
library(rpart.plot)
library(DiagrammeR)
library(nnet)
library(neuralnet)
library(kernlab)
library(e1071)
library(pROC)
library(ROCit)

source("funkcje.R")

### --- Binarna --- ###

# Dane: Transfusion.csv
# Zmienne są następujące:
#R. Recency - miesiące od ostatniej darowizny
#F. Częstotliwość - całkowita liczba darowizn
#M. Monetary - całkowita oddana krew w cm3 (ml)
#T. Czas - miesiące od pierwszej darowizny
#y. zmienna binarna określająca, czy oddał krew w marcu 2007 r. (1 = tak; 0 = nie)

Transfusion_Bin <- as.data.frame(read.csv(file="Transfusion.csv"))
colnames(Transfusion_Bin) <- c("Recency", "Frequency","Monetary","Time","Y_out")
Transfusion_Bin$Y_out <- factor(Transfusion_Bin$Y_out)

# Krotki przeglad danych
print("Ilosc danych wejsciowych: ")
print(length(Transfusion_Bin[,1]))
print(summary(Transfusion_Bin))

### ---> Trzeba podzielic zbior danych w miare rownomiernie jesli chodzi o klasy (0:570 & 1:178)

training.samples <- Transfusion_Bin$Y_out %>% createDataPartition(p = 0.8, list = FALSE)
train.data  <- Transfusion_Bin[training.samples, ]
test.data <- Transfusion_Bin[-training.samples, ]


# --- Drzewko Binarne - reczne --- #

Drzewko_Bin <- Tree( Y = "Y_out", Xnames = c("Recency", "Frequency","Monetary","Time"), data = train.data, depth = 5, minobs = 1)
# plot(Drzewko_Bin)


# pred_Tree <- predict(Drzewko_Bin, test.data, type="class")



# --- Drzewko Binarne - rpart --- #

Drzewko_Bin_rpart = rpart( formula = Y_out~., data = train.data, minsplit = 1, maxdepth = 5)
# rpart.plot(Drzewko_Bin_rpart, type = 1, extra = 1)

pred_Drzewko_Bin_rpart_class <- predict(Drzewko_Bin_rpart, newdata = test.data, type="class")
Tablica_Drzewko_Bin_rpart <- CM.large(test.data$Y_out, pred_Drzewko_Bin_rpart_class)
print(Tablica_Drzewko_Bin_rpart)

pred_Drzewko_Bin_rpart <- predict(Drzewko_Bin_rpart, newdata = test.data, type="prob")[,2]
ROCit_Drzewko_Bin_rpart <- rocit(score=pred_Drzewko_Bin_rpart, class = test.data$Y_out)
summary(ROCit_Drzewko_Bin_rpart)
plot(ROCit_Drzewko_Bin_rpart)





