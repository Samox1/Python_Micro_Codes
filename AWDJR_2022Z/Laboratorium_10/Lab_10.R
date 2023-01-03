# LABORATORIUM 10


# Ocena skuteczności klasyfikatora
# Krzywa ROC
# Walidacja krzyżowa




### Ocena skuteczności klasyfikatora ###

library(MASS)
# Regresja liniowa oparta o statystycznie istotne czynniki: glu i ped
pima.lr <- glm (type~glu+ped, data=Pima.tr, family = binomial(link = "logit"))

# Predykcja
pima.predict <- predict (pima.lr, newdata = Pima.te, type = "response")
pima.result <- ifelse (pima.predict>0.50, "Yes", "No")
pima.true <- Pima.te[,8]

# Macierz pomyłek
pima.cm <- table(pima.true,pima.result)
pima.cm


TN <- pima.cm[1,1]
TP <- pima.cm[2,2]
FP <- pima.cm[1,2]
FN <- pima.cm[2,1]
P <- TP+FN
N <- TN+FP
# Czułość
TP/P

# Specyficzność
TN/N

# Precyzja
TP/(TP+FP)

# Wartość predykcyjna ujemna
TN/(TN+FN)

# Dokładność
(TN+TP)/sum(pima.cm)

# Miara F1
2*TP/(2*TP+FP+FN)


library(caret)
pima.conf <- confusionMatrix (as.factor(pima.result), pima.true, positive = "Yes")
pima.conf

pima.conf$byClass



### Krzywa ROC (Receiver Operating Characteristic) ###

progi <- seq(0.04,0.99,0.01)
roc <- sapply (progi, function(p)
  confusionMatrix (as.factor(ifelse(pima.predict>p, "Yes", "No")), pima.true, positive = "Yes")$byClass[c("Sensitivity","Specificity")])
plot(1-roc["Specificity",], roc["Sensitivity",], xlab="1-specyficzność", ylab="czułość", main="ROC", type = "l")


library(PRROC)
pima.true <- ifelse(Pima.te[,8]=="Yes", 1, 0)
prroc.obj <- roc.curve(scores.class0 = pima.predict, weights.class0 = pima.true, curve=TRUE)
plot(prroc.obj)

library(ROCit)
rocit.obj <- rocit(score=pima.predict,class=Pima.te[,8])
summary(rocit.obj)

plot(rocit.obj)

best.yi.index <- which.max(rocit.obj$TPR-rocit.obj$FPR)
best.cutoff <- rocit.obj$Cutoff[best.yi.index]
best.tpr <- rocit.obj$TPR[best.yi.index]
best.fpr <- rocit.obj$FPR[best.yi.index]
sprintf("Best Cutoff = %.2f (TPR = %.3f, FPR = %.3f)", best.cutoff, best.tpr, best.fpr)



### Walidacja krzyżowa ###

data("swiss")
# Dane dotyczą płodności w 47 prowincjach Szwajcarii w roku 1888
head(swiss)

set.seed(123) # Ustawiamy ziarno generatora liczb losowych
swiss.samples <- createDataPartition(swiss$Fertility, p = 0.8, list = FALSE)
swiss.train <- swiss[swiss.samples,]
swiss.test <- swiss[-swiss.samples,]
swiss.lm <- lm(Fertility ~., data = swiss.train)
swiss.predict <- predict(swiss.lm, newdata=swiss.test)
data.frame (R2 = R2(swiss.predict, swiss.test$Fertility),
            RMSE = RMSE(swiss.predict, swiss.test$Fertility),
            MAE = MAE(swiss.predict, swiss.test$Fertility))

# Wypisanie modeli kompatybilnych z funkcją train()
names(getModelInfo())

# Bootstrap
train.control <- trainControl(method="boot", number=100)
swiss.boot <- train(Fertility ~ ., data = swiss, method = "lm", trControl = train.control)
print(swiss.boot)

# Leave one out cross validation (LOOCV)
train.control <- trainControl(method = "LOOCV")
swiss.loocv <- train(Fertility ~ ., data = swiss, method = "lm", trControl = train.control)
print(swiss.loocv)

# 10-krotna walidacja krzyżowa
train.control <- trainControl(method = "cv", number=10)
swiss.cv <- train(Fertility ~ ., data = swiss, method = "lm", trControl = train.control)
print(swiss.cv)

# 10-krotna walidacja krzyżowa powtórzona 3 razy
train.control <- trainControl(method = "repeatedcv", number=10, repeats=3)
swiss.repeatedcv <- train(Fertility ~ ., data = swiss, method = "lm", trControl = train.control)
print(swiss.repeatedcv)

