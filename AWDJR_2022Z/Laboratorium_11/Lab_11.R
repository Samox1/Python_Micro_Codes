# LABORATORIUM 11


# Liniowa Analiza Dyskryminacyjna
# Kwadratowa Analiza Dyskryminacyjna
# Maszyny Wektorów Nośnych



### Liniowa Analiza Dyskryminacyjna ###

library(MASS)
library(ggplot2)

S <- matrix(c(3,0,0,3),2,2)
m1 <- c(2,2)
m2 <- c(-1,-1)

n1 <- 60
n2 <- 20
n <- n1 + n2

x1 <- mvrnorm(n1, m1, S)
x2 <- mvrnorm(n2, m2, S)

dane <- data.frame (wsp1 = c(x1[,1],x2[,1]),
                    wsp2 = c(x1[,2],x2[,2]),
                    klasa = as.factor (rep (c("1","2"), c(n1,n2))))

theme_set(theme_bw())
ggplot(dane) + geom_point(aes(x = wsp1, y = wsp2, color = klasa), shape = 19, size = 3)


dane.lda <- lda(klasa ~ wsp1 + wsp2, data = dane)
dane.lda


proj <- as.matrix(dane[,1:2]) %*% dane.lda$scaling
dane$proj <- proj[,1]
ggplot(dane) + geom_point(aes(x = proj, y = 0, color = klasa), shape=21, size=5) + geom_vline(xintercept=0)


library(klaR)
partimat(klasa ~ wsp1 + wsp2, data = dane, method="lda")


library(caret)
dane.pred <- predict(dane.lda, dane)
dane.conf <- confusionMatrix(dane.pred$class,dane$klasa, positive = "1")
dane.conf



### Kwadratowa Analiza Dyskryminacyjna ###

S1 <- matrix(c(4,0,0,4),2,2)
S2 <- matrix(c(2,0,0,1),2,2)
S3 <- matrix(c(1,0,0,4),2,2)
m1 <- c(3,3)
m2 <- c(-1,-1)
m3 <- c(4,-2)

n1 <- 60
n2 <- 40
n3 <- 50
n <- n1 + n2 + n3

x1 <- mvrnorm(n1, m1, S1)
x2 <- mvrnorm(n2, m2, S2)
x3 <- mvrnorm(n3, m3, S3)

dane <- data.frame (wsp1 = c(x1[,1], x2[,1], x3[,1]),
                    wsp2 = c(x1[,2], x2[,2], x3[,2]),
                    klasa = as.factor (rep (c("1", "2", "3"), c(n1, n2, n3))))

ggplot(dane) + geom_point(aes(x = wsp1, y = wsp2, color = klasa), shape = 19, size = 3)


partimat(klasa ~ wsp1 + wsp2, data = dane, method="lda")


partimat(klasa ~ wsp1 + wsp2, data = dane, method="qda")


# Trenujemy klasyfikator LDA i testujemy go przy użyciu 10-krotnej walidacji krzyżowej
dane.lda.cv <- train(klasa ~ wsp1 + wsp2, data = dane, method="lda", trControl = trainControl(method = "cv", number=10))
# Trenujemy klasyfikator QDA i testujemy go przy użyciu 10-krotnej walidacji krzyżowej
dane.qda.cv <- train(klasa ~ wsp1 + wsp2, data = dane, method="qda", trControl = trainControl(method = "cv", number=10))

dane.lda.cv
dane.qda.cv


# Obserwacje zrzutowane na proste ortogonalne do prostych rozdzielających
dane.lda <- lda(klasa ~ wsp1 + wsp2, data = dane)
proj <- as.matrix(dane[,1:2]) %*% dane.lda$scaling
dane.proj <- data.frame(proj, klasa=dane$klasa)
g <- ggplot(dane.proj, aes(x=LD1, y=LD2))
g + geom_point(aes(color=klasa), size=3)




### Maszyny Wektorów Nośnych ###

x <- c(1.0, 1.5, 1.0, 1.5, 2.5, 3.5, 2.0, 3.0, 3.5, 4.5, 5.0, 5.0)
y <- c(2.0, 1.0, 4.0, 3.0, 2.0, 2.0, 5.5, 5.0, 4.0, 4.5, 2.0, 3.5)

dane <- data.frame (x = x,
                    y = y,
                    klasa = as.factor (rep (c("-1", "1"), each=6)))

g <- ggplot(dane) + geom_point (aes (x = x, y = y, color = klasa), shape = 19, size = 3)


library(e1071)

data.svm <- svm(klasa ~ x + y,
                data = dane,
                type = "C-classification",
                kernel = "linear",
                cost = 10,
                scale = FALSE)

# Wektory nośne
print(data.svm$SV)

# Wartości współczynników alfa
print(data.svm$coefs) 

# Wyznaczenie wektora w
w <- t(data.svm$SV) %*% data.svm$coefs
w

b <- data.svm$rho
b

xx <- 0:6

hiper <- data.frame (xx = xx,
                     r = -w[1]/w[2]*xx + b/w[2],
                     m1 = -w[1]/w[2]*xx + (b+1)/w[2],
                     m2 = -w[1]/w[2]*xx + (b-1)/w[2])

# Hiperpłaszczyzna rodzielająca
r <- geom_line(data = hiper, aes (x=xx, y=r), color = "red")

# Hiperpłaszczyzny marginesów
m1 <- geom_line(data = hiper, aes (x=xx, y=m1))
m2 <- geom_line(data = hiper, aes (x=xx, y=m2))

g + r + m1 + m2
