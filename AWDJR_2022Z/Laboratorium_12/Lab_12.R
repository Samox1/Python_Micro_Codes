# LABORATORIUM 11


# Drzewa klasyfikujące - wprowadzenie
# Reguły podziału
# Miary różnorodności
# Optymalizacja drzewa
# Pakiet rpart



### Drzewa klasyfikujące - wprowadzenie ###

library(MASS)
library(ggplot2)
theme_set (theme_bw())
set.seed(123)

n <- 30
m1 <- c (2, 2)
S1 <- matrix (c (1,0,0,1), 2, 2)
x1 <- mvrnorm (n, m1, S1)

r <- 6
x2 <- seq (0, r, length.out = n)
y2 <- sqrt(r^2-x2^2)+runif(n,-0.5,0.5)

dane <- data.frame (x = c (x1[,1], x2),
                    y = c (x1[,2], y2),
                    klasa = as.factor (rep (c ("1","2"), each = n)))

ggplot (dane, aes (x=x, y=y)) +
  geom_point (aes (color=klasa), size = 2) +
  geom_vline (xintercept = 4) +
  geom_hline (yintercept = 4)



### Reguły podziału ###



### Miary różnorodności ###



### Optymalizacja drzewa ###



### Pakiet rpart ###

library (rpart)

# Uczenie drzewa
tree <- rpart(klasa ~ x + y, dane, minsplit = 2, minbucket = 1)
tree

summary (tree)

# Rysowanie drzewa
plot (tree)
text (tree, use.n = TRUE, all = TRUE, font = 2) 

library(rpart.plot)
rpart.plot (tree, type = 1, extra = 1) 

library(MASS)
library(caret)

pima.tree <- rpart (type ~., Pima.tr)
rpart.plot (pima.tree, type = 1, extra = 1)


pima.predict <- predict (pima.tree, newdata = Pima.te, type = "class")
pima.true <- Pima.te[,8]

pima.cm <- confusionMatrix(pima.predict, pima.true, positive = "Yes")
pima.cm

iris.tree <- rpart (Species ~., iris)
rpart.plot (iris.tree, type = 1, extra = 1)
