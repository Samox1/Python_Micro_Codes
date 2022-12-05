# LABORATORIUM 7


# Korelacje Pearsona i Spearmana
# Test χ2 Pearsona dla macierzy kontyngencji
# Regresja liniowa
# Wielokrotna regresja liniowa
# Binowanie



### Korelacje Pearsona i Spearmana ###

lb2kg <- 0.4536
gallons2liter <- 3.78541178
miles2km <- 1.609344

masa <- 1000*lb2kg*mtcars$wt # masa pojazdu (w kg)
spalanie <- 100*gallons2liter/(mtcars$mpg*miles2km) # spalanie (w litrach na 100 km)
plot (masa, spalanie)

cor (masa, spalanie)
cor (masa, spalanie, method="spearman")

masa
rank(masa)

spalanie
rank(spalanie)

cor (rank (masa), rank (spalanie))

df <- data.frame (masa = masa, spalanie = spalanie, czas = mtcars$qsec)
cor (df)

library(corrplot)
corrplot (cor(df))

cor.test (masa, spalanie)

x <- seq(0,6,0.5)
y <- x^2-6*x+8
plot(x, y)

cor.test (x,y)



### Test χ2 Pearsona dla macierzy kontyngencji ###

biegi <- factor (mtcars$am, labels = c("automatic","manual"))
cylindry <- factor (mtcars$cyl)
tab <- table (biegi, cylindry)
tab

chisq.test(tab)
tab.exp <- chisq.test(tab)$expected
tab.exp
tab.exp / sum(tab.exp)

tab.biegi <- apply(tab, 1, sum) / sum(tab)
tab.cylindry <- apply(tab, 2, sum) / sum(tab)
tab.biegi %o% tab.cylindry
tab / sum(tab)



### Regresja liniowa ###

x <- 1:20
y <- 2*x-2 + runif(length(x), -3, 3)
plot(x, y)

xy.lm <- lm(y ~ x)
summary(xy.lm)

cor(x,y)^2
xy.lm$coefficients

b <- xy.lm$coefficients[1]
a <- xy.lm$coefficients[2]
plot(x, y, pch=19)
lines(x, a*x+b, col="red", lwd=3)

x <- c(1,2,5,10,20,50,100,200,500,1000)
y <- (x*(1+runif(length(x),-0.9,0.9)))^(-2)
plot(x, y, log="xy")

xy.lm <- lm(log(y) ~ log(x))
summary(xy.lm)

b <- exp(xy.lm$coefficients[1])
a <- xy.lm$coefficients[2]
plot(x, y, pch=19, log="xy")
lines(x, b*x^a, col="red", lwd=3)

# Jeszcze raz przykład ze zbioru mtcars
fit <- lm (spalanie ~ masa, data = df)
new.data <- data.frame (masa = seq(1e3,3e3,200))
new.data$spalanie <- predict (fit, new.data)

library(ggplot2)
ggplot(df, aes(x=masa, y=spalanie)) + geom_point () + geom_point (data=new.data, color="red")



### Wielokrotna regresja liniowa ###

df2 <- data.frame (spalanie=spalanie,
                   masa = masa,
                   cylindry = mtcars$cyl,
                   moc = mtcars$hp)

fit2 <- lm (spalanie ~ masa+cylindry+moc, data = df2)

summary (fit2)

summary (fit)


fit3 <- lm (spalanie ~ moc, data = df2)
summary (fit3)

n  <- 100
x1 <- rnorm (n, 175, 7)
x2 <- rnorm (n, 30, 8)
x3 <- abs (rnorm (n, 60, 30))

y <- x1 + 2*x2 - 3*x3 + 4*x1*x2 - 5*x1*x3 + 6*x2*x3 + 10 + rnorm(n, 0, 0.5)

df3 <- data.frame (x1, x2, x3, y)
y.lm <- lm (y ~ x1*x2*x3, data=df3)
summary(y.lm)


### Binowanie ###

x <- 1:100
y <- x + runif(length(x), -20, 20)


library(fields)
xy.sb <- stats.bin(x, y)
xy.sb

plot (x, y)
points (xy.sb$centers, xy.sb$stats[2,], col="red", pch=19, cex=1.5)


library(dplyr)
library(magrittr)

df4 <- data.frame (x = x, y = y)
df5 <- data.frame (x = xy.sb$centers,
                   y = xy.sb$stats[2,],
                   sd = xy.sb$stats[3,],
                   n = xy.sb$stats[1,])

df5 %<>%
  mutate (std.err = 2*sd/sqrt(n))

xy.lm <- lm(y ~ x, data = df5)

my.lm <- function(x, model) {
  model$coefficients[2]*x+model$coefficients[1]
}

library(ggplot2)

ggplot(df4, aes (x = x, y = y)) +
  geom_point (color = "gray") +
  geom_pointrange(data=df5, aes (x = x, y = y, ymax = y+std.err, ymin = y-std.err), color="red") +
  stat_function(fun = my.lm, args = list (model = xy.lm), color="red")
