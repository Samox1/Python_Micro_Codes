# LABORATORIUM 6


# Hipotezy statystyczne -> TEKST
# Testowanie hipotez -> TEKST
# Błędy w testowaniu hipotez -> TEKST
# Statystyka testowa, zbiór krytyczny i p-wartość -> TEKST
# Test χ2
# Test Kołmogorowa-Smirnowa
# Testy normalności rozkładu
# Dopasowywanie parametrów rozkładów



### Test χ2 ###

x <- c(100,50,20,10,5)
plot(x)
p1 <- c(0.5, 0.2, 0.2, 0.05, 0.05)
plot(p1)
chisq.test (x, p = p1)

p2 <- c(0.5, 0.3, 0.1, 0.05, 0.05)
chisq.test( x, p = p2)

p <- x/sum(x)
plot(p)
chisq.test (x, p = p)

plot (p)
lines (p1, col="red")
lines (p2, col="blue")

h <- hist (rpois(200, lam = 3), breaks = (0:15)-0.5)
chisq.test (h$counts, p = dpois(h$mids, lam = 3))
chisq.test (h$counts, p = dpois(h$mids, lam = 3), rescale.p = TRUE)

plot (h$mids, h$density)
lines (h$mids, dpois(h$mids, lam = 3), col="red", lwd=2)



### Test Kołmogorowa-Smirnowa ###

x <- rnorm (100)
y <- rnorm (100)
ks.test (x, y)

y <- rnorm (100, 1, 2)
ks.test (x, y)

ks.test (x, "pnorm", 0, 1)

x <- rpois (100, lam = 5)
y <- rpois (100, lam = 5)
ks.test (x, y)

y <- rpois (100, lam = 2)
ks.test (x, y)

# Wynik poniższego testu będzie niepoprawny, ponieważ rozkład Poissona jest rozkładem dyskretnym
ks.test (x, "ppois", lam = 5)



### Testy normalności rozkładu ###

shapiro.test (rnorm(100))
shapiro.test (rnorm(100, 0, 5))
shapiro.test (runif(100, 0, 1))
wilcox.test (rnorm(100))
wilcox.test (rnorm(100, 1, 2), mu = 1)
wilcox.test (rnorm(100, 1, 2), mu = 2)



### Dopasowywanie parametrów rozkładów ###

library(MASS)

x <- rnorm (200, mean = 5, sd = 1.5)
hist(x)
fit1 <- fitdistr (x, "normal")
fit1

y <- rexp (200, rate = 0.5)
hist(y)
fit2 <- fitdistr (y, "exponential")
fit2

z <- rweibull (200, scale = 1.5, shape = 3)
hist(z)
fit3 <- fitdistr (z, "weibull", list (scale = 1, shape = 1))
fit3

fit1$estimate
fit1$sd
fit1$loglik

# Testowanie rozkładu normalnego
wilcox.test (x, mu = fit1$estimate["mean"])

# Testowanie rozkładu wykładniczego
ks.test (y, "pexp", rate = fit2$estimate["rate"])

# Testowanie rozkładu Weibulla
ks.test (z, "pweibull", scale = fit3$estimate["scale"], shape = fit3$estimate["shape"])


# Rysowanie
library(ggplot2)
g <- ggplot (data.frame(x,y,z))

g + geom_histogram(aes(x = x,..density..), fill="blue", colour="black", alpha=0.4) +
  stat_function(fun = dnorm,
                args = list(mean = fit1$estimate["mean"],
                            sd = fit1$estimate["sd"]),
                colour="red",
                size=1.5)

g + geom_histogram(aes(x = y,..density..), fill="blue", colour="black", alpha=0.4) + 
  stat_function(fun = dexp,
                args = list(rate = fit2$estimate["rate"]),
                colour="red",
                size=1.5)

g + geom_histogram(aes(x = z,..density..), fill="blue", colour="black", alpha=0.4) + 
  stat_function(fun = dweibull,
                args = list(scale = fit3$estimate["scale"], shape = fit3$estimate["shape"]),
                colour="red",
                size=1.5)
