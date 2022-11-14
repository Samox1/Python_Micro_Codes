# LABORATORIUM 4


# Tworzenie wykresów
# Tworzenie histogramów
# Rozkłady prawdopodobieństwa
# Funkcja sample
# Dystrybuanta empiryczna



### Tworzenie wykresów ###

x <- 1:10
plot(x, x^2)

plot (x,
      x^2,
      xlab = "x",
      ylab = expression(f(xi)==x^2),
      main = "Wykres funkcji f(x)",
      col = "red",
      pch = 19,
      font = 2,
      font.lab = 4,
      font.main = 3,
      cex = 2)

y <- 2*x+2
z <- 3*x-1
plot (x, y, type = "l", lwd = 2, col = "blue")
lines (x, z, lwd = 2, col = "red")

png ("fig2.png")
plot (x, y, type = "l", lwd = 2, col = "blue")
lines (x, z, lwd = 2, col = "red")
dev.off()

x <- 1:10
plot (x,
      x^2,
      xlab = "x",
      ylab = expression(f(x)),
      col = "red",
      bg = "red",
      pch = 21,
      cex = 2)
points (x,
        x^1.5,
        col = "blue",
        bg = "blue",
        pch = 22,
        cex = 2)
legend ("topleft",
        legend = c (expression(x^1.5),expression(x^2)),
        pch = c (22,19),
        col = c ("blue","red"),
        pt.bg = c("blue","red"))



### Tworzenie histogramów ###

y <- c(0,0,1,2,3,1,2,3,4)
tabulate(y)

y <- c(0,0,1.1,1.9,2.1,2,3)
tabulate(y)

df <- data.frame(x=c(1,1,2,2,3,4,5), y=c(2,2,3,1,5,5,5))
df

table(df)

x <- c(1,1,1,2,2,4,10)
hist(x)

x <- c (1,1,1,2,2,4,10)
h <- hist (x)

h

x <- rep (1:5, c(20, 9, 4, 0, 1))
h <- hist (x)

h <- hist (x, breaks = 0.5:5.5)



### Rozkłady prawdopodobieństwa ###

runif (5)
runif (5, -10, 10)

x <- seq (-2, 2, 0.1)
sigmas <- c (0.5, 1, 0.3)

plot (x, dnorm(x, 0, sigmas[1]), ylim = c(0,1.5), pch = 19)
points (x, dnorm(x, 0, sigmas[2]), pch = 19, col = "blue")
points (x, dnorm(x, 0, sigmas[3]), pch = 19, col = "green", t = "o")
lines (x, pnorm(x, 0, sigmas[3]), col = "red", lwd = 2)
legend ("topleft",
        legend = c (expression (sigma==0.5),
                    expression (sigma==1.0),
                    expression (sigma==0.3),
                    expression (sigma==0.3)),
        lty = c (0, 0, 1, 1),
        pch = c (19, 19, 19, NA),
        col = c ("black","blue", "green", "red")
)

qnorm (0.95, 0, 1)
qnorm (0.5, 3, 0.5)



### Funkcja sample ###

sample(1:3, 2)
sample(1:10, 2)
sample(1:10)
sample(1:10, 4)
sample(1:10, 20, replace=TRUE)
sample(1:3, 10, replace=TRUE, prob=c(0.1,0.8,0.1))
sample(letters[1:3], 10, replace=TRUE)
sample(c(0.1, 0.2, 0.3), 10, replace=TRUE)

# Random walker
rw <- sample(c(-1,1),100,replace=TRUE)
rw_cum <- cumsum(rw)
plot(rw_cum, type = "l")



### Dystrybuanta empiryczna ###

x <- c(1,1,1,2,5,6,3,7,8,10)
plot(ecdf(x))

get.x <- function(x) {
  return(seq(min(x), max(x), length.out = 100))
}

make.plots <- function(N) {
  x <- rnorm(N, 0, 1)
  plot(ecdf(x), main = N)
  xx <- get.x(x)
  lines(xx, pnorm(xx, 0, 1), col = "red", lwd = 2)  
}

par(mfrow = c(2,2))

N <- c(10, 50, 100, 500)

sapply(N, make.plots)
