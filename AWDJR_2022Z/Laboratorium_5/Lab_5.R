# LABORATORIUM 5


# Podstawy ggplot2
# Modyfikowanie właściwości
# Grupowanie
# Przezroczystość
# Histogramy
# Warunkowanie
# Zapisywanie wykresów do pliku



### Podstawy ggplot2 ###

library(ggplot2)

df1 <- data.frame(a = 1:10, b = (1:10) + runif(10,-2,2), c = 1:10)
df1

ggplot(df1, aes(x = a, y = b))

ggplot(df1, aes(x = a, y = b)) + geom_point()

ggplot(df1, aes(x = a, y = b)) + geom_point()
ggplot(df1) + geom_point(aes(x = a, y = b))
ggplot() + geom_point(data = df1, aes(x = a, y = b))
ggplot(df1, aes(x = a)) + geom_point(aes(y = b))

g <- ggplot(df1, aes(x = a, y = b))
g + geom_point()

df2 <- data.frame(a = 1:10, b = (1:10)+runif(10,-2,2), c = 1:10)
g <- ggplot(df1)
g + geom_point(aes(x = a, y = b))

g %+% df2 + geom_point(aes(x = a, y = b))



### Modyfikowanie właściwości ###

g <- ggplot(df1, aes(x = a))
g + geom_point(aes(y=b), size=4, shape=22, colour="red") + 
  geom_point(aes(y = c), size=3, shape=21, fill="blue")

g + geom_point(aes(y=b), size=4, shape=22, colour="red", fill="blue") + 
  geom_line(aes(y = c), size=1.2, linetype=3, colour="darkgreen")

g <- ggplot(df1, aes(x = a))
g + geom_smooth(aes(y = b), method="lm", size=1.2, colour="red") + 
  geom_point(aes(y = b), shape=21, size=3, fill="black")

p <- ggplot(df1, aes(x = a)) + geom_smooth(aes(y = b), method="lm", size=1.2, colour="red") + 
  geom_point(aes(y = b), shape=21, size=3, fill="black")
build <- ggplot_build(p)
build



### Grupowanie ###

df2 <- data.frame(a = c(df1$a, df1$a), b = c(df1$b, df1$c), n = sample(20), ser = c(rep("S1",10), rep("S2", 10)))
df2

g <- ggplot(df2)
g + geom_point(aes(x = a, y = b, fill=ser), size=3, shape=21)

g + geom_point(aes(x = a, y = b, shape=ser), size=3, colour="blue")

g + geom_point(aes(x = a, y = b, fill=ser, size=n), shape=21)

# Tworzenie danych
x1 <- 1:10; x2 <- 5:15; x3 <- 10:20
y1 <- x1 + runif(10,-2,2)
y2 <- x2 + runif(11,-3,3)
y3 <- x3 + runif(11,-2,2)
df3 <- data.frame(x = c(x1, x2, x3), y = c(y1, y2, y3), ser = c(rep("S1", 10), rep("S2", 11), rep("S3", 11)))
df3

g <- ggplot(df3)

p <- geom_point(aes(x = x, y = y, fill = ser), size=3, shape=21)
g + p

# Dopasowanie do wszystkich punktów
g + p + geom_smooth(aes(x = x, y = y), method = "lm")

# Dopasowanie do poszczegolnych serii
g + p + geom_smooth(aes(x = x, y = y, group = ser, colour=ser), method = "lm")



### Przezroczystość ###

x <- rnorm(10000, 0, 1); y <- rnorm(10000, 0, 1)
df.norm <- data.frame(x = x, y = y)
g <- ggplot(df.norm)

# Bez przezroczystosci
g + geom_point(aes(x = x, y = y), shape = 21)

# Z przezroczystoscia
g + geom_point(aes(x = x, y = y), shape = 21, alpha = 0.25)


# Bez przezroczystosci
df.norm <- data.frame(x = rnorm(10000,0,1), y=rnorm(10000,0,1))
df.norm1 <- data.frame(x = rnorm(10000,2,1), y=rnorm(10000,-2,1))
df.norm2 <- data.frame(x = rnorm(10000,-2,1), y=rnorm(10000,-2,1))
g <- ggplot(df.norm)

g + geom_point(aes(x = x, y = y), shape=21, fill="blue", colour="blue") + 
  geom_point(data = df.norm1, aes(x = x,y = y), shape=21, fill = "darkgreen", colour = "darkgreen") + 
  geom_point(data = df.norm2, aes(x = x,y = y), shape=21, fill = "red", colour = "red")

# Z przezroczystoscia
g + geom_point(aes(x = x, y = y), shape=21, fill="blue", colour="blue", alpha=0.1) + 
  geom_point(data=df.norm1, aes(x=x,y=y), shape=21, fill="darkgreen", colour="darkgreen", alpha=0.1) + 
  geom_point(data=df.norm2, aes(x=x,y=y),shape=21,fill="red",colour="red", alpha=0.1)



### Histogramy ###

# Generacja danych
N1 <- 10000
N2 <- 5000
N3 <- 20000

sigma1 <- 1
sigma2 <- 0.5

mu <- 0

x1 <- rnorm(N1, mu, sigma1)
x2 <- rnorm(N2, mu, sigma1)
x3 <- rnorm(N3, mu, sigma1)
x4 <- rnorm(N1, mu, sigma2)
x5 <- rnorm(N2, mu, sigma2)
x6 <- rnorm(N3, mu, sigma2)

label.size <- c(rep("Size 1", N1), rep("Size 2", N2), rep("Size 3", N3))
label.par <- c(rep("Par 1", N1 + N2 + N3), rep("Par 2", N1 + N2 + N3))

df <- data.frame(x = c(x1, x2, x3, x4, x5, x6), size = c(label.size, label.size), par = label.par)

g1 <- ggplot(data = df[df$size == "Size 1",])
g1 + geom_histogram(aes(x = x, fill = par, colour = par))


g1 + geom_histogram(aes(x = x, fill = par, colour = par), position="identity", alpha=0.3)

g1 + geom_density (aes(x = x, fill = par), colour="black", alpha=0.3)

g1 + geom_histogram (aes(x = x,..density.., fill = par, colour = par), position="identity", alpha=0.3) +
  stat_function (fun = dnorm, args = list(mean = 0, sd = 0.5))



### Warunkowanie ###

g2 <- ggplot(data = df)
g2 + geom_histogram(aes(x = x,..density.., fill = size, colour = size), alpha=0.6) + facet_grid(size ~ par)



### Zapisywanie wykresów do pliku ###

ggsave ("histogramy.pdf", device = "pdf", width = 15, height = 10, units = "cm")
