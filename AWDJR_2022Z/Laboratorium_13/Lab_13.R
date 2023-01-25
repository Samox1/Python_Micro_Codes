# LABORATORIUM 13


# Analiza skupień
# Optymalna liczba skupień
# Klastrowanie hierarchiczne
# Redukcja wymiaru




### Analiza skupień ###

library(MASS)
set.seed(111)

n <- 30
sigma <- matrix(c(1,0,0,1),2,2)

mu1 <- c(5,5)
mu2 <- c(1,1)
mu3 <- c(4,-2)

kolory <- c(rep("orange", n), rep("violet", n), rep("green", n))

clust1 <- mvrnorm(n, mu1, sigma)
clust2 <- mvrnorm(n, mu2, sigma)
clust3 <- mvrnorm(n, mu3, sigma)

all_points <- rbind (clust1, clust2, clust3)

xrange <- range (all_points[,1])
yrange <- range (all_points[,2])

par(mfrow = c(2,2))

plot(all_points, col=kolory, pch=19, cex=2, xlab="X", ylab="Y", xlim=xrange, ylim=yrange)
title("Klastry", cex.main=1.4, font=2)

cl <- kmeans(all_points, 3)

plot(all_points, col=kolory, pch=19, cex=2, xlab="X", ylab="Y", xlim=xrange, ylim=yrange)
text(all_points[,1], all_points[,2], cl$cluster, font=2)
title("Metoda 3-srednich", cex.main=1.4, font=2)

cl1 <- kmeans(all_points, 4)

plot(all_points, col=cl1$cluster, pch=19, cex=2, xlab="X", ylab="Y", xlim=xrange, ylim=yrange)
title("Metoda 4-srednich", cex.main=1.4, font=2)

cl2 <- kmeans(all_points, 5)

plot(all_points, col=cl2$cluster, pch=19, cex=2, xlab="X", ylab="Y", xlim=xrange, ylim=yrange)
title("Metoda 5-srednich", cex.main=1.4, font=2)




### Optymalna liczba skupień ###

random_points <- function(n, K, xrange, yrange) {
  x <- runif (n, xrange[1], xrange[2])
  y <- runif (n, yrange[1], yrange[2])
  cl <- kmeans (cbind (x,y), K)
  return (cl$tot.withinss)
}

K <- 1:10
W.rand <- sapply (K, function(k) mean (sapply (1:20, function(i) random_points (3*n, k, xrange, yrange))))
W.data <- sapply (K, function(k) mean (sapply (1:20, function(i) kmeans(all_points, k)$tot.withinss)))

W.df <- data.frame (K = K,
                    W.rand = W.rand,
                    W.data = W.data,
                    GS = log(W.rand/W.data))

library(ggplot2)
g <- ggplot(W.df)

g + theme_bw() + 
  geom_point ( aes (x=K, y=W.rand, color="losowe"), size=3) +
  geom_point ( aes (x=K, y=W.data, color="oryginalne"), size=3) +
  labs (x = "K", y="W", color="dane")


g + theme_bw() +
  geom_point ( aes (x=K, y=GS), size=3) +
  labs (x="K", y=expression(lnW[rnd]-lnW[data]))


library(caret)
cluster.predict <- as.factor (cl$cluster)
cluster.true <- as.factor (rep (c(1,3,2), each = n))
cluster.cm <- confusionMatrix (cluster.predict, cluster.true)
cluster.cm




### Klastrowanie hierarchiczne ###

sub_points <- rbind (clust1[1:10,], clust2[1:10,], clust3[1:10,])
hc <- hclust (dist (sub_points))
plot(hc, hang = -1)

hc$height
hc$merge

height.diff.max <- hc$height[which.max (diff (hc$height))+1]
plot (hc, hang = -1)
abline (h=height.diff.max, col="red", lwd=2)




### Redukcja wymiaru ###

# Przykład 12.6
x <- seq(-5, 5, by=.1)
y <- x

eta <- runif(101, max = 1)
dzeta <- runif(101, max = 1)

x <- x + eta
y <- y + dzeta

par(mfrow = c(2,2))

plot(x, y, pch=19, xlab="Test 1", ylab="Test 3", font=2, font.lab=2, xlim=c(-5,5), ylim=c(-5,5))
abline(h=0, v=0, lwd=2, col="gray")
abline(0,1,lwd=2,col="red")
abline(0,-1,lwd=2,col="green")
text(4.5,-0.5,expression(x[1]),cex=2)
text(-0.5,4.5,expression(x[2]),cex=2)
text(4.7,4,expression(y[1]),cex=2, col="red")
text(-4.5,4,expression(y[2]),cex=2, col="green")
title("Dane", cex.main=1.4)

test <- data.frame(x, y)

test.pc <- princomp(~., cor=T, data=test)

plot(test.pc, main="")
title("Wariancja", cex.main=1.4)

plot(test.pc$scores, xlim=c(-2,2), ylim=c(-2,2), xlab="Składowa 1", ylab="Składowa 2")
title("Składowe", cex.main=1.4)






