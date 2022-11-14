### Laboratorium nr 4 - Zadanie 2
# Autor: Szymon Baczyński
# Data:  14.11.2022

rm (list = ls())

# 1) Wylosuj n=10^3 liczb z rozkładu normalnego o średniej m=2 i odchyleniu standardowym s=0.5 
#   (n, m, s powinny być zmiennymi których wartość będzie można zmieniać w skrypcie).

n = 10^3
m = 2
s = 0.5

data <- rnorm(n, mean = m, sd = s)


# 2) Następnie wykonaj histogram z tych danych i zapisz go do zmiennej.
hist_norm <- hist(data, 15)


# 3) Wykonaj wykres punktowy empirycznego rozkładu gęstości prawdopodobieństwa (tę gęstość można wyciągnąć z obiektu histogram.

plot(hist_norm$mids, hist_norm$density, pch = 19,
     xlab = "x",
     ylab = expression(f(x)),
     ylim = c(0,1.0))


# 4) Dodaj drugą serię do wykresu (czerwona linia), która będzie przedstawiać 
#   teoretyczną gęstość prawdopodobieństwa dla zadanej średniej m i odchylenia s.

x_x <- seq(min(hist_norm$breaks), max(hist_norm$breaks),0.01)
hist_dnorm <- dnorm(x_x, mean=m, sd=s)
lines(x_x, hist_dnorm, col='red')


# 5) Podpisz osie i dodaj legendę tak jak na rysunku poniżej. Zadbaj o to aby czerwona linia nie wychodziła poza wykres.

legend ("topleft",
        legend = c('data','fit'),
        lty = c(0,1),
        pch = c(19, NA),
        col = c("black", "red"))


# 6) Zapisz wykres do pliku w formacie png.

png("Fig_1.png")

plot(hist_norm$mids, hist_norm$density, pch = 19, xlab = "x", ylab = expression(f(x)),ylim = c(0,1.0))
lines(x_x, hist_dnorm, col='red')
legend ("topleft", legend = c('data','fit'), lty = c(0,1), pch = c(19, NA),col = c("black", "red"))

dev.off()


