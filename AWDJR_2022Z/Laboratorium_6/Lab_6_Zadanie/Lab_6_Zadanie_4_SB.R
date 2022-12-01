### Laboratorium nr 6 - Zadanie 4
# Autor: Szymon Baczyński
# Data:  28.11.2022

rm (list = ls())



# 1) Wczytaj do pamięci plik data04.txt. Przy pomocy pakietu ggplot2 stwórz histogram z wczytanych danych, zbudowany z ok. 20 binów. 
#     Oceń jego kształt i zastanów się z jakiego rozkładu mogą pochodzić dane.

library(ggplot2)

dane <- read.table("data04.txt", col.names = c("x"))

g <- ggplot(dane)
g + geom_histogram(aes(x = x,..density..), fill="blue", colour="black", alpha=0.4, bins = 20)



# 2) Zweryfikuj hipotezę mówiącą, że dane pochodzą z rozkładu normalnego. 
#     Wykorzystaj testy Shapiro-Wilka i Wilcoxona. Skomentuj uzyskane wyniki.

print(shapiro.test(dane$x))
print(wilcox.test(dane$x))
print(wilcox.test(dane$x, mu=0.37))
print(wilcox.test(dane$x, mu=0.39))
print(wilcox.test(dane$x, mu=0.4))
print(wilcox.test(dane$x, mu=0.41))

# Komentarz: 
# Test Shapiro wykazał niską wartosc p-value w odniesieniu testu czy dane pochodzą z rozkładu normalnego.
# Test Wilcoxona przy odpowiedniej wartosci "mu" wykazal znaczna wartosc p-value co moze oznaczac, że dane są zbliżone do rozkładu normalnego.



# 3) Przy pomocy funkcji fitdistr() dopasuj dane do rozkładów: Cauchy’ego, wykładniczego, beta oraz gamma. 
#     Oceń, które dopasowanie jest najlepsze.

library(MASS)

# fitdistr -> Cauchy’ego
fit1 <- fitdistr(dane$x, "cauchy")
print(fit1$loglik)

# fitdistr -> wykładniczego
fit2 <- fitdistr(dane$x, "exponential")
print(fit2$loglik)

# fitdistr -> beta
fit3 <- fitdistr(dane$x, "beta", start = list(shape1 = 2, shape2 = 4))
print(fit3$loglik)   # <- To jest maksimum "loglik" ze wszystkich testow

# fitdistr -> gamma
fit4 <- fitdistr(dane$x, "gamma")
print(fit4$loglik)

# Komentarz: 
# Logarytm z funkcji wiarygodności jest największy w przypadku funkcji "beta" (256.7), wynikiem zbliżonym jest 
# wynik "loglik" z dopasowania funkcji gamma (221.3). Biorą pod uwagę parametr "loglik" to funkcja "beta" jest 
# najbardziej zbliżona do rozkładu sprawdzanych danych.



# 4) Wykonaj test Kołmogorova-Smirnova dla rozkładów otrzymanych w pkt. 3. Skomentuj uzyskane wyniki.

# ks.test -> Cauchy’ego
print(ks.test(dane$x, "pcauchy", location = fit1$estimate["location"], scale = fit1$estimate["scale"]))

# ks.test -> wykładniczego
print(ks.test(dane$x, "pexp", rate = fit2$estimate["rate"]))

# ks.test -> beta
print(ks.test(dane$x, "pbeta", shape1 = fit3$estimate["shape1"], shape2 = fit3$estimate["shape2"]))

# ks.test -> gamma
print(ks.test(dane$x, "pgamma", shape = fit4$estimate["shape"], rate = fit4$estimate["rate"]))

# Komentarz:
# Test Kołmogorova-Smirnova wykazał najlepsze dopasowanie w przypadku funkcji "beta" (p-value = 0.8418).
# W przypadku funkcji "gamma" p-value jest już znacząco mniejsze, a przy pozostałe funkcje nie są brane pod uwagę.



# 5) Dodaj do rysunku histogramu z pkt. 1. teoretyczną gęstość prawdopodobieństwa 
#     (przy pomocy funkcji stat_function() na podstawie wyników z pkt. 3 i 4.

g + geom_histogram(aes(x = x,..density..), fill="blue", colour="black", alpha=0.4, bins = 20) +
    stat_function(fun = dbeta,
                args = list(shape1 = fit3$estimate["shape1"], shape2 = fit3$estimate["shape2"]),
                colour="red",
                size=1.5)
