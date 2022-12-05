### Laboratorium nr 7 - Zadanie 5
# Autor: Szymon Baczyński
# Data:  05.12.2022

rm (list = ls())

library(dplyr)
library(ggplot2)
library(fields)

# 1) Wczytaj do pamięci zbiór danych ChickWeight i zapoznaj się z nim, a następnie stwórz ramkę danych zawierającą wyłacznie te rekordy, 
#     dla których wartość zmiennej Diet wynosi 1 lub 2, natomiast zmiennej Time mniej niż 21 dni (użyj funkcji filter z pakietu dplyr).

kurczaki <- data.frame(datasets::ChickWeight)
summary(kurczaki)

kurczaki_filter <- kurczaki %>% filter((Diet == 1 | Diet == 2) & Time < 21)
summary(kurczaki_filter)


# 2) Wykorzystując stworzoną pkt. 1 ramkę danych, wykonaj w pakiecie ggplot wykres wagi kur od długości ich życia. Użyj przezroczystości 
#     aby nachodzące na siebie punkty były lepiej widoczne. Podpisz osie jak na przykładzie poniżej.

g <- ggplot(kurczaki_filter)
g1 <- g + geom_point(aes(x = Time, y = weight, colour = Diet, fill = Diet), size=2, shape=21, alpha=0.25) + xlab("dni od narodzin") + ylab("waga [g]") + theme_bw()
plot(g1)

# 3) Wykonaj dwa binowania danych (osobno dla każdej serii) i stwórz nową ramkę danych zawierającą środki zbinowanych punktów, 
#     średnią wartość oraz niepewność standardową średniej). Przy wykonywaniu binowania nadpisz domyślne granice binów.

breaks_bin <- seq(-1,21,2)

kurczaki_Diet1 <- kurczaki_filter %>% filter(Diet == 1)
kurczaki_filter.bin1 <- stats.bin(unlist(kurczaki_Diet1["Time"]), unlist(kurczaki_Diet1["weight"]), breaks = breaks_bin)
kurczaki_bin1 <- data.frame(x=kurczaki_filter.bin1$centers, y=kurczaki_filter.bin1$stats[2,], std.err = kurczaki_filter.bin1$stats[3,]/sqrt(kurczaki_filter.bin1$stats[1,]), Diet=1)

kurczaki_Diet2 <- kurczaki_filter %>% filter(Diet == 2)
kurczaki_filter.bin2 <- stats.bin(unlist(kurczaki_Diet2["Time"]), unlist(kurczaki_Diet2["weight"]), breaks = breaks_bin)
kurczaki_bin2 <- data.frame(x=kurczaki_filter.bin2$centers, y=kurczaki_filter.bin2$stats[2,], std.err = kurczaki_filter.bin2$stats[3,]/sqrt(kurczaki_filter.bin2$stats[1,]), Diet=2)


# 4) Dodaj punkty otrzymane z binowania w pkt. 3 do wykresu z pkt. 2. Wykorzystaj do tego geometrię geom_errorbar() lub geom_pointrange()

kurczaki_bin <- data.frame(rbind(kurczaki_bin1, kurczaki_bin2))
kurczaki_bin$Diet <- as.factor(kurczaki_bin$Diet)
p1 <- geom_pointrange(data=kurczaki_bin, aes(x = x, y = y, ymax = y+std.err, ymin = y-std.err, fatten=7, colour = kurczaki_bin$Diet)) 
g2 <- g1 + p1
plot(g2)



# 5) Przy pomocy funkcji geom_smooth() dodaj do wykresu regresję liniową wykonaną dla zbinowanych punktów

p3 <- geom_smooth(data=kurczaki_bin, aes(x = x, y = y, colour = kurczaki_bin$Diet), method = "lm", linewidth=1.1)
g3 <- g2 + p3
plot(g3)

