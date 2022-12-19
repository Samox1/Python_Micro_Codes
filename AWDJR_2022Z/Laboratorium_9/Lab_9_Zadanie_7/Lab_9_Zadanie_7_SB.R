### Laboratorium nr 9 - Zadanie 7
# Autor: Szymon Baczyński
# Data:  19.12.2022

rm(list = ls())

library(ggplot2)

# 1) Wczytaj zbiór danych z pliku twitter.dat. Oblicz funkcje autokorelacji dla przetasowanych danych z kolumn comments 
#   i emo z przesunięciem maksymalnym równym 50 (użyj funkcji sample() do wykonania permutacji).

cyber_dane <- read.table("cyber.dat", header = TRUE)
cyber_dane_sample <- sample(cyber_dane[sample(1:nrow(cyber_dane)),])

cyber.acf <- list()
cyber.acf$comments <- acf(cyber_dane_sample$comments, lag.max = 50)
cyber.acf$emo <- acf(cyber_dane_sample$emo, lag.max = 50)
cyber.df <- data.frame(lag=cyber.acf$emo$lag, comments=cyber.acf$comments$acf, emo=cyber.acf$emo$acf)



# 2) Wykonaj jeden wykres, który będzie pokazywał obie funkcje autokorelacji.

g1 <- ggplot (cyber.df, aes (x = lag)) + 
  geom_point (aes (y = comments, color="comments"), size=3) + 
  geom_point (aes (y = emo, color="emo"), size=3) +
  labs (x = "lag", y = "acf", color = "seria") +
  theme_bw()

plot(g1)



# 3) Wczytaj do pamięci dane z pliku wig20.dat. Wykorzystaj drugą kolumnę ramki (wartość indeksu WIG20) do 
#   stworzenia obiektu typu ts. Dobierz odpowiednią częstotliwość danych.

wig20_dane <-read.table("wig20.dat", header = FALSE)[0:(484),]
wig20_ts <- ts((wig20_dane$V2), frequency = 4)



# 4) Wykonaj dekompozycję serii czasowej stworzonej w pkt. 3 i oblicz funkcje autokorelacji 
#   (z przesunięciem maksymalnym równym 40) dla tej serii czasowej oraz dla samej składowej losowej sygnału.

wig20_comp <- decompose(wig20_ts)
# plot(wig20_comp)
wig20_acf <- list()
wig20_acf_x <- acf(wig20_comp$x, lag.max = 40, na.action=na.pass)
wig20_acf_random <- acf(wig20_comp$random, lag.max = 40, na.action=na.pass)
wig20_acf <- data.frame(lag=seq(0,40,1), Original=wig20_acf_x$acf, Random = wig20_acf_random$acf)

g2 <- ggplot (wig20_acf, aes (x = lag)) + 
  geom_point (aes (y = Original, color="Original"), size=3) + 
  geom_point (aes (y = Random, color="Random"), size=3) +
  labs (x = "lag", y = "acf", color = "series") +
  theme_bw()

plot(g2)