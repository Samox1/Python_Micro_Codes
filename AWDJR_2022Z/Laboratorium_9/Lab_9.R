# LABORATORIUM 8


# Obiekty typu time series
# Wykresy serii czasowych
# Dekompozycja serii czasowych
# Funkcja autokorelacji



### Obiekty typu time series ###

# Liczba urodzeń w Nowym Jorku od stycznia 1946 do grudnia 1959 roku
nybirths <- read.table ("nybirths.dat")
nybirths.ts <- ts (nybirths, frequency = 12, start = c(1946,1))
nybirths.ts

# Dane pobrane w 15-minutowych oknach z serwisu Twitter podczas Olimpiady w Londynie w 2012 roku.
twitter <- read.table ("cyber.dat", header = TRUE)
head (twitter)

twitter.comments.ts <- ts (twitter$comments, frequency = 96)
head (twitter.comments.ts)

# Wielowymiarowa seria czasowa
twitter.mts <- ts (twitter[,4:7], frequency = 96)
head (twitter.mts)



### Wykresy serii czasowych ###

plot.ts (nybirths.ts)

plot.ts (twitter.comments.ts)

plot.ts (twitter.mts)


library(ggplot2)
library(dplyr)
library(magrittr)
library(lubridate)

twitter %<>%
  mutate (date.time = as_datetime (paste (date, time),
                                   format = "%Y-%m-%d %H:%M:%S"))

g <- ggplot(twitter, aes (x = date.time))

g + theme_bw() +
  geom_line (aes(y = comments)) + 
  labs (x = "data", y = "liczba komentarzy w oknie")


g + theme_bw() +
  geom_line (aes(y = neg, color = "negatywne")) + 
  geom_line (aes(y = pos, color = "pozytywne")) +
  scale_color_manual (values = c("black","orange")) +
  labs (x = "data", y = "średnia zawartość emocji", color = "emocje")



### Dekompozycja serii czasowych ###

comments.comp <- decompose (twitter.comments.ts)
plot(comments.comp)



### Funkcja autokorelacji ###

x <- 1:10
acf(x)

acf (1:100)

acf (twitter$emo, lag.max = 100)

acf (twitter.comments.ts)

emo.acf <- acf (twitter$emo)

comments.acf <- acf (twitter$comments)

acf.df <- data.frame (lag = emo.acf$lag, comments = comments.acf$acf, emo = emo.acf$acf)
acf.df

ggplot (acf.df, aes (x = lag)) + 
  geom_point (aes (y = comments, color="comments"), size=3) + 
  geom_point (aes (y = emo, color="emo"), size=3) +
  labs (x = "przesunięcie", y = "autokorelacja", color = "seria") +
  theme_bw()

ccf (twitter$comments, twitter$emo)
