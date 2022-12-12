### Laboratorium nr 7 - Zadanie 5
# Autor: Szymon Baczyński
# Data:  05.12.2022

rm(list = ls())


library(ggplot2)



# 1) Wczytaj dane z pliku orings.txt. Kolumna temp zawiera temperaturę w stopniach Fahrenheita, natomiast binarna zmienna fail informuje o tym czy nastąpiła awaria o-ringu. 
#    Przelicz temperaturę na stopnie Celcjusza i wykonaj wykres punktowy ilustrujący dane.

orings_dane <- read.table("orings.txt", header = TRUE)
orings_C <- orings_dane
orings_C["temp"] <- (orings_C["temp"] - 32.0) / 1.8


g <- ggplot(orings_C)
g1 <- g + geom_point(aes(x = temp, y = fail)) + xlab("temperatura [st. C]") + ylab("wystapienie awarii")
plot(g1)



# 2) Wykonaj regresję logistyczną dla zmiennej wynikowej fail w funkcji zmiennej objaśniającej tempC. 
#     Odpowiedz na pytanie czy otrzymany model jest statystycznie uzasadniony na poziomie ufności α=0.05?

orings_model <- glm(formula = fail ~ temp, data = orings_C,  family = binomial)
print(summary(orings_model))

# Komentarz:
# Tak, jest statystycznie uzasadniony



# 3) Ilukrotnie zmieni się szansa awarii jeśli temperatura powietrza spadnie z 15 na 10 stopni Celcjusza?

szansa_awarii_z15na10 = 1/exp(orings_model[["coefficients"]][2] * (15-10))
print("Punkt 3:")
print(szansa_awarii_z15na10)



# 4) Przy pomocy funkcji predict() wyznacz prawdopobieństwa awarii w funkcji temperatury dla przedziału od 1 do 30 stopni Celcjusza z różnicą 1 stopnia. 
#     O ile zmieni się prawdopodobieństwo awarii jeśli temperatura powietrza spadnie z 15 na 10 stopni Celcjusza?

orings_new <- data.frame(temp = seq(1,30,1))
orings_new["fail"] <- predict(orings_model, newdata = orings_new, type = "response")
print("Punkt 4:")
print(orings_new[orings_new["temp"] == 10 ,"fail"] - orings_new[orings_new["temp"] == 15 ,"fail"])



# 5) Zobrazuj dane z pkt. 4 w formie czerwonej linii dodanej do rysunku z pkt. 1.

g2 <- g1 + geom_line(data = orings_new, aes(x=temp, y=fail), color="red")
plot(g2)

