### KOLOKWIUM - Zadanie 3
# Autor: Szymon Baczyński
# Data:  31.01.2022


rm(list = ls())


## Do zadań należy wykorzystać zbiór danych Sacramento z biblioteki caret. 
## Zawiera on 932 obserwacji 9 zmiennych opisujących różne parametry domów na sprzedaż w Kaliforni. 
## Przed przystąpienie do zadań należy zapoznać się ze zbiorem w zakładce Help.


library(caret)
library(tidyverse)
library(ggplot2)


# 1) Sprawdź czy możliwe jest wykluczenie którejś ze zmiennych kategorycznych ze względu na dominację w niej jednego z poziomów (powyżej 90%). 
#   Jeśli tak to pozostaw tylko rekordy z dominującą wartością zmiennej, a następnie usuń tę kolumnę ze zbioru. 
#   Wykorzystaj do tego zadania funkcję summary() oraz operator potoku.

data("Sacramento")
summary(Sacramento)     # 3 zmienne kategoryczne - city, zip, type
Sacramento %>% select(where(is.factor)) %>% summary       # zmienna "type" ma duzo jednej wartosci
Sacramento %>% group_by(city) %>% summarise(n=n()/nrow(.)) %>% arrange(desc(n))
Sacramento %>% group_by(zip) %>% summarise(n=n()/nrow(.)) %>% arrange(desc(n))
Sacramento %>% group_by(type) %>% summarise(n=n()/nrow(.)) %>% arrange(desc(n))

dane <- Sacramento %>% .[(.["type"] == "Residential"), ] %>% select(-c(type))
summary(dane)



# 2) Wykonaj kilka wariantów regresji liniowej traktując kolumnę price jako zmienną wynikową:
#   - wariant 1: wszystkie pozostałe zmienne są predyktorami,
#   - wariant 2: bez zmiennej zip,
#   - wariant 3: bez zmiennej city,
#   - wariant 4: bez zmiennych zip oraz city.
#   Przeanalizuj uzyskane warianty regresji liniowej i na tej podstawie wybierz zmienne do modelu predykcyjnego. Uzasadnij swój wybór w komentarzu.

# city + zip + beds + baths + sqft + latitude + longitude

model.lm_1 <- lm(price ~ . , data = dane)
model.lm_2 <- lm(price ~ city + beds + baths + sqft + latitude + longitude, data = dane)
model.lm_3 <- lm(price ~ zip + beds + baths + sqft + latitude + longitude, data = dane)
model.lm_4 <- lm(price ~ beds + baths + sqft + latitude + longitude, data = dane)

print(paste0("Wariant 1: R^2 = ", summary(model.lm_1)$r.squared, "  ||  R^2 adj. = ", summary(model.lm_1)$adj.r.squared))
print(paste0("Wariant 2: R^2 = ", summary(model.lm_2)$r.squared, "  ||  R^2 adj. = ", summary(model.lm_2)$adj.r.squared))
print(paste0("Wariant 3: R^2 = ", summary(model.lm_3)$r.squared, "  ||  R^2 adj. = ", summary(model.lm_3)$adj.r.squared))
print(paste0("Wariant 4: R^2 = ", summary(model.lm_4)$r.squared, "  ||  R^2 adj. = ", summary(model.lm_4)$adj.r.squared))



# 3) Zbuduj model regresyjny oparty na zmiennych wybranych w pkt. 2 i wykonaj przy jego pomocy predykcję ceny. 
#   Do predykcji użyj tych samych danych co do trenowania modelu. Zapisz wynik predykcji jako nową kolumnę w ramce z danymi. 
#   Oblicz i wyświetl co najmniej dwie miary skuteczności modelu.

model.reg <- train(price ~ . , data = dane, method="lm", trControl = trainControl(method = "cv"))
# print(model.reg)
dane["predict"] <- predict(object = model.reg, newdata = dane)

RMSE(obs = dane$price, pred = dane$predict)
MAE(obs = dane$price, pred = dane$predict)



# 4) Wykonaj wykres punktowy w pakiecie ggplot2, przedstawiający zależność ceny przewidzianej przez model od ceny rzeczywistej. 
#   Dla lepszej wizualizacji różnic między predykcją a rzeczywistością dodaj do wykresu prostą y=price w kolorze czerwonym. Podpisz osie.

g <- ggplot(dane, aes(x = price, y = predict)) + geom_point(shape=19) +
      geom_line(aes(y=price), color = "red", linewidth=1) +
      labs (x = "Cena rzeczywista", y = "Cena przewidziana") +
      theme_bw()
g

