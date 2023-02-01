### KOLOKWIUM - Zadanie 1
# Autor: Szymon Baczyński
# Data:  31.01.2022


rm(list = ls())


## Do zadań należy wykorzystać zbiór danych Arrests z biblioteki carData dotyczący 
## aresztowań osób w Toronto za posiadanie niewielkich ilości marihuany. 
## Przed przystąpieniem do zadań należy zapoznać się ze zbiorem w zakładce Help.


library(dplyr)
library(tidyr)
library(carData)
library(ggplot2)
library(MASS)


# 1) Przy pomocy pakietu funkcji group_by oraz summarise i n(), stwórz ramkę danych, 
# w której dla każdego roku będzie informacja ile osób zostało zwolnionych z aresztu po zatrzymaniu, a ile nie.


Arrests <- carData::Arrests
dane_years <- Arrests %>% group_by(year, released) %>% summarise(count = n()) 
print(dane_years)



# 2) Przekształć ramkę z pkt 1. na szeroką rozbijając kolumnę release na dwie kolumny.

dane_wide <- dane_years %>% pivot_wider(names_from = released, values_from = count, names_prefix = "released.")
print(dane_wide)



# 3) Wykonaj wykres punktowy składający się z dwóch serii wedle poniższego wzoru.

g1 <- ggplot(dane_wide, aes(x = year)) + 
      geom_point(aes(y = released.No, color="No"), size=3) +
      geom_point(aes(y = released.Yes, color="Yes"), size=3) +
      labs (x = "Year", y = "Number of individuals", color = "Released") +
      theme_bw()
g1



# 4) Przy pomocy testów χ2 Pearsona dla macierzy kontyngencji sprawdź czy zmienna released jest 
#     zależna od zmiennych colour, sex oraz citizen. Udziel odpowiedzi w komentarzu.

print(chisq.test(Arrests$released, y = Arrests$colour))
print(chisq.test(Arrests$released, y = Arrests$sex))
print(chisq.test(Arrests$released, y = Arrests$citizen))

# Komentarz:
# Zmienna "released" jest zależna od "colour" i "citizen"



# 5) Wykonaj histogram zmiennej wiek zbudowany z ok. 20 binów. Przy pomocy funkcji fitdistr() oraz ks.test() 
#   zweryfikuj jaki rozkład najlepiej opisuje tę zmienną. Wybierz pośród rozkładów Poissona, Weibulla oraz Gamma. 
#   Nanieś teoretyczne gęstości rozkładów na histogram w postaci kolorowych ciągłych linii i dodaj legendę. 
#   Do wykonania wykresu można użyć pakietu podstawowego lub ggplot2


hist_age <- hist(Arrests$age, breaks = 20, freq = FALSE)

fit_Poisson <- fitdistr(Arrests$age, "Poisson")
fit_Poisson$loglik
ks.test(Arrests$age, "ppois", lambda = fit_Poisson$estimate["lambda"])

fit_Weibulla <- fitdistr(Arrests$age, "weibull")
fit_Weibulla$loglik
ks.test(Arrests$age, "pweibull", shape = fit_Weibulla$estimate["shape"], scale = fit_Weibulla$estimate["scale"])

fit_Gamma <- fitdistr(Arrests$age, "gamma")
fit_Gamma$loglik
ks.test(Arrests$age, "pgamma", shape = fit_Gamma$estimate["shape"], rate = fit_Gamma$estimate["rate"])

# hist_age <- hist(Arrests$age, breaks = 20, freq = FALSE)
lines(dpois(seq(1, range(Arrests$age)[2]), fit_Poisson$estimate["lambda"]), col="green", lwd=2)
lines(dweibull(seq(1, range(Arrests$age)[2]), shape = fit_Weibulla$estimate["shape"], scale = fit_Weibulla$estimate["scale"]), col="red", lwd=2)
lines(dgamma(seq(1, range(Arrests$age)[2]), shape = fit_Gamma$estimate["shape"], rate = fit_Gamma$estimate["rate"]), col="blue", lwd=2)
legend ("topright", legend = c('Poisson','Weibull','Gamma'), lty = c(1,1,1), lwd = c(2,2,2) ,col = c("green", "red", "blue"))


