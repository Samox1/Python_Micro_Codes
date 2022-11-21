### Laboratorium nr 5 - Zadanie 3
# Autor: Szymon Baczyński
# Data:  21.11.2022

rm (list = ls())

library(ggplot2)

# 1) Wczytaj do pamięci zbiór danych mtcars z biblioteki datasets

# ?datasets::mtcars -> Format
# [, 1]	mpg	= Miles/(US) gallon       <--- rozmiar
# [, 2]	cyl	= Number of cylinders
# [, 3]	disp	= Displacement (cu.in.)
# [, 4]	hp	= Gross horsepower        <--- x
# [, 5]	drat	= Rear axle ratio
# [, 6]	wt	= Weight (1000 lbs)
# [, 7]	qsec	= 1/4 mile time         <--- y
# [, 8]	vs	= Engine (0 = V-shaped, 1 = straight)
# [, 9]	am	= Transmission (0 = automatic, 1 = manual)   <--- kolor
# [,10]	gear	= Number of forward gears
# [,11]	carb	= Number of carburetors

dane_cars <- datasets::mtcars


# 3) Rozmiar punktów jest propocjonalny do zużycia paliwa wyrażonego w litrach na 100 km 
#     (należy przeliczyć z mil na galon)

dane_cars["paliwo"] <- 235.214583 / dane_cars["mpg"]


# 4) Kolorem oznaczony jest rodzaj skrzyni biegów. Przydatnym może być utworzenie 
#     dodatkowej kolumny typu factor lub character (zmienna am jest typu numeric)

dane_cars["skrzynia"] <- with(dane_cars, ifelse(am == 0, "automatic", "manual"))


# 2) Wykonaj wykres punktowy zależności czasu na 1/4 mili od mocy silnika

g <- ggplot(dane_cars)
g + geom_point(aes(x = hp, y = qsec, fill=skrzynia, size=paliwo), shape=21) + 
  ggtitle("Dane z magazynu Motor Trend (1974)") +
  xlab("moc [KM]") +
  ylab("czas na 1/4 mili [s]")


# 5) Zapisz obrazek do pliku w formacie pdf.

ggsave ("Lab-5_Zad-3_SB.pdf", device = "pdf", width = 15, height = 10, units = "cm")
