### Laboratorium nr 3 - Zadanie 1
# Autor: Szymon Baczyński
# Data:  07.11.2022

library(magrittr)
library(dplyr)
library(tidyr)


# 1) Wczytaj do pamięci zbiór pomiarów z symulacji umieszczony w pliku er_N1000_k8_O0.1_part1.txt

sim_data <- data.frame(read.table("E:\\GitHub\\Python_Micro_Codes\\AWDJR_2022Z\\Laboratorium_3\\Lab_3_Zadanie\\er_N1000_k8_O0.1_part1.txt", header = TRUE))
print(head(sim_data))


# 2) Korzystając z operatora potoku i funkcji group_by() oraz summarise() oblicz dla każdej kombinacji zmiennych rate, observers, method:
    # średnią precyzję,
    # kwantyl rzędu α=0.95 (funkcja quantile()) zmiennej rank,
    # średni czas.

sim_data %<>% group_by(rate, observers, method) %>%
  summarise(prec.mean=mean(precision), css0.95=quantile(rank, 0.95), time.mean=mean(time))

print(sim_data)


# 3) Dokonaj transformacji powyższej tabeli zmieniając ją na szeroką tabelę. 
#   Kolejne kolumny mają zawierać średnią precyzję dla różnych przypadków z kolumny method (pomiń kolumny css0.95 i time.mean dla uzyskania odpowiedniego efektu)

sim_data_wide <- sim_data %>% 
                select(-c(css0.95, time.mean)) %>%
                pivot_wider(names_from = method, values_from = prec.mean)

print(sim_data_wide)


# 4) Przy pomocy funkcji filter() wybierz z powyższej tabeli tylko warianty z losowymi obserwatorami

sim_data_wide_random <- sim_data_wide %>% filter(observers == "Random")
print(sim_data_wide_random)


# 5) Posługując się ramką danych otrzymaną w pkt. 2 oraz funkcjami select(), filter(), group_by(), summarise(), which.max(), sprawdź, która metoda ma najwyższą średnią precyzję w przypadku losowych obserwatorów.

# ramka z 4) 
sim_data_wide_random_best_method <- sim_data_wide_random %>% .[,-2] %>% mutate(method.best = colnames(.[,-1])[max.col(.[,-1])]) %>% .[,c('rate','method.best')]
print(sim_data_wide_random_best_method)

# ramka z 2)
sim_data_best <- sim_data %>% select(-c(css0.95, time.mean)) %>% filter(observers == "Random") %>% summarise(method.best = method[which.max(prec.mean)]) %>% .[,-2]
print(sim_data_best)