# LABORATORIUM 3

# Potoki
# Pakiet dplyr
# Pakiet tidyr
# Cheat Sheets


### Potoki ###

x <- c(0.109, 0.359, 0.63, 0.996, 0.515, 0.142, 0.017, 0.829, 0.907)
round(exp(diff(log(x))), 1)

library(magrittr)

x %>%
  log() %>%
  diff() %>%
  exp() %>%
  round(1)

log(2)
2 %>% log
round(pi, 6)
pi %>% round(6)
6 %>% round(pi, digits=.)
x <- 2
x %<>% log()
x

df1 <- data.frame(a = 1:10, b = (1:10) + runif(10,-2,2), c = 1:10)
# Pełna macierz korelacji
df1 %>% cor

# Teraz chcemy korelowac jedynie kolumny a i b
df1 %>% cor(a,b)
# A do tego potrzebny jest operator %$%
df1 %$% cor(a,b)

# R ma wlasne potoki - od wersji 4.1
# 2 |> sqrt()
# pi |> round(6)


### Pakiet dplyr ###
df1 <- data.frame(a = 1:10, b = runif(10,-2,2), c = 1:10)
df1[order(df1$b),]

library(dplyr)
arrange(df1, b)

df1[,c("b","a")]

select(df1, b, a)

df1[df1$a < 6 & df1$b < 0,]

filter(df1, a < 6, b < 0)

df1 %>%
  filter(a < 6, b < 0) %>% 
  select(c, b) %>%
  arrange(desc(b), c)

df1 %<>% mutate(d=a*b+c)
df1

# Wczytanie zbioru zawierającego dane o kwiatach
data("iris")
# Prezentacja zbioru
head(iris)

summary(iris)

iris %>%
  group_by(Species) %>%
  summarise(mean.sepal.length = mean(Sepal.Length), mean.sepal.width = mean(Sepal.Width))



### Pakiet tidyr ###

# Szeroka tablica
rank.wide <- read.table("rank_vs_rate.txt", header=TRUE)
rank.wide

# Długa tablica
rank.long <- read.table("rank.txt", header=TRUE)
rank.long

library(tidyr)
rank.longer <- pivot_longer(rank.wide, 
                            cols=2:4, 
                            names_to = "rate", 
                            values_to = "rank", 
                            names_prefix = "rate", 
                            names_transform = list(rate=as.numeric))
rank.longer

rank.wider <- pivot_wider(rank.long,
                          names_from = rate,
                          values_from = rank)
rank.wider


### Cheat sheets ###
# Link: https://www.rstudio.com/resources/cheatsheets/
# Innym sposobem jest kliknięcie w RStudio przycisku Help i wybranie opcji Cheat Sheets z rozwijanego menu.


