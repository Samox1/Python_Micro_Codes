# LABORATORIUM 8


# Regresja logistyczna
# Regresja Poissona




### Regresja logistyczna ###

# Ramka danych zawierających informację o kandydatach na studia II stopnia. Zawiera następujące kolumny:
# admit - zmienna binarna określająca czy student został przyjęty na studia czy nie,
# gre - graduate record exam - wynik z testu końcowego po studiach I stopnia,
# gpa - grade point average - średnia ocen ze studiów pierwszego stopnia,
# rank - zmienna wyliczeniowa przyjmująca wartości od 1 do 4, 
#        określa prestiż ukończonej szkoły, im niższy tym lepiej

studia <- read.csv("studia.csv")
studia$admit <- factor(studia$admit)
studia$rank <- factor(studia$rank)
summary(studia)

studia.lr <- glm (admit ~ gre+gpa+rank, data=studia, family=binomial)
summary (studia.lr)


# Sposób 1
studia$logit <- predict (studia.lr)
studia$p <- 1/(1+exp(-studia$logit))

# Sposób 2
studia$p2 <- predict (studia.lr, type = "response")

head(studia)


nowi <- data.frame (gre = mean(studia$gre), gpa = mean(studia$gpa), rank = factor(1:4))
nowi$p <- predict (studia.lr, newdata = nowi, type = "response")
nowi


# Dysponujemy następującą ramką danych zawierającą informacje o wynikach kontroli prędkości na drodze, 
# na której ograniczenie prędkości wynosiło 90 km/h.
kontrola.drogowa <- data.frame (gender = rep ( c ("M","F"), c (60,40)),
                                speed = c (runif (60, 60, 120), runif (40, 50, 100)))
kontrola.drogowa$przekroczenie <- factor (ifelse(kontrola.drogowa$speed>90, "tak", "nie"))
head(kontrola.drogowa)

# Sprawdzamy macierz kontygnencji
tab <- table(kontrola.drogowa$gender, kontrola.drogowa$przekroczenie)
tab

tab/sum(tab)

# Wykonujemy test chi^2
chisq.test (tab)

# Wykonujemy regresję logistyczną
kontrola.drogowa.lr <- glm (przekroczenie ~ gender, data=kontrola.drogowa, family=binomial)
summary(kontrola.drogowa.lr)

# Obliczamy prawdopodobieństwa i szanse na to, że kobieta przekroczy dozwoloną prędkość
p.f <- tab["F","tak"]/sum(tab["F",])
p.f

o.f <- p.f/(1-p.f)
o.f

# Obliczamy prawdopodobieństwa i szanse na to, że kobieta przekroczy dozwoloną prędkość
p.m <- tab["M","tak"]/sum(tab["M",])
p.m

o.m <- p.m/(1-p.m)
o.m

# Iloraz szans
o.m/o.f

# Eksponens współczynników regresji liniowej
exp(kontrola.drogowa.lr$coefficients)


library(dplyr)
library(tidyr)
library(magrittr)

UCBAdmissions

admissions <- as.data.frame (UCBAdmissions)
admissions %<>% pivot_wider (names_from = "Admit", values_from = "Freq")
admissions %<>% group_by (Dept) %>% summarise (admitted = sum (Admitted), rejected = sum (Rejected))
admissions %<>% mutate (p = admitted/(admitted+rejected)) %>% mutate (o = p/(1-p))
admissions

admissions.lr <- glm (cbind (admitted,rejected) ~ Dept, data = admissions, family = binomial)
summary (admissions.lr)

exp (admissions.lr$coefficients)
admissions$o/admissions$o[1]

Titanic

# Transformacja tablicy na ramkę danych
titanic <- as.data.frame(Titanic[,,,])
head(titanic)

titanic %<>% pivot_wider(names_from = "Survived", values_from = "Freq")
names(titanic)[4:5] <- c("Died","Survived")
head(titanic)

# Wykonanie regresji logistycznej
titanic.lr <- glm (cbind (Survived,Died) ~ Class+Sex+Age, data = titanic, family = binomial)
summary(titanic.lr)

test <- data.frame (Class = c("1st","2nd","3rd"), Age = rep("Adult",3), Sex = rep("Male",3))
predict(titanic.lr, newdata = test)

predict(titanic.lr, newdata = test, type = "response")



### Regresja Poissona ###

warp <- warpbreaks
warp

library(ggplot2)
g <- ggplot(warp)

g + geom_histogram (aes(x=breaks, y=..density..), breaks=seq(0,70,10), alpha=0.5) +
  geom_density (aes(x=breaks), color="red", size=2)

# Regresja Poissona
warp.pr <- glm(breaks~wool+tension, data=warp, family = poisson)
summary(warp.pr)

# Predykcja dla nowych danych
te <- c(8,9,17,18,26,27,35,36,44,45,53,54)
warp.te <- warpbreaks[te,] 
warp.tr <- warpbreaks[-te,] # Te próbki wyłączam z testowania modelu

warp.pr <- glm(breaks~wool+tension, data=warp.tr, family = poisson)
warp.te$predict <- predict(warp.pr, newdata=warp.te[,-1], type="response")
warp.te


library(faraway)
data (gala)
head (gala)

summary (gala)

library(ggplot2)
g <- ggplot(gala)

g + geom_histogram (aes(x=Species, y=..density..), breaks=seq(0,450,50), alpha=0.5) +
  geom_density (aes(x=Species), color="red", size=2)

gala.pr <- glm (Species ~ ., data = gala, family = poisson)
summary (gala.pr)

gala.pr2 <- glm (Species ~ Endemics+Area+Nearest, data = gala, family = poisson)
summary (gala.pr2)

exp(gala.pr2$coefficients)
