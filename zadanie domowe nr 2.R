#zadanie 1
lista2 <- list(dane1 = c(NA, 1:10),
               dane2 = c(-5:5, NA))

lapply(lista2,is.na)
library(dplyr)
library(gapminder)
gapminder  
kraje<-gapminder%>%
  select(country,year,pop)
kraje
#zad 2
lista4 <- list(dane1 = 20:1,
               dane2 = 1:10,
               dane3 = 1:5)
sapply(lista4, max)
sapply(lista4, min)
