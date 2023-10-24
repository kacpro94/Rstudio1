#zadanie 1
lista2 <- list(dane1 = c(NA, 1:10),
               dane2 = c(-5:5, NA))

lapply(lista2,mean,na.rm=T)

lapply(lista2,sd,na.rm=T)

lapply(lista2,quantile,na.rm=T)


#zad 2
lista4 <- list(dane1 = 20:1,
               dane2 = 1:10,
               dane3 = 1:5)

jj<-sapply(lista4, max)
jj
sapply(lista4, min)
#Roznica jest to ze sapply zwraca wektor, a lapply liste
jj1<-lapply(lista4,max)
lapply(lista4,min)

funkcja_zmiennosc = function(szym){
  return(sd(szym,na.rm=T)/mean(szym,na.rm=T))
}
sapply(lista4,funkcja_zmiennosc)

#zad 3
?apply()
#bierze macierz zamiast listy czy wektora 
dupa<-apply(mtcars,2,mean)

#zad4
library(dplyr)
tapply(mtcars$mpg,mtcars$cyl, mean)
summary(tapply(mtcars$mpg,mtcars$cyl, mean))

tapply(mtcars$mpg,mtcars$cyl,summary)

#zad5
anscombe
dane<-fgl
fgl

#a
apply(dane[1:9],2,mean)
#b
str(fgl)
#c
