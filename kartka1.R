#zad 1
pierwsza<-c(1:8)
druga<-c(13,5,2,7,9,121,65,4)

suma<-pierwsza+druga
różnica<-pierwsza-druga
więcej<-pierwsza+1

mean(x = pierwsza)
sd(x = druga)

#zad2

wzrost<-c(160,170,145,188,190,187,156)
waga<-c(70,66,50,90,88,67,86)
osoba<-data.frame(wzrost,waga)

str(osoba)
head(osoba)
tail(osoba)
dim(osoba)
summary(osoba)

#zad3
help("row.names")
row.names(osoba)<-c("Jan","Adam","Gosia","Patryk","Andrzej","John","Ala")
head(osoba)
(BMI<-waga/(wzrost/100)^2)

BMI[BMI>30]

sum(BMI<=25)
#zad 4
datasets::mtcars
datasets::Orange

str(mtcars)
str(Orange)
####
help(mtcars)
help(Orange)
mtcars
auta<-mtcars$mpg
mtcars[mtcars$cyl==6,]
####
spalanie<-mtcars[mtcars$mpg<20,]
mean(spalanie$hp)
####
hist(Orange$age)
####
boxplot(mtcars$mpg)
boxplot(mtcars$hp)      
boxplot(mtcars$qsec)      
#zad5
library(psych)
describe(mtcars)
summary(mtcars)
?describeBy
describeBy(mtcars,mtcars$cyl)

#zad6
proba<-sample(1:6,20,replace=T)
describe(proba)
###
rzut<-sample(1:2,100,replace=T)
sum(rzut==1)
###
sample(0:100,10,replace = T)
?rpois
plot(rt(100,df = 1))
plot(rpois(n = 100,2))
plot(rnorm(n = 100,mean = 3,sd = 1))

#zad7
lista <- list(palenie <- c(F,T,T,F,F,T,F),
              cyfry <- 0:9,
              macierz <- matrix(sample(1:5,16,replace = T),4,4),
              tekst <- c("Litwo", "ojczyzno", "moja", "ty",
                         "jestes", "jak", "zdrowie"))
lista[4]
###
cyfry<-c(lista[[2]])
cyfry[3]
###
print(lista[[3]][,3])

#zad8
wiek <- c(50, 17, 39, 27, 90)
waga <- c(80, 75, 92, 105, 60)
pacjenci<-data.frame(wiek,waga)

pacjenci<-pacjenci[wiek>18&waga<90,]

#zad 9

library(nycflights13)

flight_lm <- lm(arr_delay ~ dep_delay + month + carrier, 
                data = flights)

head(flight_lm)
View(flight_lm)
str(flight_lm)
names(flight_lm)
summary(flight_lm)
#zad 1o
VADeaths
colnames(VADeaths)
rownames(VADeaths)

male<-matrix(VADeaths[,c(1,3)],,2)
female<-matrix(VADeaths[,c(2,4)],,2)

mean(male[,1])
mean(male[,2])
srednia<-c(mean(male[,1]),mean(male[,2]))
male1<-rbind(male,srednia)

srednia2<-c(mean(female[,1]),mean(female[,2]))
female1<-rbind(female,srednia2)

#zad11****************

liczba<-as.numeric(readline("wprowdź liczbe: "))
liczba
if(liczba%%4==0){
  print("liczba podzielna przez 4")
} else{
  print("liczba niepodzielna przez 4")
}

#zad12********************8
ksiazki <- sample(0:4, size = 25, replace = TRUE)
mean(ksiazki)

sum(ksiazki>=3)
sum(ksiazki==0)
ksiazki2<-0
for(x in ksiazki){
  if(x>=1){
    ksiazki2<-ksiazki2+1
  }
  
  
}
ksiazki2#tyle osob czyta ksiazki

#****

miesiac<-as.numeric(readline("podaj numer miesiaca: "))

if(miesiac==1&3&5&7&8&10&12){
  print("miesiac ma 31 dni")
}else if(miesiac==2){
  rok<-as.numeric(readline("Podaj rok: "))
  if(rok%%4==0){
    print("miesiac ma 29 dni")
  }else{print("miesiac ma 28 dni")}
  
}else{print("miesiac ma 30 dni")}