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
describeBy()

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

#zad8
liczba<-as.numeric(readline("wprowdź liczbe: "))
liczba
if(liczba%%4==0){
  print("liczba podzielna przez 4")
} else{
  print("liczba niepodzielna przez 4")
}
#zad9
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

#zad10

miesiac<-as.numeric(readline("podaj numer miesiaca: "))

if(miesiac==1&3&5&7&8&10&12){
  print("miesiac ma 31 dni")
}else if(miesiac==2){
  rok<-as.numeric(readline("Podaj rok: "))
  if(rok%%4==0){
    print("miesiac ma 29 dni")
  }else{print("miesiac ma 28 dni")}
  
}else{print("miesiac ma 30 dni")}
#zad 11