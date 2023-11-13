library(tidyr)
library(dplyr)
library(ggplot2)
wta <- read_csv("wta.csv")

wygrane<-wta%>%
  count(Winner)%>%
  top_n(3)

  
#wykres przedstawiajacy 3 panie z najwieksza iloscia wygranych
library(ggplot2)
wykres<-wygrane%>%
  ggplot(mapping = aes(x = Winner,y=n))+
  geom_bar(stat = "Identity")

wykres

swiatekPrzegrane<-wta%>%
  count((Player_1=="Swiatek I."|Player_2=="Swiatek I.")&Winner!="Swiatek I.")
swiatekliczbameczow<-wta%>%
  count(Player_1=="Swiatek I."|Player_2=="Swiatek I.")

swiatekWygrane<-wta%>%
  count(Winner=="Swiatek I.")
  
swiatekwyniki<-data.frame()
swiatekwyniki<-rbind(swiatekWygrane[2,2],swiatekPrzegrane[2,2])
rownames(swiatekwyniki)<-c("wygrane","przegrane")

#wykres przedstawiajacy ile meczow iga przegrala a ile wygrala od poczatku kariery
wykres2<-swiatekwyniki%>%
  ggplot(mapping = aes(x = row.names(swiatekwyniki),y=n))+
  geom_bar(stat = "Identity")+
  labs(y="liczba meczow",x="")
wykres2

wta$Date<-as.Date(wta$Date)

wynikirok<-wta%>%
  group_by("rok"=format(Date,"%Y"))%>%
  count("wygrane"=Winner=="Swiatek I.")%>%
  filter(wygrane==T)%>%
  select(rok,n)

#wykres przedstawiajacy wygrane igi swiatek w czasie
wynikirok%>%
  ggplot(aes(x=rok,y=n,group=1))+
  geom_line(size=1.5)

model<-lm(data=wynikirok)
  
  
ggplot(wynikirok,aes(x=rok,y=n))+
  geom_point()+
  geom_smooth()
#mozna zrobic wykres ile zwyciest miala iga w kazdym roku
#wykres porownoujacy sabalenke z iga
#jakas zaleznosc miedzy tym na jakim korcie iga grala i jaki byl wynik meczu
#linia trendu dla igi?
#sprawdzic zaleznosc czy jesli osoba z wieksza ranga gra z nizsza to czy czesciej wygrywa
#spradzic ile finalow wygrala iga a ile przegrala(czy jest jakas zaleznosc)

