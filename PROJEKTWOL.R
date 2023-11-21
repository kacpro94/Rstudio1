#JAK RODZICE i OTOCZENIE WPLYWA NA SPOZYCIE ALKOHOLU SWOICH DOROSLYCH DZIECI
library(dplyr)
library(tidyr)
library(ggplot2)
library(mice)
library(corrplot)
dane <- read_csv("archive (4).zip")
summary(dane)

#poznajemy dane
head(dane)
tail(dane)

str(dane)

#w zasadzie duża większośc danych to zmienne porządkowe(od 1-5), ale jest też kilka kategor
#ycznych np. płec, wyksztalcenie lub czy osoba miala dostep do internetu. Jest tez kilka zmiennych 
#numerycznych np. GP3, które przedstawia ocenę końcową
md.pattern(dane)
#zobaczymy rozklady niektorych zmiennych, zacznijmy od miejsca zamieszkania
table(dane$address)
dane %>%
  ggplot() +
  geom_bar(aes(x = address))

#widzimy pewien problem, gdyz probka ze wsi jest nieliczna, co moze zawyzac lub zanizac nasze wyniki

#rozklad oceny koncowej
summary(dane$G3)
dane%>%
  ggplot()+
  geom_density(aes(x=G3))

#spradzamy wyksztalcenia rodzicow
table(dane$Medu)
table(dane$Fedu)
#co ciekawe to kobiety maja lepsze wyksztalcenie
#TUTAJ WYKRES!!!!!!!

#picie w weekend
table(dane$Walc)

#picie w tygodniu
table(dane$Dalc)

dane %>%
  ggplot() +
  geom_point(aes(x = Dalc, y = studytime), size = 2, 
             color = "grey50", alpha = 0.2)
#widzimy, ze wraz ze wzrostem picia w tygodniu, maleje nam liczba godzin nauki
#ZMIEN WYKRES

#jaka szkola
table(dane$school)
#znowu mamy malo danych ze szkoly MS,co moze zawyzac lub zanizac

#mozemy sobie porownac np ilosc nauki do szkoly



#sprawdzmy czy np ilosc nauki w tyg wplywa na spozycie alkoholu w tygodniu
#try to predict students final grade.
#jak wyksztalcenie rodzicow wplywa na spozycie alkoholu dziecka
#czy im dziecko sie wiecej uczy w tygodniu tym wiecej pije?
#czy czas podrozy do szkoly wplywa na ilosc nauki
#czy zwiazek ma znaczenie do spozycia alkoholu
#spozycie alkoholu wplywa na ilosc nieobecnosci
#czy pochodzenie(miasto/wies) ma znaczenie
#czy rozwod rodzicow wplywa na ilosc wypitego alkoholu/ocene na koniec
#czy wieksze wyksztalcenie rodzicow ma wplyw na wybor dziecka szkoly ze wzgledu na reputacje
#ktora szkola lepsza
#sprawdzic czy ilosc alkoholu wplywa na negatywna ocene(<10GP3), nizej porownanie jak
#picie w tygodniu oraz w weekend wplywa na ocene
#spozycie alkoholu a zwiazek


ggplot(dane,aes(y=factor(studytime),x=G3))+
  geom_boxplot()
#tutaj widzimy, ze nim wiecej czsu studenci spedzali nad nauką, tym wyższa koncowa ocene mieli
#co ciekawe, nawet studenci ktorzy uczyli sie po 4 godziny nie zdawali ORAZ uczniowie ktorzy uczyli sie 3 godziny
#wychodzili na tym lepiej niz ci co uczyli sie 4

#Sprawdzmy ile bylo takich studentow:
dane%>%
  select(studytime,G3)%>%
  filter(studytime==4)%>%
  count(G3==0)
#widzimy ze bylo 3 takich studentow
dane%>%
  select(studytime,G3)%>%
  filter(studytime==4)%>%
  count(G3==20)


#sprawdzmy ile studentow, ktorzy uczli sie godzine nie zdalo
dane%>%
  select(studytime,G3)%>%
  filter(studytime==1)%>%
  count(G3==0)
#widzimy ze bylo ich 4 razy wiecej

dane%>%
  select(studytime,G3)%>%
  filter(studytime==4)%>%
  ggplot()+
  geom_density(aes(x=G3))
  
  
  
