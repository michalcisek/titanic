# OPIS ZMIENNYCH:
# survived        Przezycie
# (0 = Nie; 1 = Tak)
# Pclass          Klasa
# (1 = pierwsza klasa; 2 = druga klasa; 3 = trzecia klasa)
# Name            Imie i nazwisko
# Sex             Plec
# Age             Wiek
# Sibsp           Liczba rodzenstwa i wspolmalzonkow na pokladzie 
# Parch           Liczba rodzicow i dzieci na pokladzie
# Ticket          Numer biletu
# Fare            Oplata za przejazd
# Cabin           Kabina
# Embarked        Miejsce wejscia na poklad
# (C = Cherbourg; Q = Queenstown; S = Southampton)
# Titanic plynal trasa Southampton -> Cherbourg -> Queenstown -> Nowy Jork
# 
# Jesli wiek ponizej roku to w postaci liczby po przecinku (np. 0.75)
# Jesli wiek szacowany to jest postaci xx.5

rm(list=ls())
library(ggplot2)
library(dplyr)
library(mice)

train<-read.csv("train.csv",stringsAsFactors = F)
test<-read.csv("test.csv",stringsAsFactors = F)
dane<-bind_rows(train,test)

# Brakujace obserwacje ----------------------------------------------------
apply(dane,2,function(x) sum(is.na(x)))

# Dwie wartosci brakujace dla Embarked
table(dane$Embarked)
dane[dane$Embarked=="",]
#gdzie pasezerowie wsiadali mozemy wywnioskowac na podstawie ceny zaplaconej za bilet. Najdrozsze bilety powinni placic
#pasazerowie wsiadajacy w Cherbourg (najdluzsza odleglosc), a najtansze w Southampton

#sprawdzamy jak rozkladaja sie ceny w zaleznosci od portu i klasy
ggplot(dane[dane$Embarked!="",],aes(x=Embarked,y=Fare,fill=factor(Pclass)))+
  geom_boxplot()+
  theme_bw()+
  geom_hline(yintercept = 80,colour="red",linetype="dashed")

#widac ze mediana ceny biletu dla pierwszej klasy w Cherbourg jest rowna obserwowanej cenie biletow osob dla ktorych brakuje nam Embarked
dane[dane$Embarked=="","Embarked"]<-"C"

# 1 brakujacy dla Fare
dane[is.na(dane$Fare),]

#sprawdzamy gestosc oplaty za bilet dla pasazerow wsiadajacych w Southampton z 3 klasy
ggplot(dane[dane$Embarked=="S" & dane$Pclass==3,], aes(x=Fare))+
  geom_density()+
  geom_vline(xintercept=median(dane[dane$Embarked=="S" & dane$Pclass==3,"Fare"],na.rm=T), linetype="dashed")+
  theme_bw()

#zauwazamy ze przypisanie mediany bedzie najrozsadniejsze
dane[is.na(dane$Fare),"Fare"]<-median(dane[dane$Embarked=="S" & dane$Pclass==3,"Fare"],na.rm = T)

# 263 brakujacych dla Age
dane[is.na(dane$Age),]

# Make variables factors into factors
factor_vars <- c('PassengerId','Pclass','Sex','Embarked')

dane[factor_vars] <- lapply(dane[factor_vars], function(x) as.factor(x))

# Set a random seed
set.seed(750)

# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(dane[, !names(dane) %in% c('PassengerId','Name','Ticket','Cabin','Survived')], method='rf')
mice_output<-complete(mice_mod)

dane$Age<-mice_output$Age

rm(mice_mod,mice_output,factor_vars)




dane$Name %>%
  regexpr("(?<=, ).*?(?=\\s)",.,perl = T)%>%
  regmatches(dane$Name,.) -> dane$Title

table(dane$Title)
# Capt.
# Col. - colonel
# Don.
# Dona.
# Dr. - doktor
# Jonkheer. - duński tytuł szlachecki
# Lady.
# Major.
# Master.- chlopiec ponizej 13. roku zycia, rzadziej mezczyzna ktory jest kawalerem
# Miss. - panny
# Mlle. - mademoiselle - francuski odpowiednik Miss.
# Mme. - francuski odpowiednik Mrs.
# Mr. - mezczyzni (w kazdym wieku)
# Mrs. - kobiety zamezne (obecnie i przeszlosci, czyli wdowa tez)
# Ms. - forma bezpieczna - nie wskazuje na stan cywilny kobiety
# Rev. - pastor
# Sir.
# the

inne_tytuly<-c('Capt.','Col.','Don.','Dona.','Dr.','Jonkheer.','Lady.','Major.','Rev.','Sir.','the')
dane$Title[dane$Title=='Ms.']<-"Miss."
dane$Title[dane$Title=='Mme.']<-"Mrs."
dane$Title[dane$Title=='Mlle.']<-"Miss."
dane$Title[dane$Title %in% inne_tytuly]<-"Inne"

dane$Title<-factor(dane$Title)

train<-dane[1:891,]
test<-dane[892:1309,c(1,3:13)]

rm(dane,inne_tytuly)
