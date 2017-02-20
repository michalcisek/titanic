rm(list=ls())
source("dane.R")
library(randomForest)

set.seed(750)


rf<-randomForest(factor(Survived) ~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+Title,data=train)
plot(rf)

predykcja<-predict(rf,test)

rf_rozwiazanie<-data.frame(PassengerID=test$PassengerId, Survived=predykcja)
write.csv(rf_rozwiazanie, file = '20170220_rf_rozwiazanie.csv', row.names = F)

