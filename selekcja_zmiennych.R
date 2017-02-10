library(Boruta)
boruta<-Boruta(Survived ~ . -Cabin -Name -PassengerId -Ticket,data=train,doTrace=2)
attStats(boruta)
modelBoruta<-getConfirmedFormula(boruta)
