rm(list=ls())
source("dane.R")

library(ggplot2)
ggplot(train, aes(x=Pclass,y=factor(Survived)))+
  geom_jitter()
spineplot(train$Pclass,factor(train$Survived))


library(corrplot)
corrplot(train,method="number")
