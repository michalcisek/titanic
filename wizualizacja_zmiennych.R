rm(list=ls())
library(ggplot2)
library(GGally)

my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=loess, fill="red", color="red", ...) +
    geom_smooth(method=lm, fill="blue", color="blue", ...)
  p
}

source("dane.R")


ggplot(train, aes(x=Pclass,y=factor(Survived)))+
  geom_jitter()
spineplot(train$Pclass,factor(train$Survived))


g = ggpairs(train,columns = c(2,3,5:8,10,12,13), lower = list(continuous = my_fn))
g
