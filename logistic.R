# logistic Reg
# Paul Turner 

library(ggplot2)
library(ggthemes)
library(dplyr)
library(corrgram)
library(corrplot)
library(caTools)
library(Rcpp)
library(Amelia)

train = read.csv('input/titanic_train.csv')
test = read.csv('input/titanic_test.csv')

#explore data

missmap(train)
