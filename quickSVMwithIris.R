# Svm
# Paul Turner

library(ISLR)
library(e1071)

data <- iris

help(svm)

model <- svm(Species ~ . , data = data)

tuned.model <- tune(svm, train.x = data[1:4], train.y = data[,5], kernel = 'radial', ranges = list(cost=c(0.01,.1,1.5,2,2.5),gamma=c(0.1,0.5,1,2)))

tuned.model


