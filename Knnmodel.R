#Knearest 
#Paul Turner

library(ISLR)
library(class)
library(ggplot2)

#Data that will be used is Caravan
any(is.na(Caravan))

purchased.caravan <- Caravan[,86]
standardized.Caravan <- scale(Caravan[-86])

indx <- 1:1000

test <- standardized.Caravan[indx,]
purchased.test <- purchased.caravan[indx]

train <- standardized.Caravan[-indx,]
purchased.train <- purchased.caravan[-indx]

kept.classification.error <- 1
classification.error <- NULL

kvalues <- 1:20
for (k in kvalues) {
  purchased.predicted <- knn(train ,test ,purchased.train ,k=k)
  
  classification.error[k] <- mean(purchased.test != purchased.predicted)
}

max(classification.error)
min(classification.error)


error <- data.frame(classification.error, kvalues)
ggplot(error, aes(kvalues, classification.error)) + geom_point() + geom_line()

