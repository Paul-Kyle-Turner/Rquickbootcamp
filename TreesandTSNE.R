# Dec / Random Tree
# Paul Turner

library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ggplot2)
library(corrgram)
library(corrplot)
library(dplyr)
library(Rtsne)
library(ISLR)

# college data

data.college <- College

str(data.college)

ggplot(data.college, aes(Grad.Rate, Room.Board)) + geom_point(aes(color = Private))

ggplot(data.college, aes(F.Undergrad)) + geom_histogram(aes(fill = factor(Private)))

ggplot(data.college, aes(Grad.Rate)) + geom_histogram(aes(fill = factor(Private)))

strange <- subset(data.college, Grad.Rate > 100)
data.college['Cazenovia College','Grad.Rate'] <- 100

ggplot(data.college, aes(Grad.Rate)) + geom_histogram(aes(fill = factor(Private)))

#tsne but labels are confusing me right now

tsne = F
if(tsne == TRUE){
  rtsne <- Rtsne(data.college, dims = 2, perplexity = 30, verbose = T, max_iter = 1000)
  
  embeding <- as.data.frame(rtsne$Y)
  
  ggplot(embeding, aes(embeding[,1], embeding[,2], colours = data.college$Private)) + geom_point()
  
}

# split college data into .7 .3 for training and testing

split.college <- sample.split(College$Grad.Rate, SplitRatio = .7)

train.college <- subset(data.college, split.college == T)
test.college <- subset(data.college, split.college == F)

# Trees

dec.tree <- rpart(train.college$Private ~ . , train.college)
random.forest <- randomForest(train.college$Private ~ . , train.college)

tree.predictions.dec.tree <- predict(dec.tree, test.college)
tree.predictions.random.forest <- predict(random.forest, test.college)

head(tree.predictions.dec.tree)

private.tree.predict <- function(x){
  if(x >= .5){
    return('Yes')
  }else{
    return('No')
  }
}

tree.predictions.dec.tree$predictions <- sapply(tree.predictions.dec.tree$Yes, private.tree.predict)

prp(dec.tree)

table(tree.predictions.dec.tree)












