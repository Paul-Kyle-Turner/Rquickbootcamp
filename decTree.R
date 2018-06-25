# Decision Trees
# Paul Turner

library(rpart)
library(rpart.plot)

str(kyphosis)
head(kyphosis)

tree <- rpart(Kyphosis ~ . , method = 'class', data = kyphosis)

printcp(tree)

prp(tree)

