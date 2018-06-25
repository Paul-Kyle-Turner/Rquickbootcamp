# Random Forest
# Paul Turner

library(randomForest)

random.forest.model <- randomForest(Kyphosis ~ . , kyphosis)

print(random.forest.model)

random.forest.model$confusion
