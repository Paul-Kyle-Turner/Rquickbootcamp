#Knn Model 2
# Paul Turner

library(corrgram)
library(corrplot)
library(caTools)
library(class)
library(ggplot2)

iris.data <- read.csv('input/iris.csv')

# first look
head(iris.data)
str(iris.data)

#factor to numeric conversion
#iris.data$Species <- sapply(iris.data$Species, function(x){as.numeric(x)})
#head(iris.data)
#str(iris.data)
# normalize

scale(iris.data[,1:4])

# cor

corrilation <- cor(iris.data[1:4])
corrplot(corrilation)

# split

split <- sample.split(iris.data$Species)
train.iris.data <- subset(iris.data, split == TRUE)
test.iris.data <- subset(iris.data, split == FALSE)

# knn
classification.error <- NULL
k.values <- 1:20
for (kval in k.values) {
  predicted.species <- knn(train.iris.data[1:4],test.iris.data[1:4],train.iris.data$Species, k = kval)
  classification.error[kval] <- mean(test.iris.data$Species != predicted.species)
}

classification.error.df <- data.frame(classification.error, k.values)
ggplot(classification.error.df, aes(x=k.values,y=classification.error)) + geom_point() + geom_line()

