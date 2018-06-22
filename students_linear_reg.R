# R machine learning project 1
# Paul Turner

library(ggplot2)
library(ggthemes)
library(dplyr)
library(corrgram)
library(corrplot)
library(caTools)

# import data

df <- read.csv('input/student-mat.csv', sep=';')

# corrolations plot

num.cols <- sapply(df, is.numeric)

cor.data <- cor(df[,num.cols])

print(corrplot(cor.data))

# clean data

indx <- sapply(df, is.factor)
df[indx] <- lapply(df[indx], function(x) as.integer(x))

# split data into training and testing

sample <- sample.split(df$G3, SplitRatio = .65)

train <- subset(df,sample == TRUE)

test <- subset(df,sample == FALSE)

# create and train model on data

model <- lm(G3 ~ . , data = train)

# predictions

G3.predictions <- predict(model,test)

results <- cbind(G3.predictions, test$G3)
colnames(results) <- c('predictions', 'actual')
results <- as.data.frame(results)
print(head(results))

to.zero <- function(x){
  if(x < 0){
    return(0)
  }else{
    return(x)
  }
}

results$predicted <- sapply(results$predicted, to.zero())

mse <- mean( (results$actual - results$predicted)^2 )
print(mse)

print(mse^.5)

SSE <- sum( (results$predicted - results$actual)^2 )

SST = sum( (mean(df$G3) - results$real)^2)

R2 = 1 - SSE/SST
# visualize model

plot(model)
print(summary(model))

