# logistic Reg
# Paul Turner 

library(ggplot2)
library(dplyr)
library(corrgram)
library(corrplot)
library(caTools)
library(Rcpp)
library(Amelia)

train = read.csv('input/titanic_train.csv')
test = read.csv('input/titanic_test.csv')

#explore data

any(is.na(train))
any(is.na(test))

missmap(train) # creates a map that identifies the missing values

# plot explore

ggplot(train, aes(Survived)) + geom_bar(aes(fill=factor(Pclass)))

ggplot(train, aes(Pclass)) + geom_bar(aes(fill=factor(Pclass)))

ggplot(train, aes(Age)) + geom_histogram(aes(fill=factor(Pclass)))

ggplot(train, aes(Fare)) + geom_histogram(aes(fill=factor(Pclass)))

# replace missing data

na.replace.age <- function(age, class){
  out <- age
  for (i in 1:length(age)) {
    if (is.na(age[i])){
      if (class[i] == 1){
        out[i] = 37
      }else if (class[i] == 2){
        out[i] = 29
      }else{
        out[i] = 24
      }
    }else{
      out[i] = age[i]
    }
  }
  return(out)
}

train$Age = na.replace.age(train$Age, train$Pclass)
test$Age = na.replace.age(test$Age, test$Pclass)

missmap(train)

# delete some data that doesn't matter

train <- select(train, -PassengerId,-Name,-Ticket,-Cabin)
test <- select(test, -PassengerId,-Name,-Ticket,-Cabin)

# force factors to useable int then numeric

indx <- sapply(train, is.factor)
train[indx] <- lapply(train[indx], function(x){as.integer(x)})
train.features <- as.data.frame(sapply(train, as.numeric))

indx <- sapply(test, is.factor)
test[indx] <- lapply(test[indx], function(x){as.integer(x)})

# cor plot for unseen corilations

train.features <- cor(train.features)
corrplot(train.features)

# model

training.indx <- sample.split(train$Survived, SplitRatio = .65)
train.train <- subset(train, training.indx == TRUE)
train.test <- subset(train, training.indx == FALSE)

log.model <- glm(Survived ~ . ,data = train.train, family = binomial(link = 'logit'))

summary(log.model)

fitted.probabilities <- predict(log.model,newdata=train.test, type='response')
fitted.results <- ifelse(fitted.probabilities > 0.5,1,0)

classification.error <- mean(fitted.results != train.test$Survived)
print(paste('Accuracy',1-classification.error))

table(train.test$Survived, fitted.probabilities > 0.5)

