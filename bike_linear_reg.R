# linear reg project
# Paul Turner


library(ggplot2)
library(ggthemes)
library(dplyr)
library(corrgram)
library(corrplot)
library(caTools)

# import data

train <- read.csv('input/bike_train.csv')
test <- read.csv('input/bike_test.csv')

head(train)

ggplot(train, aes(temp,count)) + geom_point()

train$datetime <- as.POSIXct(train$datetime)
test$datetime <- as.POSIXct(test$datetime)

ggplot(train, aes(datetime, count)) + geom_point()

num.cols <- sapply(train, is.numeric)
corr.data <- cor(train[,num.cols])
print(corrplot(corr.data))

ggplot(train, aes(factor(season), count)) + geom_boxplot()

train$hour <- sapply( train$datetime , function(x){format(x, "%H")} )
test$hour <- sapply( test$datetime , function(x){format(x, "%H")} )

train.working1 <- filter(train, train$workingday == 1)
train.working0 <- filter(train, train$workingday == 0)

p1 <- ggplot( train.working1, aes(hour, count) ) 
p1 <- p1 + geom_point(position = position_jitter(w=1,h=0), aes(color=temp)) 
p1 <- p1 + scale_color_gradientn(colors = c('purple','blue','green','yellow','orange','red','blaCK'))

p2 <- ggplot( train.working0, aes(hour, count) ) 
p2 <- p2 + geom_point(position = position_jitter(w=1,h=0), aes(color=temp)) 
p2 <- p2 + scale_color_gradientn(colors = c('purple','blue','green','yellow','orange','red','blaCK'))

temp.model <- lm(count ~ temp, data = train)

summary(temp.model)

