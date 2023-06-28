#load packages

library(caret)
library(purrr)
library(imputeMissings)
library(rpart.plot)
###############################################################
#load and attach dataset 

attach(iris)
str(iris)
###############################################################
# check for missing values
map(iris, ~sum(is.na(.)))  # using purrr package

###############################################################
#Data partition
set.seed(100)
Train <- createDataPartition(iris$Species, p=0.8, list=FALSE)
training <- iris[ Train, ]
testing <- iris[ -Train, ]

###############################################################

#create the nb model using train()
Model1 <- train(Species~., data=training, method="rpart")

Model2 <- train(Species~., data=training, method="rpart", parms = list(split = "information"))

# plot decision trees
rpart.plot(Model1$finalModel)
rpart.plot(Model2$finalModel)


# prediction for test data
predSpecies1<-predict(Model1, newdata = testing)
predSpecies2<-predict(Model2, newdata = testing)

##fitness metrics for validation
confusionMatrix(predSpecies1, testing$Species)
confusionMatrix(predSpecies2, testing$Species)
###############################################################
#prediction using Model 2
predict(Model2,data.frame(Sepal.Length=6,Sepal.Width=6,Petal.Length=6,Petal.Width=6),type="prob")