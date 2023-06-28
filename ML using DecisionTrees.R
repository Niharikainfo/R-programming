
# load packages
library(caret)
library(rpart.plot)

###############################################################
# import cars dataset and convert it to dataframe

cars<-as.data.frame(cars)
str(cars)

###############################################################
# convert all variables into factors
cars[,c(1:7)]<-lapply(cars[,c(1:7)],factor)
str(cars)
###############################################################
# partition the dataset
set.seed(100)
intrain <- createDataPartition(y = cars$car_eval, p= 0.8, list = FALSE)
training <- cars[intrain,]
testing <- cars[-intrain,]

###############################################################
#Model 1: create the model with criterion as information gain
Model1 <- train(car_eval~., data = training, method = "rpart",parms = list(split = "information"))

# plot decision tree
rpart.plot(Model1$finalModel)

# prediction for test data
pred<-predict(Model1, newdata=testing)

#fitness metrics for validation
confusionMatrix(pred, testing$car_eval)

###############################################################
#Model 2: create the model with criterion as gini index
Model2 <- train(car_eval~., data = training, method = "rpart")

# plot decision tree
rpart.plot(Model2$finalModel)

# prediction for test data
pred<-predict(Model2, newdata=testing)

#fitness metrics for validation
confusionMatrix(pred, testing$car_eval)

varImp(Model1)
varImp(Model2)
