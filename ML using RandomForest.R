
# load packages
library(caret)
library(rpart.plot)
library(randomForest)
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
#Model creation
Model1 <- train(car_eval~., data = training, method = "rpart",parms = list(split = "information"))
Model2 <- train(car_eval~., data = training, method = "rpart")
Model3 <- train(car_eval~., data = training, method = "rf")

#Model 2 Tree
rpart.plot(Model2$finalModel)

#Model3 output
Model3
Model3$finalModel
varImp(Model3)


# prediction for test data
p1<-predict(Model1, newdata=testing)
p2<-predict(Model2, newdata=testing)
p3<-predict(Model3, newdata=testing)

#fitness metrics for validation
caret::confusionMatrix(p1, testing$car_eval)
caret::confusionMatrix(p2, testing$car_eval)
caret::confusionMatrix(p3, testing$car_eval)
###############################################################


