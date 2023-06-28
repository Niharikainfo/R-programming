#load packages

library(caret)
library(naivebayes)
###############################################################

str(iris)

###############################################################
#Data partition
set.seed(100)
Train <- createDataPartition(iris$Species, p=0.8, list=FALSE)
training <- iris[ Train, ]
testing <- iris[ -Train, ]

###############################################################

#create the nb model using train()

M <- train(Species~., data=training, method="naive_bayes")

# prediction for test data
p<-predict(M, newdata = testing)

##fitness metrics for validation
confusionMatrix(p, testing$Species)

###############################################################
#prediction
predict(M,data.frame(Sepal.Length=2,Sepal.Width=2,Petal.Length=2,Petal.Width=2),type="prob")
