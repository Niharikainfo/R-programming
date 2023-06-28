#load packages

library(moments)
library(caret)
library(purrr)
library(imputeMissings)
library(naivebayes)

###############################################################

#change the data to simple data frame
A = as.data.frame(AdmissionData)
str(A)

###############################################################
#convert variables
A$admit<-as.factor(A$admit)
A$rank<-as.factor(A$rank)

str(A)    

###############################################################


###############################################################
#Data partition
set.seed(100)
Train <- createDataPartition(A$admit, p=0.8, list=FALSE)
training <- A[ Train, ]
testing <- A[ -Train, ]

###############################################################
#Model1
#create the model using naive bayes

Model1 <- train(admit~., data=training, method="naive_bayes")
# prediction for test data
p1<-predict(Model1, newdata = testing)

#fitness metrics for validation
confusionMatrix(p1, testing$admit,positive="1")

###############################################################
#Model2
#create the  model using logistic regression

Model2 <- train(admit ~ ., data=training, method="glm",family="binomial")

# prediction for test data
p2<-predict(Model2, newdata = testing)

#fitness metrics for validation
confusionMatrix(p2, testing$admit,positive="1")

###############################################################
#prediction
#Model1
predict(Model1,data.frame(gre=400,gpa=5,rank=as.factor(2)),type="prob")
#Model2
predict(Model2,data.frame(gre=400,gpa=5,rank=as.factor(2)),type="prob")

