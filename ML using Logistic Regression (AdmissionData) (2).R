#load packages

library(moments)
library(caret)

###############################################################
#load the dataset 

#change the data to simple data frame
A = as.data.frame(AdmissionData)
str(A)

###############################################################
#convert variables
A$admit<-as.factor(A$admit)
A$rank<-as.factor(A$rank)

str(A)    

###############################################################

#Data partition
set.seed(100)
Train <- createDataPartition(A$admit, p=0.8, list=FALSE)
training <- A[ Train, ]
testing <- A[ -Train, ]

###############################################################
#Model 1
#create the model using logistic regression

Model1 <- train(data=training, admit~., method="glm", family="binomial")
summary(Model1)

# prediction for test data
predAdmit1<-predict(Model1, newdata = testing)

#fitness metrics for validation
confusionMatrix(predAdmit1, testing$admit)
confusionMatrix(predAdmit1, testing$admit, positive = "1") #changing the positive class


###############################################################

#Model 2
#create the model using logistic regression

Model2 <- train(data=training, admit~gpa+rank, method="glm", family="binomial")
summary(Model2)

# prediction for test data
predAdmit2<-predict(Model2, newdata = testing)

#fitness metrics for validation
confusionMatrix(predAdmit2, testing$admit, positive = "1")

###############################################################
#prediction for new data
predict(Model1,data.frame(gre=400, gpa=5,rank=as.factor(2)),type="prob")
predict(Model2,data.frame(gpa=5,rank=as.factor(2)),type="prob")

