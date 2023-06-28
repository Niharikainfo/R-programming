#load packages

library(caret)
library(naivebayes)
library(rpart.plot)
###############################################################
#load the dataset 

#change the data to simple data frame
B = as.data.frame(BankData)
str(B)

###############################################################
#convert variables
B$job<-as.factor(B$job)
B$marital<-as.factor(B$marital)
B$education<-as.factor(B$education)
B$default<-as.factor(B$default)
B$housing<-as.factor(B$housing)
B$loan<-as.factor(B$loan)
B$contact<-as.factor(B$contact)
B$y<-as.factor(B$y)

str(B)    
###############################################################

#Data partition
set.seed(100)
Train <- createDataPartition(B$y, p=0.8, list=FALSE)
training <- B[ Train, ]
testing <- B[ -Train, ]

###############################################################
#Model 1
#create the model using logistic regression

Model1 <- train(data=training, y~., method="glm", family="binomial")

# prediction for test data
pred1<-predict(Model1, newdata = testing)

#fitness metrics for validation
confusionMatrix(pred1, testing$y,positive="yes")

###############################################################

#Model 2
#create the model using naive bayes

Model2 <- train(data=training, y~., method="naive_bayes")

# prediction for test data
pred2<-predict(Model2, newdata = testing)

#fitness metrics for validation
confusionMatrix(pred2, testing$y, positive="yes")


###############################################################
###############################################################
#prediction for new data
predict(Model1,data.frame(age=50, job=as.factor("management"),marital=as.factor("married"),education=as.factor("secondary"),
        default=as.factor("yes"),housing=as.factor("yes"), loan=as.factor("yes"), 
        contact=as.factor("cellular"), duration = 200), type="prob")

predict(Model2,data.frame(age=50, job=as.factor("management"),marital=as.factor("married"),education=as.factor("secondary"),
                          default=as.factor("yes"),housing=as.factor("yes"), loan=as.factor("yes"), 
                          contact=as.factor("cellular"), duration = 200), type="prob")
