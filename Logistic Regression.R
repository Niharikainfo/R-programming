library(regclass)
library(dvmisc)

A<-AdmissionData

#convert categorical variables into factors
A$rank<-as.factor(A$rank)
A$admit<-as.factor(A$admit)

#binomial logistic regression model
M <- glm(data = A, admit ~ gre + gpa + rank, family = "binomial")

#output
summary(M) # provides values in terms of log odds

logit_prob(coef(M)) # from dvmisc package (provides probabilities)

#relevel rank to set the reference category as 4
A$rank<-relevel(A$rank, ref="4")

#Model Accuracy
confusion_matrix(M) # from regclass package

#predicting probability for new data
predict(M, newdata = data.frame(gre=700,gpa=5,rank=as.factor(1))) #log odds
predict(M, newdata = data.frame(gre=700,gpa=5,rank=as.factor(4))) #log odds

predict(M, newdata = data.frame(gre=700,gpa=5,rank=as.factor(1)),type="response")  #probability
predict(M, newdata = data.frame(gre=700,gpa=5,rank=as.factor(4)),type="response")  #probability
