#load packages
library(moments)
library(Hmisc)
library(lm.beta)

B<-BaycoastData
 
###########################################
#Variable conversions

B$Heating<-as.factor(B$Heating)
B$Kitchen<-as.factor(B$Kitchen)
B$Suburb<-as.factor(B$Suburb)
B$Material<-as.factor(B$Material)

###########################################
#Model 1
M1<-lm(data=B,Price~Heating)
summary(M1)
predict(M1,newdata=data.frame(Heating=as.factor(0)))
predict(M1,newdata=data.frame(Heating=as.factor(1)))

#Model 2
M2<-lm(data=B,Price~Suburb)
summary(M2)
predict(M2,newdata=data.frame(Suburb=as.factor(1)))
predict(M2,newdata=data.frame(Suburb=as.factor(2)))
predict(M2,newdata=data.frame(Suburb=as.factor(3)))

#To change the reference category
B$Suburb<-relevel(B$Suburb, ref=2)

#Model 3
M3<-lm(data=B,Price~Heating+Kitchen+Material+Area+LotSize)
summary(M3)
lm.beta(M3)

#Model 4 (with 85.2% accuracy)
B$BayViewsT<-sqrt(B$BayViews)
B$RRT<-sqrt(B$`RentalReturn%`)
M4<-lm(data=B,Price~Heating+Material+Area+LotSize+BayViewsT+RRT)
summary(M4)


