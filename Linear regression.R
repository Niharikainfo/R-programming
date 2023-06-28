#load packages
library(moments)
library(Hmisc)
library(lm.beta)

B<-BaycoastData

###########################################################
#Example 1
###########################################################

#normality
skewness(B$Price)
kurtosis(B$Price)

skewness(B$Age)
kurtosis(B$Age)

skewness(B$BayViews)
kurtosis(B$BayViews)

B$BayViewsT<-sqrt(B$BayViews) #transformation of BayViews
skewness(B$BayViewsT)
kurtosis(B$BayViewsT)

#outliers
boxplot(B$Price)
boxplot(B$Age)
boxplot(B$BayViewsT)

#scatter plots and correlation between DV and IVs
pairs(B$Price~B$BayViewsT+B$Age)
cor.test(B$Price,B$Age)
cor.test(B$Price,B$BayViewsT)

#multicollinearity
cor(B$BayViewsT,B$Age)

#regression model
M<-lm(data=B, Price~BayViewsT+Age)

summary(M) #output of regression model
lm.beta(M) #standardized coefficients

M$fitted.values # to see the predicted values

plot(B$Price,M$fitted.values) # to plot actual vs. predicted values

# predict price for Age=10 and BayViews=0.5
predict(M,newdata=data.frame(Age=10,BayViewsT=sqrt(0.5)))

########################################################

###########################################################
#Example 2
###########################################################
#outliers
boxplot(B$Price)
boxplot(B$Age)
boxplot(B$LotSize)
boxplot(B$Area)
boxplot(B$`RentalReturn%`)

#normality
skewness(B$Price)
skewness(B$Age)
skewness(B$LotSize)
skewness(B$Area)
skewness(B$`RentalReturn%`)

kurtosis(B$Price)
kurtosis(B$Age)
kurtosis(B$LotSize)
kurtosis(B$Area)
kurtosis(B$`RentalReturn%`)

#correlations
x<-B[,c(2,4,5,6,17)]
rcorr(as.matrix(x))

# Regression Models
# Model 1
Model1<-lm(data=B, Price~Area+LotSize+Age+`RentalReturn%`)

summary(Model1) 

lm.beta(Model1)

# Model 2
Model2<-lm(data=B, Price~Area+LotSize+`RentalReturn%`)

summary(Model2) 

lm.beta(Model2)

# Model 3
B$RRT<-sqrt(B$`RentalReturn%`)
Model3<-lm(data=B, Price~Area+LotSize+RRT)

summary(Model3) 

lm.beta(Model3)

# predict price using Model 3 for Area=100, Lot Size=100, Rental return=5
predict(Model3,newdata=data.frame(Area=100,LotSize=100,Ag=10,RRT=sqrt(5)))

