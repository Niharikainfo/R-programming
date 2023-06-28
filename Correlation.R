# load the packages
library(dplyr)
library(DescTools)
library(moments)
library(ggplot2)
library(Hmisc)


B<-BaycoastData

#Example 1          ##########################################################

#scatter plot
plot(B$Price,B$BayViews)

pairs(B$Price~B$LotSize+B$Area+B$BayViews+B$Age)

#Example 2          ##########################################################
#testing normality
skewness(B[c(2,4,5,6,14)])
kurtosis(B[c(2,4,5,6,14)])

#correlation coefficient
cor(B$Price,B$Age,method="pearson")
cor(B$Price,B$Age)
cor(B$Price,B$LotSize)
cor(B$Price,B$Area)
cor(B$Price,B$BayViews,method = "kendall")

#to return a correlation matrix for the selected variables
x<-B[,c(2,4,5,6)]
cor(x)


#Example 3          ##########################################################

cor.test(B$Price,B$Age)
rcorr(as.matrix(x))
