
# load the packages
library(dplyr)
library(DescTools)
library(moments)


# Import the datafile BaycoastData and assign it to a local variable
B<-BaycoastData

#Check normality assumption
skewness(B$Price)
kurtosis(B$Price)

#one-sample t-test
t.test(B$Price, mu=300)
t.test(B$Price, mu=900)
t.test(B$Price, mu=400, alternative="greater")
t.test(B$Price, mu=800, alternative="less")

#Exercise
A<-Attrition
t.test(A$Attrition_rate, mu=1.5,alternative="less")

t.test(A$Attrition_rate, conf.level = .95)
t.test(A$Attrition_rate, conf.level = .90)
t.test(A$Attrition_rate, conf.level = .99)
