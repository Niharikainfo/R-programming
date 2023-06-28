# load the packages
library(dplyr)
library(DescTools)
library(moments)
library(ggplot2)


#Example 1       #######################################################################

B<-as.data.frame(BaycoastData)

#Example 1
#Normality assumption
B %>%
  group_by(AirCon) %>%
  summarise(shapiro.test(Price)$statistic,shapiro.test(Price)$p.value)

#Levene's F Test
# the independent variable should be converted to a factor (i.e. categorical variable) before applying levene's test
LeveneTest(B$Price,as.factor(B$AirCon))


###################################################################################################################
# Example 2

#Normality assumption
B %>%
  group_by(Kitchen) %>%
  summarise(shapiro.test(Price)$statistic,shapiro.test(Price)$p.value)


#Levene's F Test
# the independent variable should be converted to a factor (i.e. categorical variable) before applying levene's test
LeveneTest(B$Price,as.factor(B$Kitchen))

###################################################################################################################
# Exercise
A<-Attrition
A %>%
  group_by(Gender) %>%
  summarise(shapiro.test(Attrition_rate)$statistic,shapiro.test(Attrition_rate)$p.value)

A %>%
  group_by(Gender) %>%
  summarise(skewness(Attrition_rate),kurtosis(Attrition_rate))

boxplot(A$Attrition_rate ~ A$Gender)

qplot(sample=A$Attrition_rate,color=as.factor(A$Gender))

# F-test can't be applied as Attrition rate is not normally distributed in male and female categories