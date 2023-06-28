# load the packages
library(dplyr)
library(DescTools)
library(moments)
library(ggplot2)
library(rstatix)

B<-BaycoastData

###################################################################################
# Example 1
###################################################################################

#Normality assumption
B %>%
  group_by(Material) %>%
  summarise(skewness(Price),kurtosis(Price))

#Homogeneity of variances assumption (Levene's F Test)
LeveneTest(B$Price,as.factor(B$Material))

#Anova Test
x<-aov(B$Price~ as.factor(B$Material))# the Iv should be converted to a categorical variable
summary(x)

###################################################################################
# If homogeneity of variances is not found then Welch ANOVA is applied
welch_anova_test(data=B,Price~ as.factor(Material)) # available in rstatix package

###################################################################################


###################################################################################
#Example 2
###################################################################################

#Normality assumption
B %>%
  group_by(Condition) %>%
  summarise(skewness(Price),kurtosis(Price))

#Homogeneity of variances assumption (Levene's F Test)
LeveneTest(B$Price,as.factor(B$Condition))

#Anova Test
y<-aov(B$Price~ as.factor(B$Condition))# the Iv should be converted to a categorical variable
summary(y)
tapply(B$Price,B$Condition,mean)



