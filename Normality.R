
install.packages("ggplot2")

# load the packages
library(dplyr)
library(DescTools)
library(moments)
library(ggplot2)


#Example 1       #######################################################################

B<-as.data.frame(BaycoastData)

#QQ Plot for Price
qplot(sample=B$Price)
qplot(sample=B$Price,main="QQ plot", xlab="Expected Normal",ylab="Observed value of Price")

#Boxplot for Price
boxplot(B$Price)

#skewness and kurtosis of Price
x<-BaycoastData %>%
  summarise(skewness(Price),kurtosis(Price))
as.data.frame(x) # to print the output as a dataframe so that the correct numeric values are printed

#Shapiro Wilk test for normality of price
shapiro.test(B$Price)

#Example 2        ####################################################################

#QQ Plot
B$Material<-as.factor(B$Material) # it is necessary to convert Material into a factor i.e. categorical variable before applying qplot()
qplot(sample=B$Price,color=B$Material)

#Boxplot
boxplot(B$Price ~ B$Material)

#skewness and kurtosis of price for different groups of Material
x<-B %>%
  group_by(Material) %>%
  summarise(skewness(Price),kurtosis(Price))
as.data.frame(x) # to print the output as a dataframe


#Shapiro Wilk test for normality of price for different groups of Material
B %>%
  group_by(Material) %>%
  summarise(shapiro.test(Price)$statistic,shapiro.test(Price)$p.value)

B %>%
  group_by(Material) %>%
  summarise(shapiro.test(Price)$p.value)

#Example 3        ####################################################################

#QQ Plot of price for different groups of Storeys
B$Storeys<-as.factor(B$Storeys)
qplot(sample=B$Price,color=B$Storeys)

#skewness and kurtosis of price for different groups of Storeys
x<-B %>%
  group_by(Storeys) %>%
  summarise(skewness(Price),kurtosis(Price))
as.data.frame(x) # to print the output as a dataframe

#test for normality of price for different groups of Storeys
B %>%
  group_by(Storeys) %>%
  summarise(shapiro.test(Price)$statistic,shapiro.test(Price)$p.value)