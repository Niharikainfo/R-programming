# install the packages either by using the following commands or by using Install button from the bottom right corner
install.packages("dplyr")
install.packages("DescTools")
install.packages("moments")

# load the packages
library(dplyr)
library(DescTools)
library(moments)
library(readxl)

# Import the datafile BaycoastData and assign it to a local variable
B<-BaycoastData

save(BaycoastData,file="B.RData")

# Conversions
B<-as.data.frame(B)
B$Material<-as.factor(B$Material)
B$Style<-as.factor(B$Style)
B$Kitchen<-as.factor(B$Kitchen)
B$Heating<-as.factor(B$Heating)
B$AirCon<-as.factor(B$AirCon)
B$Suburb<-as.factor(B$Suburb)
B$Condition<-as.factor(B$Condition)
B$RentalStatus<-as.factor(B$RentalStatus)

str(B)

# Frequency distribution for Rooms
table(B$Rooms) #frequency distribution
Fd_Rooms<-as.data.frame(table(B$Rooms)) #convert to data frame
Fd_Rooms
names(Fd_Rooms)<-c("No. of Rooms","No. of Houses") #change column headings
Fd_Rooms


#pie chart for Storeys
pie(B$Storeys) #doesn't give the required chart
Fd_Storeys<-table(B$Storeys)
pie(Fd_Storeys) #doesn't provide sufficient information
pie(Fd_Storeys,labels=Fd_Storeys,col=c("blue","red"))
legend("topright", c("Single Storey","Double Storey"),cex = 0.5, fill = c("blue","red"))

#Barplot for Rooms
barplot(table(B$Rooms))
barplot(table(B$Rooms),main="Barpot for Rooms",xlab="No. of Rooms",ylab="No. of houses")

#histogram for Price
hist(B$Price)
hist(B$Price,main="Histogram for Price",xlab="Price",ylab="No. of houses")


#Descriptive Statistical Measures for Price
mean(B$Price)
sd(B$Price)
var(B$Price)
median(B$Price)
Mode(B$Price)
skewness(B$Price)
kurtosis(B$Price)

#all measures for Price in one go
x<-B %>%
  summarize(mean(Price),sd(Price),median(Price), Mode(Price), skewness(Price),kurtosis(Price))

as.data.frame(x)

#measures for Price, Lotsize, Age and Area together
sapply(B[c(2,4,5,6)],mean) 
sapply(B[c(2,4,5,6)],sd) 


#Descriptive statistics of a variable for different groups (e.g. Descriptive statistics of Price for different groups of Material)
options(digits=5) # to set the number of digits to be printed in the output

x<-BaycoastData %>%
  group_by(Material) %>%
  summarise(mean(Price),sd(Price),skewness(Price),kurtosis(Price))
x
as.data.frame(x) # to print the output as a dataframe


