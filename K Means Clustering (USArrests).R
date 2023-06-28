library(factoextra)
library(dplyr)
library(Hmisc)
library(purrr)

data("USArrests")
US<-USArrests

###############################################################
# check for missing values
map(US, ~sum(is.na(.)))

###############################################################
# standardize
US<-scale(US)
str(US) # structure changes
US<-data.frame(US) # convert back to dataframe

sapply(USArrests,mean)
sapply(USArrests,sd)

sapply(US, mean) # mean of all variables = 0 after scaling
sapply(US, sd) # SD of all variables = 1 after scaling

###############################################################
# k means
set.seed(123)
k2 <- kmeans(US, centers = 2, nstart = 25) #with 2 clusters
k3 <- kmeans(US, centers = 3, nstart = 25) #with 3 clusters
k4 <- kmeans(US, centers = 4, nstart = 25) #with 4 clusters

k2$cluster
k2$centers
k2$size

###############################################################
#visualizing clusters
fviz_cluster(k2, data = US, choose.vars = c("Murder","Assault"))
fviz_cluster(k2, data = US, choose.vars = c("UrbanPop","Assault"))
fviz_cluster(k2, data = US, choose.vars = c("Assault","Rape"))

###############################################################
# adding clusters to the original dataset 
USArrestsNew<-mutate(USArrests,Cluster = k2$cluster)
USArrestsNew

###############################################################
# cluster-wise analysis
###############################################################
# new datasets having cluster numbers
US2<-USArrests
US2$ClusterNo.<-k2$cluster
US2
US2[order(US2$ClusterNo.),]

US3<-USArrests
US3$ClusterNo.<-k3$cluster
US3
US3[order(US3$ClusterNo.),]
###############################################################
#cluster-wise means
tapply(US2$Murder,US2$ClusterNo.,mean)
tapply(US2$Assault,US2$ClusterNo.,mean)
tapply(US2$Rape,US2$ClusterNo.,mean)
tapply(US2$UrbanPop,US2$ClusterNo.,mean)

tapply(US3$Murder,US3$ClusterNo.,mean)
tapply(US3$Assault,US3$ClusterNo.,mean)
tapply(US3$Rape,US3$ClusterNo.,mean)
tapply(US3$UrbanPop,US3$ClusterNo.,mean)

