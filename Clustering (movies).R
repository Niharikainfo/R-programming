# Import dataset

# Set seed for code reproducibility 
set.seed(6)

###############################################################
# check for missing values
map(movie, ~sum(is.na(.)))

###############################################################
# standardize
mov<-scale(movie)
str(mov) # structure changes
mov<-data.frame(mov) # convert back to dataframe


###############################################################
# K-means clustering using 3 clusters
k <- kmeans(mov, centers = 3, nstart = 25)
k$size
###############################################################
#visualizing clusters
fviz_cluster(k, data = mov, main="K Means Clustering", 
             choose.vars = c("Horror","Comedy"))
fviz_cluster(k, data = mov, main="K Means Clustering", 
             choose.vars = c("Horror","Action"))
fviz_cluster(k, data = mov, main="K Means Clustering", 
             choose.vars = c("Horror","Romcom"))
fviz_cluster(k, data = mov, main="K Means Clustering", 
             choose.vars = c("Horror","Fantasy"))

###############################################################
# adding clusters to the original dataset 
movie$custNo <- c(1:291)
movie$clusterNo<-k$cluster
movie<-data.frame(movie)

# calculating cluster-wise means of all variables
movie%>%
  group_by(Cluster) %>%
  summarise_all("mean")

###############################################################
# cluster-wise filtering 
movie1<-movie%>%
  filter(Cluster==1)

movie2<-movie%>%
  filter(Cluster==2)

movie3<-movie%>%
  filter(Cluster==3)

movie3$CustNo

###############################################################


