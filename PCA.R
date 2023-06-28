
library(purrr)
data("USArrests")

###############################################################
# check for missing values
map(USArrests, ~sum(is.na(.)))

###############################################################
# PCA
mypca<-prcomp(USArrests,scale=TRUE)
mypca
summary(mypca) #importance of principal components



