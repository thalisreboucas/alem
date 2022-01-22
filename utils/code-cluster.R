###
#
# Code by : Thalis Rebouças
#
###

library(EMCluster,quietly = TRUE)
library(plotly,quietly = TRUE)
library(dplyr,quietly = TRUE)
library(pracma,quietly = TRUE)
library(plyr ,quietly = TRUE)



# 3 clusters e using the and Sepal e Petal column's of the iris dataset
iris.cluster <- EMCluster::simple.init(iris[,1:4] , nclass = 3)
iris.cluster <- EMCluster::shortemcluster(iris[,1:4] , iris.cluster)
iris.ret <- EMCluster::emcluster(iris[,1:4],iris.cluster,assign.class = TRUE)

summary(iris.cluster)
summary(iris.ret)


############
iris.cluster <- Mclust(iris[,1:4], G=3 ) 
summary(iris.cluster)
plot(mc, what = "density")
plot(iris.cluster, what=c("classification"), dimens=c(1,3)) # using 1st and 3rd column of the iris dataset

