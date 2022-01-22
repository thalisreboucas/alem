###
#
# Code by : Thalis Rebouças
#
###


library(Amelia)
library(missForest)

# Library used for missing data.

# this library have two way if boostrap to inpunt date and one way is EM.

data.iris <- data("iris")

iris.na <- prodNA(iris, noNA = 0.2) 

amelia_fit <- amelia(iris.na, m=5, parallel = "multicore", noms = "Species")

summary(amelia_fit)

plot(amelia_fit)

hist(iris$Sepal.Width)
hist(iris.na$Sepal.Width)
hist(amelia_fit$imputations[[5]]$Sepal.Width)
