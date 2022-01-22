###
#
# Code by : Thalis Rebouças
#
###

library(dplyr)
library(mclust)
library(knitr)

class.d = diabetes$class
table(class.d)

X = diabetes[,-1]

clPairs(X, class.d)

fit <- Mclust(X)

summary(fit)

plot(fit, what = "BIC")

plot(fit, what = "classification")

