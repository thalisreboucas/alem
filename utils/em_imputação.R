###
#
# Code by : Thalis Rebouças
#
###

# Library's nescessarias para essa parte de imputação

library(Amelia , quietly = TRUE) 
library(missForest , quietly = TRUE)

# A Amelia usa o EM para inserir os dados.

# Abrindo o dataset
data.iris <- data("iris")

# Colocando NA no dataset
iris.na <- missForest::prodNA(iris, noNA = 0.1) 

# Treinando o Algoritimo para reconhecer o dataset para modelar a imputação 
amelia_fit <- Amelia::amelia(iris.na, 
                             m=5, # Numero de interações/datasets
                             parallel = "multicore",
                             noms = "Species")

# Vendo o que foi feito
summary(amelia_fit)

plot(amelia_fit)

hist(iris$Sepal.Width)

hist(iris.na$Sepal.Width)
hist(amelia_fit$imputations[[2]]$Sepal.Width)
