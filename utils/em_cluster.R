###
#
# Code by : Thalis Rebouças
#
###

# Cluster com EM
# Library's Nescessarias para essa aplicação.
library(dplyr,quietly = TRUE)
library(purrr ,quietly = TRUE)
library(mclust, quietly = TRUE)


iris2 = iris %>% 
  dplyr::select(-Species) %>% 
  as.matrix()

mustart = iris %>% 
  dplyr::group_by(Species) %>% 
  dplyr::summarise(across(.fns = function(x) mean(x) + runif(1, 0, .5))) %>% 
  dplyr::select(-Species) %>% 
  as.matrix()


# usando purrr::map para fazer o mclust::map masking
covstart = iris %>% 
  split(.$Species) %>% 
  purrr::map(select, -Species) %>% 
  purrr::map(function(x) cov(x) + diag(runif(4, 0, .5))) 

probs = c(.1, .2, .7)

starts = list(mu = mustart, var = covstart, probs = probs)

em_iris = em_mixture(
  params = starts,
  X = iris2,
  clusters = 3,
  tol      = 1e-8,
  maxits   = 1500,
  showits  = T
)

table(em_iris$cluster, iris$Species)

library(ggplot2,quietly = TRUE)
library(plotly,quietly = TRUE)

ggplot2::ggplot(data = iris,aes(x = Petal.Length, y = Sepal.Length))+
  geom_point() +
  theme_minimal()

# consegue identificar  ? 

# Vamos ver os cluster's

g1 <- ggplot2::ggplot(data = iris,aes(x = Petal.Length, y = Sepal.Length))+
  geom_point(aes(color = factor(em_iris$cluster)) , alpha = .7) +  
  geom_density2d(alpha = .2) + 
  scale_color_brewer(palette="Dark2") + 
  labs(colour = "cluster" , x = "Comprimento da Petala" , y= "Comprimento da Sepala")+
  theme_minimal()

# Visualizando 
plotly::ggplotly(g1)

# Qual a probilidade de uma especie está no cluster 2 ?

g2 <- ggplot2::ggplot(data = iris,aes(x = Petal.Length, y = Sepal.Length)) +
  geom_density2d(alpha = .2) + 
  geom_point(aes(color = em_iris$resp[, 2]) , alpha = .7)+ 
  scale_color_gradient(low="blue", high="red") + 
  labs(colour = "Probabilidade de estar no cluster 2" , x = "Comprimento da Petala" , y= "Comprimento da Sepala")+
  theme_minimal()

# visualizando
plotly::ggplotly(g2)

# Modo 2
############
mclust_iris = mclust::Mclust(iris[,1:4], 3)
table(mclust_iris$classification, iris$Species)

plot(mclust_iris, what = "density")
plot(mclust_iris, what=c("classification"), dimens=c(1,3)) 
