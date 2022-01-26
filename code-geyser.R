library(dplyr , quietly = TRUE)
library(tidyr , quietly = TRUE)
library(Lahman , quietly = TRUE)
library(ggplot2 , quietly = TRUE)
library(tibble ,quietly = TRUE)
library(plotly,quietly = TRUE)
library(purrr , quietly = TRUE)
library(VGAM,quietly = TRUE)


# Fazendo duas matrizes para facilitar
eruptions  = as.matrix(faithful[, 1, drop = FALSE])
wait.times = as.matrix(faithful[, 2, drop = FALSE])


# Aplicação de mistura de EM

start_values_1 = list(mu = c(2, 5),
                      var = c(1, 1),
                      probs = c(.5, .5))

start_values_2 = list(mu = c(50, 90),
                      var = c(1, 15),
                      probs = c(.5, .5)) 


mix_erupt   = em_mixture(start_values_1, X = eruptions,  tol = 1e-8)
mix_waiting = em_mixture(start_values_2, X = wait.times, tol = 1e-8)


#Pronto ,agora vamos comparar os valores de algoritmo EM com outro packger 

library(flexmix, quietly = TRUE)

flex_erupt = flexmix(eruptions ~ 1,
                     k = 2,
                     control = list(tolerance = 1e-8, iter.max = 100))

flex_wait = flexmix(wait.times ~ 1,
                    k = 2,
                    control = list(tolerance = 1e-8, iter.max = 100))


# tabela de Comparação erupção
mean_var = rbind(mix_erupt$mu, sqrt(mix_erupt$var))
rownames(mean_var) = c('Média', 'Variância')
colnames(mean_var) = c('cluster 1', 'cluster 2')

mean_var_flex = parameters(flex_erupt)
rownames(mean_var_flex) = c('means', 'variances')
colnames(mean_var_flex) = c('cluster 1 flex', 'cluster 2 flex')


prob_membership      = mix_erupt$probs
prob_membership_flex = flex_erupt@size / sum(flex_erupt@size)

list(
  params = cbind(mean_var, mean_var_flex),
  clusterpobs = cbind(prob_membership, prob_membership_flex)
)

# Tabela de comparação do tempo de espera
mean_var = rbind(mix_waiting$mu, sqrt(mix_waiting$var))
rownames(mean_var) = c('means', 'variances')
colnames(mean_var) = c('cluster 1', 'cluster 2')


mean_var_flex = parameters(flex_wait)
rownames(mean_var_flex) = c('means', 'variances')
colnames(mean_var_flex) = c('cluster 1 flex', 'cluster 2 flex')

prob_membership      = mix_waiting$probs
prob_membership_flex = flex_wait@size / sum(flex_wait@size)

list(
  params = cbind(mean_var, mean_var_flex),
  clusterpobs = cbind(prob_membership, prob_membership_flex)
)


# Some plots
library(ggplot2)

g1 <- qplot(x = eruptions, y = waiting, data = faithful)

ggplot(aes(x = eruptions, y = waiting), data = faithful) +
  geom_point(aes(color = factor(mix_waiting$cluster)))

ggplot(aes(x = eruptions, y = waiting), data = faithful) +
  geom_point(aes(color = mix_waiting$resp[, 1]))

