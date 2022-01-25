library(dplyr , quietly = TRUE)
library(tidyr , quietly = TRUE)
library(Lahman , quietly = TRUE)
library(ggplot2 , quietly = TRUE)
library(tibble ,quietly = TRUE)
library(plotly,quietly = TRUE)
library(purrr , quietly = TRUE)
library(VGAM,quietly = TRUE)

# Abrindo o banco de dados
df.geyser <- data(faithful)

# Fazendo duas matrizes para facilitar
eruptions  = as.matrix(faithful[, 1, drop = FALSE])
wait.times = as.matrix(faithful[, 2, drop = FALSE])

# Fazendo a Function "EM" para modelar esses dados

em_mixture <- function(
  params,
  X,
  clusters = 2,
  tol = .00001,
  maxits  = 100,
  showits = TRUE
) {
  
  # Argumentos são parâmetros iniciais (médias, covariâncias, probabilidade de cluster),
  # Vetor dados, número de clusters desejados, tolerância, iterações máximas e se
  # para mostrar as iterações
  
  # Começando os pontos
  N     = nrow(X)
  nams  = names(params)
  mu    = params$mu
  var   = params$var
  probs = params$probs
  
  # Outras inicializações
  # inicializa as 'responsabilidades' do cluster, ou seja, a probabilidade do cluster
  # adesão para cada observação i
  ri = matrix(0, ncol = clusters, nrow = N) 
  it = 0
  converged = FALSE
  
  # Mostrar interações
  if (showits)                                  
    cat(paste("Interações do EM:", "\n"))
  
  while ((!converged) & (it < maxits)) { 
    probsOld = probs
    muOld = mu
    varOld = var
    riOld = ri
    
    # Parte E do algoritmo
    # Computando as responsabilidades
    for (k in 1:clusters){
      ri[, k] = probs[k] * dnorm(X, mu[k], sd = sqrt(var[k]), log = FALSE)
    }
    
    ri = ri/rowSums(ri)
    
    # Parte M do algoritmo
    rk = colSums(ri)           # rk é o tamanho médio ponderado da associação do cluster
    probs = rk/N
    mu = (t(X) %*% ri) / rk    
    var = (t(X^2) %*% ri) / rk - mu^2
    
    # poderia fazer mu e var via verossimilhança de log aqui, mas isso é mais direto
    
    parmlistold     = rbind(probsOld, muOld, varOld)
    parmlistcurrent = rbind(probs, mu, var)
    
    it = it + 1
    
    # se showits verdadeiro, & é =1 ou divisível por 5 imprime mensagem
    if (showits & it == 1 | it%%5 == 0)        
      cat(paste(format(it), "...", "\n", sep = ""))
    
    converged = max(abs(parmlistold - parmlistcurrent)) <= tol
  }
  
  clust = which(round(ri) == 1, arr.ind = TRUE) # cria associação de cluster
  clust = clust[order(clust[, 1]), 2]           # ordena de acordo com a linha em vez do cluster
  
  out = list(
    probs   = probs,
    mu      = mu,
    var     = var,
    resp    = ri,
    cluster = clust
  )
  
  out #fim =)
  
} 


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

# Grafico 

data(geyser, package = 'MASS')

geyser = data.frame(duration = geyser$duration[-299], waiting = geyser$waiting[-1])

# compare to faithful
library(patchwork)

ggplot2::qplot(data = faithful, x = eruptions, y = waiting, alpha = I(.25)) 
ggplot2::qplot(data = geyser, x = duration,  y = waiting, alpha = I(.25))

X3 = matrix(geyser[,1]) 
X4 = matrix(geyser[,2])


# MASS version
test3 = em_mixture(start_values_1, X = X3, tol = 1e-8)
test4 = em_mixture(start_values_2, X = X4, tol = 1e-8)

flexmod3 = flexmix(X3 ~ 1,
                   k = 2,
                   control = list(tolerance = 1e-8, iter.max = 100))
flexmod4 = flexmix(X4 ~ 1,
                   k = 2,
                   control = list(tolerance = 1e-8, iter.max = 100))

# note variability differences compared to faithful dataset
# Eruptions/Duration
mean_var = rbind(test3$mu, sqrt(test3$var))
rownames(mean_var) = c('means', 'variances')

mean_var_flex = parameters(flexmod3)
rownames(mean_var_flex) = c('means', 'variances')

prob_membership = test3$probs
prob_membership_flex = flexmod3@size / sum(flexmod3@size)

list(
  params = cbind(mean_var, mean_var_flex),
  clusterpobs = cbind(prob_membership, prob_membership_flex)
)

# Waiting
mean_var = rbind(test4$mu, sqrt(test4$var))
rownames(mean_var) = c('means', 'variances')

mean_var_flex = parameters(flexmod4)
rownames(mean_var_flex) = c('means', 'variances')

prob_membership = test4$probs
prob_membership_flex = flexmod4@size / sum(flexmod4@size)

list(
  params = cbind(mean_var, mean_var_flex),
  clusterpobs = cbind(prob_membership, prob_membership_flex)
)

# Some plots
library(ggplot2)

qplot(x = eruptions, y = waiting, data = faithful)

ggplot(aes(x = eruptions, y = waiting), data = faithful) +
  geom_point(aes(color = factor(mix_waiting$cluster)))

ggplot(aes(x = eruptions, y = waiting), data = faithful) +
  geom_point(aes(color = mix_waiting$resp[, 1]))

