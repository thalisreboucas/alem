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
    cluster = clust ,
    interactions = it 
  )
  
  out #fim =)
  
} 
