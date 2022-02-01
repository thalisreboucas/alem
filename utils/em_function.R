#
#O código a seguir é baseado em algoritmos observados em 
# Murphy,2012 Probabilistic Machine Learning,
# especificamente, Capítulo 11, seção 4.
#

em_mixture <- function(
  params,
  X,
  clusters = 2,
  tol = .00001,
  maxits   = 100,
  showits  = TRUE
) {
  
  require(mvtnorm , quietly = TRUE)
  N     = nrow(X)
  mu    = params$mu
  var   = params$var
  probs = params$probs
  
  ri = matrix(0, ncol=clusters, nrow=N)       
  ll = 0                                        
  it = 0                                        
  converged = FALSE                             
  
  if (showits)                                  
    cat(paste("Interações do EM:", "\n"))
  
  while (!converged & it < maxits) { 
    probsOld = probs
    llOld = ll
    riOld = ri
    
    ### Parte E
    for (k in 1:clusters){
      ri[,k] = probs[k] * dmvnorm(X, mu[k, ], sigma = var[[k]], log = FALSE)
    }
    
    ri = ri/rowSums(ri)
    
    ### Parte M
    rk = colSums(ri)            
    probs = rk/N
    
    for (k in 1:clusters){
      varmat = matrix(0, ncol = ncol(X), nrow = ncol(X))    
      
      for (i in 1:N){
        varmat = varmat + ri[i,k] * X[i,]%*%t(X[i,])
      }
      
      mu[k,]   = (t(X) %*% ri[,k]) / rk[k]
      var[[k]] =  varmat/rk[k] - mu[k,]%*%t(mu[k,])
      
      ll[k] = -.5*sum( ri[,k] * dmvnorm(X, mu[k,], sigma = var[[k]], log = TRUE) )
    }
    
    ll = sum(ll)
    
    parmlistold =  c(llOld, probsOld)           
    parmlistcurrent = c(ll, probs)              
    it = it + 1
    
    if (showits & it == 1 | it%%5 == 0)         
      cat(paste(format(it), "...", "\n", sep = ""))
    
    converged = min(abs(parmlistold - parmlistcurrent)) <= tol
  }
  
  clust = which(round(ri) == 1, arr.ind = TRUE)        
  clust = clust[order(clust[,1]), 2]          
  
  
  list(
    probs   = probs,
    mu      = mu,
    var     = var,
    resp    = ri,
    cluster = clust,
    ll      = ll
  )
} 
