#-------------------------------- Expectation  -----------------------------

expectation <- function(sample,p,a,b)
{
  p_expectation <- (p*dbinom(sample,1,a)) / ( p*dbinom(sample,1,a) + (1-p)*dbinom(sample,1,b) )
  return(p_expectation)
}


#-------------------------------- Maximization   --------------------------------

maximization <- function(sample,epart){
  
  # estimate p
  
  p_temp <- mean(epart)
  
  # estimate a and b
  
  a_temp <- sum(sample*epart) / sum(epart)
  b_temp <- sum(sample*(1-epart)) / sum(1-epart)
  
  list(p_temp,a_temp,b_temp)   
}



#---------------------------- Expectation Maximization Algorithm  -------------------------

EM <- function(sample,p_inits,a_inits,b_inits,maxit=1000,tol=1e-6)
{
  # Estimation of parameter(Initial)
  flag <- 0
  p_cur <- p_inits; a_cur <- a_inits; b_cur <- b_inits
  
  # Iterate between expectation and maximization parts
  
  for(i in 1:maxit){
    cur <- c(p_cur,a_cur,b_cur)
    new <- maximization(sample,expectation(sample, p_cur, a_cur, b_cur))
    p_new <- new[[1]]; a_new <- new[[2]]; b_new <- new[[3]]
    new_step <- c(p_new,a_new,b_new)
    
    # Stop iteration if the difference between the current and new estimates is less than a tolerance level
    if( all(abs(cur - new_step) < tol) ){ flag <- 1; break}
    
    
    # Otherwise continue iteration
    p_cur <- p_new; a_cur <- a_new; b_cur <- b_new
  }
  if(!flag) warning("Didn't converge\n")
  
  list(p_cur, a_cur, b_cur)
}



#------------------------------ Calculating Information matrix ----------------------------

Info.Mat.function <- function(sample, p.est, a.est, b.est){
  expectation.est <- expectation(sample,p.est, a.est, b.est)
  info.mat <- matrix(rep(0,9),3,3)
  info.mat[1,1] <- - sum(expectation.est)/(p.est^2)  - sum((1-expectation.est))/((1-p.est)^2) 
  info.mat[2,2] <- - sum(expectation.est*sample)/(a.est^2) - sum(expectation.est*(1-sample))/((1-a.est)^2)
  info.mat[3,3] <- - sum((1-expectation.est)*sample)/(b.est^2) - sum((1-expectation.est)*(1-sample))/((1-b.est)^2)
  return(-info.mat)
}


#------------------ Now Generate sample data --------------------------------

n <- 10000
p_true <- 0.85 # prob of using first coin
a_true <-  0.50 # the first coin has P(heads) = 0.50
b_true <-  0.70 # the second coin has P(heads) = 0.70
true <- c(p_true,a_true,b_true)
u <- ifelse(runif(n)<p_true, rbinom(n,1,a_true),rbinom(n,1,b_true))


# Set parameter estimates
p_init = 0.70; a_init = 0.70; b_init = 0.60


#--------------------Return EM Algorithm function and calculate Confidence Interval-----------------------------

output <- EM(u,p_init,a_init,b_init)


# Confidence Intervals
sd.out <- sqrt(diag(solve(Info.Mat.function(u,output[[1]],output[[2]],output[[3]]))))
data.frame("Truth" = true, "EM Estimate" = unlist(output), "Lower CI" = unlist(output) - qnorm(.975)*sd.out, "Upper CI" = unlist(output) + qnorm(.975)*sd.out)