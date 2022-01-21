
gen_dat <- function(beta, n) {
  X <- rnorm(n = n, mean = 0, sd = 1)
  Y <- X * beta + rnorm(n = n, mean = 0, sd = sqrt(1 - beta^2))
  bind_cols(X = X, Y = Y)
}

get_bain <- function(estimate, se, n, hypotheses){
  names(estimate) <- "beta"
  bain(estimate,
       hypotheses,
       n = n,
       Sigma = se^2) 
}

aggPMP <- function(PMP){
  cumprod(PMP) / (cumprod(PMP) + cumprod(1-PMP))   
}


best_hypothesis <- function(PMP){    
  ifelse(aggPMP(PMP) > .66, 1, 0)
}


fixed_beta <- function(estimate, se){            
  cumsum( (1/(se^2)) * estimate) / cumsum(1/(se^2))   
}


fixed_lower <- function(se, fixed_beta){
  var_meta <- 1 / cumsum( 1/(se^2) )
  fixed_beta - 1.96*sqrt(var_meta)    
}


random_beta <- function(estimate, se, tau){            
  cumsum( (1/(se^2 + tau^2)) * estimate) / cumsum(1/(se^2 + tau^2))   
}


random_lower <- function(se, random_beta, tau){
  var_meta <- 1 / cumsum(1/(se^2 + tau^2))
  random_beta - 1.96*sqrt(var_meta)    
}


rejectH0 <- function(estimate, se, tau, method = c("fixed", "random")){
  if(method == "fixed"){
    ifelse(fixed_lower(se, fixed_beta(estimate, se)) > 0, 1, 0)
  }
  else if(method == "random"){
    ifelse(random_lower(se, random_beta(estimate, se, tau), tau) > 0, 1, 0)
  }
}

