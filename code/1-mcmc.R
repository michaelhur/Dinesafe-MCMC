mcmc_chain <- function(dat, reps, n.chains=3, ...) {
  res <- list(NULL)
  for (i in 1:n.chains) {
    starts <- rnorm(4)
    res[[i]] <- run_mcmc(dat, alpha.init = starts[1], lambda.init = starts[2], 
                         beta.init = starts[3], delta.init = starts[4], n.reps=reps,...)
  }
  return(res)
}

#MCMC workhorse function

# dat should be a data frame with:
# 1. "Inspection ID" - unique identifier of each inpsection
# 2. "Establishment ID" - unique identifier of each establishment
# 3. "IsED" - indicator for whether the establishment is in the Entertainment District
# 4. "Total" - total number of infractions
# There are other columns for the breakdowns of infraction types, but will not be used here.

# (1) 'url' - unique identifier for each game
# (2) 'batter_name' - unique identifier of players
# (3) 'home' - indicator for home vs. away game)
# (4) 'y' - number of occurences for outcome of interest
# (5) 'n' - number of atbats

# i.e. "compliance.csv", the per-inspection data
# i.e. "inspection_df"

# dat <- read.csv('./data/compliance.csv')
# dat <- dat[order(dat$ESTABLISHMENT_ID, dat$INSPECTION_ID),]
# 
# N <- length(dat$Total)
# n.establishment <- length(unique(dat$ESTABLISHMENT_ID))
# ni <- as.numeric(table(dat$ESTABLISHMENT_ID))
# zero <- dat$Total == 0                        #indicates whether there was a 'success'
# cum.ni <- cumsum(ni)                      #useful for computing likelihood at the player level

# dat <- read.csv('./data/infraction.csv')
# dat <- dat[order(dat$ESTABLISHMENT_ID),]
# 
# source('./code-v3 reflected poisson/sdmean.R')
# 
# N <- length(dat$Total)
# n.establishment <- length(unique(dat$ESTABLISHMENT_ID)) ## We can just do nrow, but let's be safe
# ni <- dat$Inspection
# zero <- dat$Total == 0
# cum.ni <- cumsum(ni)

dat <- read.csv('./data/compliance.csv')
dat <- dat[order(dat$ESTABLISHMENT_ID, dat$INSPECTION_ID),]

source('./code/sdmean.R')

N <- length(dat$Total)
n.establishment <- length(unique(dat$ESTABLISHMENT_ID))
ni <- as.numeric(table(dat$ESTABLISHMENT_ID))
zero <- dat$Total == 0                        #indicates whether there was a 'success'
cum.ni <- cumsum(ni)                      #useful for computing likelihood at the player level


run_mcmc <- function(dat, alpha.init = 0, lambda.init = 0, beta.init = 0, delta.init = 0,
                n.reps = 3000, adapt = 500, tune = TRUE){
  require(MASS)
  delta_keep <- beta_keep <- lambda_keep <- alpha_keep <- matrix(numeric(n.reps*n.establishment), 
                                                                 nrow=n.reps, ncol=n.establishment)
  #impose starting values
  alpha_keep[1,] <- alpha <- if (length(alpha.init)==1) rep(alpha.init, n.establishment) else alpha.init
  lambda_keep[1,] <- lambda <- if (length(lambda.init)==1) rep(lambda.init, n.establishment) else lambda.init
  beta_keep[1,] <- beta <- if (length(beta.init)==1) rep(beta.init, n.establishment) else beta.init
  delta_keep[1,] <- delta <- if (length(delta.init)==1) rep(delta.init, n.establishment) else delta.init
  
  #calculate log-likelihood for the starting value
  t <- exp(rep(beta, ni) + dat$IsED * (rep(alpha, ni) - rep(beta, ni)))  #save repeated calc for theta
  theta <- t/(1 + t)
  
  #sample latent variable
  xs <- rbinom(N, size=1, prob=theta) #Shuld it be xs <- rpois(N, size=1, prob=theta)
  
  psi <- exp(rep(delta, ni) + dat$IsED *(rep(lambda, ni) - rep(delta, ni)))  #save repeated calc for lambda
  # psi <- exp(rep(delta, ni) + dat$IsED *(rep(lambda, ni) - rep(delta, ni)) - log(ni))  #save repeated calc for lambda

  #loglik <- sum(ifelse(xs, dat$Total*log(psi) + (dat$n - dat$Total)*log(1-psi), 0))
  loglik <- sum(ifelse(xs, dat$Total*log(psi) - psi, 0))             
  loglik_keep <- numeric(n.reps)
  loglik_keep[1] <- loglik
  
  #jumping std devs for metropolis (include these as an option?)
  jump_beta <- rep(0.10, n.establishment)
  jump_delta <- rep(0.10, n.establishment)
  jump_alpha <- rep(0.10, n.establishment)
  jump_lambda <- rep(0.10, n.establishment)
  A <- 1.1 
  B <- 1.1^(-44/56)
  for (k in 2:n.reps) {
    if (k%%100==0) cat(k, "\n")
    
    #metropolis step for \beta
    stuff <- metropolis(beta=beta, alpha=alpha, delta=delta, lambda=lambda, xs=xs,
                        jump_sd=jump_beta, param="beta")
    beta_keep[k,] <- beta <- stuff$beta
    if (tune & k < adapt) jump_beta <- ifelse(stuff$ratio > 0.44, jump_beta*A, jump_beta*B)
    
    #metropolis step for \alpha
    stuff <- metropolis(beta=beta, alpha=alpha, delta=delta, lambda=lambda,xs=xs,
                        jump_sd=jump_alpha, param="alpha")
    alpha_keep[k,] <- alpha <- stuff$alpha
    if (tune & k < adapt) jump_alpha <- ifelse(stuff$ratio > 0.44, jump_alpha*A, jump_alpha*B)
    
    t <- exp(rep(beta, ni) + dat$IsED * (rep(alpha, ni) - rep(beta, ni)))
    theta <- t/(1 + t)
    #sample latent variable
    xs <- rbinom(N, size=1, prob=theta)
    
    #metropolis step for \delta
    stuff <- metropolis(beta=beta, alpha=alpha, delta=delta, lambda=lambda,xs=xs,
                        jump_sd=jump_delta, param="delta")
    delta_keep[k,] <- delta <- stuff$delta
    if (tune & k < adapt) jump_delta <- ifelse(stuff$ratio > 0.44, jump_delta*A, jump_delta*B)
    
    #metropolis step for \lambda
    stuff <- metropolis(beta=beta, alpha=alpha, delta=delta, lambda=lambda,xs=xs,
                        jump_sd=jump_lambda, param="lambda")
    lambda_keep[k,] <- lambda <- stuff$lambda
    if (tune & k < adapt) jump_lambda <- ifelse(stuff$ratio > 0.44, jump_lambda*A, jump_lambda*B)
    
    #calculate log-likelihood
    psi <- exp(rep(delta, ni) + dat$IsED *(rep(lambda, ni) - rep(delta, ni)))  #save repeated calc for lambda
    # psi <- exp(rep(delta, ni) + dat$IsED *(rep(lambda, ni) - rep(delta, ni)) - log(ni))  #save repeated calc for lambda
    #psi <- p/(1 + p)
    #loglik <- sum(ifelse(xs, dat$Total*log(psi) + (dat$n - dat$Total)*log(1-psi), 0))
    loglik <- sum(ifelse(xs, dat$Total*log(psi) - psi, 0))        
    loglik_keep[k] <- loglik
  }
  return(list(betas=beta_keep,
              deltas=delta_keep,
              alphas=alpha_keep,
              lambdas=lambda_keep, 
              logliks=loglik_keep))
}


metropolis <- function(beta, alpha, delta, lambda, xs, jump_sd, param="beta") {
  t <- exp(rep(beta, ni) + dat$IsED * (rep(alpha, ni) - rep(beta, ni)))  #save repeated calc for theta (p is for potential)
  theta <- t/(1 + t)
  # psi <- exp(rep(delta, ni) + dat$IsED *(rep(lambda, ni) - rep(delta, ni)) - log(ni))    #save repeated calc for lambda
  psi <- exp(rep(delta, ni) + dat$IsED *(rep(lambda, ni) - rep(delta, ni)))    #save repeated calc for lambda
  #psi <- p/(1 + p)
  #compute denominator of acceptance prob
  loglike <- ifelse(xs, dat$Total*log(psi) - psi, 0)        
  #loglike <- ifelse(xs, dat$Total*log(psi) + (dat$n - dat$Total)*log(1-psi), 0)
  denom <- diff(c(0, cumsum(loglike)[cum.ni]))
  
  if (param == "beta") {
    prior.old <- dnorm(beta, mean = mean_beta,sd = sd_beta, log=T) #controls prior belief?
    beta_star <- rnorm(n.establishment, mean=beta, sd=jump_sd)
    if(any(!is.finite(beta_star))) browser()
    prior.star <- dnorm(beta_star, mean_beta, sd_beta, log=T)
    t <- exp(rep(beta_star, ni) + dat$IsED * (rep(alpha, ni) - rep(beta_star, ni)))
    theta <- t/(1 + t)
  } 
  if (param == "alpha") {
    prior.old <- dnorm(alpha, mean_alpha, sd_alpha, log=TRUE)
    alpha_star <- rnorm(n.establishment, mean=alpha, sd=jump_sd)
    if(any(!is.finite(alpha_star))) browser()
    prior.star <- dnorm(alpha_star, mean_alpha, sd_alpha, log=TRUE)
    t <- exp(rep(beta, ni) + dat$IsED * (rep(alpha_star, ni) - rep(beta, ni)))
    theta <- t/(1 + t)
  } 
  if (param == "delta") {
    prior.old <- dnorm(delta, mean_delta, sd_delta, log=TRUE) #sigma_delta makes prior diffuse
    delta_star <- rnorm(n.establishment, mean=delta, sd=jump_sd)
    if(any(!is.finite(delta_star))) browser()
    prior.star <- dnorm(delta_star, mean_delta, sd_delta, log=TRUE)
    # psi <- exp(rep(delta_star, ni) + dat$IsED *(rep(lambda, ni) - rep(delta_star, ni)) - log(ni))
    psi <- exp(rep(delta_star, ni) + dat$IsED *(rep(lambda, ni) - rep(delta_star, ni)))
    #psi <- p/(1 + p)
  } 
  if (param == "lambda") {
    prior.old <- dnorm(lambda, mean_lambda, sd_lambda, log=TRUE)
    lambda_star <- rnorm(n.establishment, mean=lambda, sd=jump_sd)
    if(any(!is.finite(lambda_star))) browser()
    prior.star <- dnorm(lambda_star, mean_lambda, sd_lambda, log=TRUE)
    # psi <- exp(rep(delta, ni) + dat$IsED *(rep(lambda_star, ni) - rep(delta, ni)) - log(ni))  
    psi <- exp(rep(delta, ni) + dat$IsED *(rep(lambda_star, ni) - rep(delta, ni)))  
    #psi <- p/(1 + p)
  } 
  #density in numerator of acceptance ratio
  #loglike <- ifelse(xs, dat$Total*log(psi) + (dat$n - dat$Total)*log(1-psi), 0)
  loglike <- ifelse(xs, dat$Total*log(psi) - psi, 0)
  num <- diff(c(0, cumsum(loglike)[cum.ni]))
  ratio <- exp(num + prior.star - denom - prior.old)
  # impose acceptance probability
  if (param == "beta") {
    beta <- ifelse(runif(n.establishment) < ratio, beta_star, beta)
    return(list(beta=beta, ratio=ratio))
  } 
  if (param == "delta") {
    delta <- ifelse(runif(n.establishment) < ratio, delta_star, delta)
    return(list(delta=delta, ratio=ratio))
  } 
  if (param == "alpha") {
    alpha <- ifelse(runif(n.establishment) < ratio, alpha_star, alpha)
    return(list(alpha=alpha, ratio=ratio))
  } 
  if (param == "lambda") {
    lambda <- ifelse(runif(n.establishment) < ratio, lambda_star, lambda)
    return(list(lambda=lambda, ratio=ratio))
  } 
}
