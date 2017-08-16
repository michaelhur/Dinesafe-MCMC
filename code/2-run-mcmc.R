## Import Packages
library("coda")
library("xtable")

## Import R script file that consists of MCMC functions.
source("code/1-mcmc.R")

## Just to extract IsED indicator for each establishment
IsED <- dat$IsED

## Parameters to be fed into MCMC computation
n.iter <- 10000
adapt <- 500
burnin <- 5000

## 'res' is a resulting MCMC object that is used to anlayze 
res <- mcmc_chain(dat, reps=n.iter)
#save(res, file="v3_results.rda")

## Extracting alpha_i's, beta_i's, delta_i's and lambda_i's from the resulting MCMC object.
alphas <- mcmc.list(lapply(res, function(x) mcmc(x$alphas[burnin:n.iter,])))
betas <- mcmc.list(lapply(res, function(x) mcmc(x$betas[burnin:n.iter,])))
deltas <- mcmc.list(lapply(res, function(x) mcmc(x$deltas[burnin:n.iter,])))
lambdas <- mcmc.list(lapply(res, function(x) mcmc(x$lambdas[burnin:n.iter,])))