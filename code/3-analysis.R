## Take an average of three chains to acquire alpha, beta, delta and lambda values
## And use those values to get theta and psi accordingly.
betaz <- apply(as.matrix(betas), 2, mean)
alphaz <- apply(as.matrix(alphas), 2, mean)

t <- exp(betaz + IsED * (alphaz - betaz))
thetaz <- t/(1+t)

lambdaz <- apply(as.matrix(lambdas), 2, mean)
deltaz <- apply(as.matrix(deltas), 2, mean)
psiz <- exp(deltaz + IsED *(lambdaz - deltaz))


###########################################
#### The Gelman-Rubin Reduction Factor ####
###########################################

## convergence diagnostics -- should be close to 1
gelman_alpha <- gelman.diag(alphas)
gelman_beta <- gelman.diag(betas)
gelman_delta <- gelman.diag(deltas)
gelman_lambda <- gelman.diag(lambdas)

## Extract first few for each of alpha, beta, delta, lambda
head(gelman_alpha, 5)
head(gelman_beta, 5)
head(gelman_delta, 5)
head(gelman_lambda, 5)

## summary
summary(gelman_alpha)
summary(gelman_beta)
summary(gelman_delta)
summary(gelman_lambda)

## Plot point estimation
library(ggplot2)

gelman_alpha_df <- as.data.frame(gelman_alpha)
colnames(gelman_alpha_df) <- c('Point_Est', "Upper_CI")

ggplot(gelman_alpha_df, aes(x = 1:nrow(gelman_alpha_df), y = Point_Est)) +
  theme_update(plot.title = element_text(hjust = 0.5)) +
  ggtitle("The Gelman-Rubin Reduction Factor for Alpha's") +
  xlab('i') + ylab('Gelman-Rubin Reduction Factor') +
  geom_point(alpha = 0.1, colour = "blue") 


gelman_beta_df <- as.data.frame(gelman_beta)
colnames(gelman_beta_df) <- c('Point_Est', "Upper_CI")

ggplot(gelman_beta_df, aes(x = 1:nrow(gelman_beta_df), y = Point_Est)) +
  theme_update(plot.title = element_text(hjust = 0.5)) +
  ggtitle("The Gelman-Rubin Reduction Factor for Beta's") +
  xlab('i') + ylab('Gelman-Rubin Reduction Factor') +
  geom_point(alpha = 0.1, colour = "blue") 


gelman_delta_df <- as.data.frame(gelman_delta)
colnames(gelman_delta_df) <- c('Point_Est', "Upper_CI")

ggplot(gelman_delta_df, aes(x = 1:nrow(gelman_delta_df), y = Point_Est)) +
  theme_update(plot.title = element_text(hjust = 0.5)) +
  ggtitle("The Gelman-Rubin Reduction Factor for Delta's") +
  xlab('i') + ylab('Gelman-Rubin Reduction Factor') +
  geom_point(alpha = 0.1, colour = "blue") 


gelman_lambda_df <- as.data.frame(gelman_lambda)
colnames(gelman_lambda_df) <- c('Point_Est', "Upper_CI")

ggplot(gelman_lambda_df, aes(x = 1:nrow(gelman_lambda_df), y = Point_Est)) +
  theme_update(plot.title = element_text(hjust = 0.5)) +
  ggtitle("The Gelman-Rubin Reduction Factor for Lambda's") +
  xlab('i') + ylab('Gelman-Rubin Reduction Factor') +
  geom_point(alpha = 0.1, colour = "blue") 



#95% credible intervals
alpha_summary <- summary(alphas)
beta_summary <- summary(betas)
lambda_summary <- summary(lambdas)
delta_summary <- summary(deltas)

## Extract a few
alpha_summary[[2]][,c(1,5)]
beta_summary[[2]][,c(1,5)]
delta_summary[[2]][,c(1,5)]
lambda_summary[[2]][,c(1,5)]

head(alpha_summary[[2]][,c(1,5)],5)
head(beta_summary[[2]][,c(1,5)],5)
head(delta_summary[[2]][,c(1,5)],5)
head(lambda_summary[[2]][,c(1,5)],5)

summary(alpha_summary[[2]][,c(1,5)])
summary(beta_summary[[2]][,c(1,5)])
summary(delta_summary[[2]][,c(1,5)])
summary(lambda_summary[[2]][,c(1,5)])


## Construct a data frame for each of alpha, beta, delta and lambda that consists of 95% credible interval. 
alpha_ci_df <- as.data.frame(alpha_summary[[2]][,c(1,5)])
colnames(alpha_ci_df) <- c("L","U")
alpha_ci_df$alpha <- alphaz

beta_ci_df <- as.data.frame(beta_summary[[2]][,c(1,5)])
colnames(beta_ci_df) <- c("L","U")
beta_ci_df$beta <- betaz

delta_ci_df <- as.data.frame(delta_summary[[2]][,c(1,5)])
colnames(delta_ci_df) <- c("L","U")
delta_ci_df$delta <- deltaz

lambda_ci_df <- as.data.frame(lambda_summary[[2]][,c(1,5)])
colnames(lambda_ci_df) <- c("L","U")
lambda_ci_df$lambda <- lambdaz


## Credible Interval plots for first 10 inspections
ggplot(alpha_ci_df[1:10,], aes(y = 1:10, x = alpha)) +
  theme_update(plot.title = element_text(hjust = 0.5)) +
  ggtitle("95% Credible Interval for first 10 Alpha values") +
  xlab("Credible Interval") + ylab("i") +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmax = U, xmin = L))

ggplot(beta_ci_df[1:10,], aes(y = 1:10, x = beta)) +
  theme_update(plot.title = element_text(hjust = 0.5)) +
  ggtitle("95% Credible Interval for first 10 Beta values") +
  xlab("Credible Interval") + ylab("i") +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmax = U, xmin = L))

ggplot(delta_ci_df[1:10,], aes(y = 1:10, x = delta)) +
  theme_update(plot.title = element_text(hjust = 0.5)) +
  ggtitle("95% Credible Interval for first 10 Delta values") +
  xlab("Credible Interval") + ylab("i") +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmax = U, xmin = L))

ggplot(lambda_ci_df[1:10,], aes(y = 1:10, x = lambda)) +
  theme_update(plot.title = element_text(hjust = 0.5)) +
  ggtitle("95% Credible Interval for first 10 Lambda values") +
  xlab("Credible Interval") + ylab("i") +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmax = U, xmin = L))


## Construct credible interval for the difference alpha_i - beta_i and delta_i - lambda_i
compute_v <- function(s_i, s_j, n_i = 5001, n_j = 5001) {
  return(((s_i^2/n_i) + (s_j^2/n_j))^2/(s_i^4/(n_i^2*(n_i-1))+s_j^4/(n_j^2*(n_j-1))))
}


construct_ci <- function(x_i, x_j, s_i, s_j, n_i = 5001, n_j = 5001, alpha = 0.95){
  v = compute_v(s_i, s_j, n_i, n_j)
  diff = x_i - x_j
  t = qt((1-alpha)/2, v)
  se = sqrt(s_i^2/n_i + s_j^2/n_j)
  l = diff - t*se
  u = diff + t*se
  return(c(l,u))
}

alpha_beta_ci = matrix(NA, ncol = 2, nrow = 15347)
for (i in 1:15347){
  alpha_beta_ci[i,] = construct_ci(alpha_summary$statistics[i,"Mean"], beta_summary$statistics[i,"Mean"], 
                                   alpha_summary$statistics[i,"SD"], beta_summary$statistics[i,"SD"])
}

delta_lambda_ci = matrix(NA, ncol = 2, nrow = 15347)
for (i in 1:15347){
  delta_lambda_ci[i,] = construct_ci(delta_summary$statistics[i,"Mean"], lambda_summary$statistics[i,"Mean"], 
                                   delta_summary$statistics[i,"SD"], lambda_summary$statistics[i,"SD"])
}

head(alpha_beta_ci)
head(exp(alpha_beta_ci)/(1+exp(betaz + alpha_beta_ci)))

head(delta_lambda_ci)
head(exp(delta_lambda_ci))


dat$theta <- thetaz
dat$psi <- psiz

library(dplyr)
param_df <- dat %>%
  group_by(ESTABLISHMENT_ID) %>%
  summarize(IsED = mean(IsED), theta = mean(theta), psi = mean(psi), min_inspection = mean(MINIMUM_INSPECTIONS_PERYEAR), inspection = n(), Total = sum(Total))


## Histogram of theta values
ggplot(param_df,aes(x = theta)) + 
  theme_update(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Histogram of Theta values by Region") +
  xlab("Theta") + ylab("Count") +
  geom_histogram(data = subset(param_df, IsED == 1), aes(fill = "Downtown"), alpha = 0.2) +
  geom_histogram(data = subset(param_df, IsED == 0), aes(fill = "Elsewhere"), alpha = 0.2) + 
  scale_fill_manual(name = "Region", values = c(Downtown = "red", Elsewhere = "blue"))


IsED <- param_df$IsED
psi <- param_df$psi
theta <- param_df$theta


## ECDF of Theta
ggplot(param_df,aes(x = theta)) + 
  theme_update(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Infraction Potential Based on \n Establishment's Location") +
  xlab("Infraction potential") + ylab("Empirical Distribution Function") +
  stat_ecdf(data = subset(param_df, IsED == 1), aes(colour = "Downtown")) +
  stat_ecdf(data = subset(param_df, IsED == 0), aes(colour = "Elsewhere")) + 
  scale_colour_manual(name = "Region", values = c(Downtown = "red", Elsewhere = "blue"))

par(mfrow=c(1,1))
non_ed_est <- ecdf(theta[IsED == 0])
ed_est <- ecdf(theta[IsED == 1])
plot(non_ed_est, main="Infraction Potential Based on \n Establishment's Location", ylab="Empirical Distribution Function", 
     xlab="Infraction potential", xlim=c(0.56, 0.63))
lines(ed_est, col=2)
op <- par(cex = 0.5)
legend(0.56, 0.8, c("Entertainment District", "Elsewhere"), col=2:1, lty=1)


## ECDF of Psi
par(mfrow=c(1,1))
non_ed_count <- ecdf(psi[IsED == 0])
ed_count <- ecdf(psi[IsED == 1])
plot(non_ed_count, main="ECDF of Expected Count of Non-Compliance", ylab="Empirical Distribution Function", 
     xlab="Infraction Count")#, xlim=c(0.56, 0.63))
lines(ed_count, col=2)
op <- par(cex = 0.5)
legend(4, 0.8, c("Entertainment District", "Elsewhere"), col=2:1, lty=1)

ggplot(param_df,aes(x = psi)) + 
  theme_update(plot.title = element_text(hjust = 0.5)) +
  ggtitle("ECDF of Expected Count of Non-Compliance") +
  xlab("Infraction Count") + ylab("Empirical Distribution Function") +
  stat_ecdf(data = subset(param_df, IsED == 1), aes(colour = "Downtown")) +
  stat_ecdf(data = subset(param_df, IsED == 0), aes(colour = "Elsewhere")) + 
  scale_colour_manual(name = "Region", values = c(Downtown = "red", Elsewhere = "blue"))


## Histogram of Expected Count of Non-Compliance
ggplot(param_df,aes(x = psi)) + 
  theme_update(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Distribution of Expected Count of Non-Compliance") +
  xlab("Infraction Count") + ylab("Count") +
  geom_histogram(data = subset(param_df, IsED == 1), aes(fill = "Downtown"), alpha = 0.2) +
  geom_histogram(data = subset(param_df, IsED == 0), aes(fill = "Elsewhere"), alpha = 0.2) + 
  scale_fill_manual(name = "Region", values = c(Downtown = "red", Elsewhere = "blue"))

## Density of Expected Count of Non-Compliance
ggplot(param_df,aes(x = psi)) + 
  theme_update(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Distribution of Expected Count of Non-Compliance") +
  xlab("Infraction Count") + ylab("Density") +
  geom_histogram(data = subset(param_df, IsED == 1), aes(y = ..count../sum(..count..), fill = "Downtown"), position="identity", alpha = 0.2) +
  geom_histogram(data = subset(param_df, IsED == 0), aes(y = ..count../sum(..count..), fill = "Elsewhere"), position="identity", alpha = 0.2) + 
  scale_fill_manual(name = "Region", values = c(Downtown = "red", Elsewhere = "blue"))



min_inspection  <- param_df$min_inspection
min_ed <- min_inspection[IsED != 0]
min_noned <- min_inspection[IsED ==0]

par(mfrow=c(1,1))
non_ed_est_min <- ecdf(min_noned*theta[IsED == 0])
ed_est_min <- ecdf(min_ed*theta[IsED == 1])
plot(non_ed_est_min, main="Infraction Potential over \n Minimum Required Inspections", ylab="Empirical Distribution Function", 
     xlab="Infraction potential")
lines(ed_est_min, col=2)
op <- par(cex = 0.75)
legend(0.5, 0.9, c("Entertainment District", "Elsewhere"), col=2:1, lty=1)

par(mfrow=c(1,1))
non_ed_count_min <- ecdf(min_noned*psi[IsED == 0])
ed_count_min <- ecdf(min_ed*psi[IsED == 1])
plot(non_ed_count_min, main="Expected Count of Non-Compliance during \n Minimum Required Inspections", ylab="Empirical Distribution Function", 
     xlab="Infraction Count")
lines(ed_count_min, col=2)
op <- par(cex = 0.75)
legend(0.5, 0.9, c("Entertainment District", "Elsewhere"), col=2:1, lty=1)


## ECDF of Theta
## Likelihood of getting at least one infraction in minimum required inspections, which we denote by n, is obtained by binomial distribution
## 1 - probability of getting no infraction in min required inspection
## 1 - pbinom(n = min_inspection, p = theta, x = 0)

param_df$min_ins_theta = sapply(1:nrow(param_df), FUN = function(i) 1 - pnorm(0, param_df$min_inspection[i], param_df$theta[i]))

ggplot(param_df,aes(x = min_ins_theta)) + 
  theme_update(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Infraction Potential in Minimum Required Inspection") +
  xlab("Infraction potential") + ylab("Empirical Distribution Function") +
  stat_ecdf(data = subset(param_df, IsED == 1), aes(colour = "Downtown")) +
  stat_ecdf(data = subset(param_df, IsED == 0), aes(colour = "Elsewhere")) + 
  scale_colour_manual(name = "Region", values = c(Downtown = "red", Elsewhere = "blue"))

## Sum of n i.i.d Poisson distribution X_sum is such that X_sum ~ Pois(n*lambda)
param_df$min_ins_psi = param_df$min_inspection * param_df$psi

ggplot(param_df,aes(x = min_ins_psi)) + 
  theme_update(plot.title = element_text(hjust = 0.5)) +
  ggtitle("ECDF of Expected Count of Non-Compliance \n during Minimum Required Inspections") +
  xlab("Infraction Count") + ylab("Empirical Distribution Function") +
  stat_ecdf(data = subset(param_df, IsED == 1), aes(colour = "Downtown")) +
  stat_ecdf(data = subset(param_df, IsED == 0), aes(colour = "Elsewhere")) + 
  scale_colour_manual(name = "Region", values = c(Downtown = "red", Elsewhere = "blue")) +
  scale_x_continuous(breaks=seq(0,10,1))
                     

ggplot(param_df,aes(x = min_ins_psi)) + 
  theme_update(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Expected Count of Non-Compliance in \n Minimum Required Inspection") +
  xlab("Infraction Count") + ylab("Count") +
  geom_histogram(data = subset(param_df, IsED == 1), aes(fill = "Downtown"), alpha = 0.2) +
  geom_histogram(data = subset(param_df, IsED == 0), aes(fill = "Elsewhere"), alpha = 0.2) + 
  scale_fill_manual(name = "Region", values = c(Downtown = "red", Elsewhere = "blue"))

ggplot(param_df,aes(x = min_ins_psi)) + 
  theme_update(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Expected Count of Non-Compliance \n during Minimum Required Inspection") +
  xlab("Infraction Count") + ylab("Density") +
  geom_histogram(data = subset(param_df, IsED == 1), aes(y = ..count../sum(..count..), fill = "Downtown"), position="identity", alpha = 0.2) +
  geom_histogram(data = subset(param_df, IsED == 0), aes(y = ..count../sum(..count..), fill = "Elsewhere"), position="identity", alpha = 0.2) + 
  scale_fill_manual(name = "Region", values = c(Downtown = "red", Elsewhere = "blue"))


# If X_1, X_2, ... and X_n ~ iid ~ Pois(lambda), then X_1 + X_2 + ... + X_n ~ Pois(n*lambda)
n.zeros2 <- NULL
ranges2 <- NULL

n.inspection <- nrow(dat)

sim_y_matrix2 <- matrix(NA, ncol = n.inspection, nrow = 5000)
sim_x_matrix2 <- matrix(NA, ncol = n.inspection, nrow = 5000)

for (m in 1:5000) {
  sim_x_matrix2[m,] <- sapply(1:n.inspection, FUN = function(x) rbinom(n = 1, size = 1, prob = thetaz[x]))
  ## Max of 1 and rpois since if there is an infraction, there must be at least 1 infraction
  ## if rpois() returns 0, then by default we populate the entry with 1, else populate with rpois()
  sim_y_matrix2[m,] <- sapply(1:n.inspection, FUN = function(x) max(1, rpois(n = 1, lambda = psiz[x])))
  sim_y_matrix2[m,] <- sim_x_matrix2[m,] * sim_y_matrix2[m,]
  n.zeros2 <- c(n.zeros2, sum(sim_y_matrix2[m,] == 0))
  ranges2 <- c(ranges2, max(sim_y_matrix2[m,] - min(sim_y_matrix2[m,])))
}

real.n.zero2 <- sum(dat$Total == 0)
p12 <- sum(n.zeros2 > real.n.zero2)/length(n.zeros2)
real.range2 <- max(dat$Total)-min(dat$Total)
p22 <- sum(ranges2 > real.range2)/length(ranges2)

p12
p22

hist(n.zeros2)
abline(v=real.n.zero2, col=2)
hist(ranges2)
abline(v=real.range2, col=2)


#acceptance probs
nunique <- function(x) length(unique(x))
ac.alpha <- apply(res[[1]]$alphas, 2, nunique)/n.iter
ac.beta <- apply(res[[1]]$betas, 2, nunique)/n.iter
ac.delta <- apply(res[[1]]$deltas, 2, nunique)/n.iter
ac.lambda <- apply(res[[1]]$lambda, 2, nunique)/n.iter

hist(ac.alpha)
hist(ac.beta)
hist(ac.delta)
hist(ac.lambda)

prob_df <- data.frame(alphas=ac.alpha, betas=ac.beta, deltas=ac.delta, lambdas=ac.lambda)
xtable(prob_df, caption="Acceptance Rates for model parameters sampled via Metropolis-Hastings")
