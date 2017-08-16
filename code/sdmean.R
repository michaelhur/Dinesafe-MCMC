ed <- dat[dat$IsED == 1,'Total']
noned <- dat[dat$IsED == 0,'Total']

IsInf_ed <- ifelse(ed != 0, 1, 0)
IsInf_noned <- ifelse(noned != 0, 1, 0)

mean_beta <- mean(IsInf_noned)
sd_beta <- sd(IsInf_noned)

mean_alpha <- mean(IsInf_ed)
sd_alpha <- sd(IsInf_ed)

mean_delta <- mean(noned)
sd_delta <- sd(noned)

mean_lambda <- mean(ed)
sd_lambda <- sd(ed)