setwd("~/shot_spotter/")

load("data/no_holidays_samples_new_param.Rdata")
samps[1,] <- 1/samps[1,] * 1000 # spatial lengthscale in meters
samps[4,] <- 1/samps[4,] * 60   # temporal lengthscale in minutes

samps <- coda::as.mcmc(t(samps[c(1,4:6),]))
coda::effectiveSize(samps)
coda::HPDinterval(samps)


load("data/yes_holidays_samples.Rdata")

selfweight <- samps[5,]/samps[4,]
coda::HPDinterval(coda::as.mcmc(selfweight))
