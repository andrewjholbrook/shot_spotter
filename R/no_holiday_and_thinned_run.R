setwd("~/shot_spotter/")

library(hpHawkes)

load("data/no_holidays_thinned.Rdata")

X <- as.matrix(df_no_holidays_thinned[,1:2])
times <- df_no_holidays_thinned$Time

# fixed spatial smoother (2nd parameter) corresponds to 1.6 km lengthscale
# fixed temporal smoother (3rd parameter) corresponds to 14 day lenghtscale
Max <- 10000
burn <- 1000
set.seed(666)

res <- sampler(n_iter=Max,burnIn=burn,locations = X,
                 params=c(1, 1/1.6, 1/(14*24),1,1,1),
                 times=times,gpu=2,radius=2)
res2 <- sampler(n_iter=Max,burnIn=burn,locations = X,
                  params=c(1, 1/1.6, 1/(14*24),1,1,1),
                  times=times,gpu=2,radius=2)
res3 <- sampler(n_iter=Max,burnIn=burn,locations = X,
                  params=c(1, 1/1.6, 1/(14*24),1,1,1),
                  times=times,gpu=2,radius=2)
res4 <- sampler(n_iter=Max,burnIn=burn,locations = X,
                  params=c(1, 1/1.6, 1/(14*24),1,1,1),
                  times=times,gpu=2,radius=2)

png(filename = "figures/no_holiday_thinned_converg_new_param.png", width = 7, height = 7, units = 'in', res = 300)
par(mfrow=c(2,2))

# spatial lengthscale
plot(1/res$samples[1,],type="l",ylab="Spatial lengthscale (km)")
lines(1/res2$samples[1,],col="red")
lines(1/res3$samples[1,],col="green")
lines(1/res4$samples[1,],col="blue")
abline(h=median(c(1/res$samples[1,],1/res2$samples[1,],1/res3$samples[1,],1/res4$samples[1,])),lwd=4)

plot(1/res$samples[4,],type="l",ylab="Temporal lengthscale (hrs)")
lines(1/res2$samples[4,],col="red")
lines(1/res3$samples[4,],col="green")
lines(1/res4$samples[4,],col="blue")
abline(h=median(c(1/res$samples[4,],1/res2$samples[4,],1/res3$samples[4,],1/res4$samples[4,])),lwd=4)

plot(res$samples[5,],type="l",ylab="Self-excitatory weight")
lines(res2$samples[5,],col="red")
lines(res3$samples[5,],col="green")
lines(res4$samples[5,],col="blue")
abline(h=median(c(res$samples[5,],res2$samples[5,],res3$samples[5,],res4$samples[5,])),lwd=4)

# plot(res$samples[5,]/res$samples[4,],type="l",ylab="Self-excitatory weight")
# lines(res2$samples[5,]/res2$samples[4,],col="red")
# lines(res3$samples[5,]/res3$samples[4,],col="green")
# lines(res4$samples[5,]/res4$samples[4,],col="blue")
# abline(h=median(c(res$samples[5,]/res$samples[4,],res2$samples[5,]/res2$samples[4,],res3$samples[5,]/res3$samples[4,],res4$samples[5,]/res4$samples[4,])),lwd=4)


plot(res$samples[6,],type="l",ylab="Background weight")
lines(res2$samples[6,],col="red")
lines(res3$samples[6,],col="green")
lines(res4$samples[6,],col="blue")
abline(h=median(c(res$samples[6,],res2$samples[6,],res3$samples[6,],res4$samples[6,])),lwd=4)

dev.off()

samps <- cbind(res$samples,res2$samples,res3$samples,res4$samples)

save(samps,file = "data/no_holidays_thinned_samples_new_param.Rdata")


