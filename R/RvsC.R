setwd("~/shot_spotter/")

library(hpHawkes)
library(mvtnorm)

Results <- matrix(ncol=3)

for (i in c(1000,2000,3000,4000,5000,10000,20000,50000)) {
  Results <- rbind(Results,timeTest(locationCount = i,r=TRUE,maxIts=1))
  Results <- rbind(Results,timeTest(locationCount = i,maxIts =1))
}

Results <- Results[-1,3]

speedups <- Results[c(1,3,5,7,9,11,13,15)]/Results[c(2,4,6,8,10,12,14,16)]
