setwd("~/shot_spotter/")

library(hpHawkes)

# get events data
load("data/no_holidays.Rdata")
X <- as.matrix(df_no_holidays[,1:2])
times <- df_no_holidays$Time

# get posterior samples means
load("data/no_holidays_samples_new_param.Rdata")

# get probability child

prob_child1 <- hpHawkes::probability_se(locations = X,
                                    times = times,
                                    params = rowMeans(samps),
                                    gpu = 2)

# prob_child2 <- hpHawkes::probability_se(locations = X,
#                                         times = times,
#                                         params = rowMeans(samps),
#                                         threads = 2, simd=2)

save(prob_child1,file="data/prob_child.Rdata")


# get posterior of prob_childs based on thinned sample of length 1000
thinned <- samps[,seq(from=1,to=36000,by=36)]
post_prob_child <- matrix(0,length(times),1000)
for (i in 1:1000) {
  post_prob_child[,i] <- hpHawkes::probability_se(locations = X,
                                                  times = times,
                                                  params = thinned[,i],
                                                  gpu = 2)
}

save(post_prob_child,file="data/post_prob_child.Rdata")

