setwd("~/shot_spotter/")

library(ggplot2)

load("data/post_prob_child.Rdata")
load("data/prob_child.Rdata")

# n51335 ~ 0.17
# n13 ~ 0.275
# n3119 ~ 0.55
# n1303 ~ 0.725
# n 46446 ~ 0.965

DF <- data.frame(Probability = c( post_prob_child[9428,],
                                  post_prob_child[51335,],
                                  post_prob_child[13,],
                                  post_prob_child[3119,],
                                  post_prob_child[1303,],
                                  post_prob_child[12873,],
                                  post_prob_child[46446,]))

load("data/dates_and_self_excit_probs.Rdata")
DF$Date <- c(rep(df2$Date[9428], 1000),
             rep(df2$Date[51335], 1000),
             rep(df2$Date[13], 1000),
             rep(df2$Date[3119], 1000),
             rep(df2$Date[1303], 1000),
             rep(df2$Date[12873], 1000),
             rep(df2$Date[46446], 1000))

load("data/dc_locations_and_self_excit_probs.Rdata")
DF$Latitude <- c(rep(df$Latitude[9428], 1000),
                 rep(df$Latitude[51335], 1000),
             rep(df$Latitude[13], 1000),
             rep(df$Latitude[3119], 1000),
             rep(df$Latitude[1303], 1000),
             rep(df$Latitude[12873], 1000),
             rep(df$Latitude[46446], 1000))

DF$Longitude <- c(rep(df$Longitude[9428], 1000),
                  rep(df$Longitude[51335], 1000),
                 rep(df$Longitude[13], 1000),
                 rep(df$Longitude[3119], 1000),
                 rep(df$Longitude[1303], 1000),
                 rep(df$Longitude[12873], 1000),
                 rep(df$Longitude[46446], 1000))

DF$Date <- factor(DF$Date)
DF$Latitude <- factor(DF$Latitude)
DF$Longitude <- factor(DF$Longitude)
DF$Event <- with(DF, interaction(Date,  Latitude, Longitude,drop=TRUE))
levels(DF$Event) <- c("07-15-07 (-76.993, 38.834)",
                         "07-10-06 (-76.979, 38.844)",
                         "11-08-18 (-76.969, 38.856)",
                         "01-28-06 (-76.984, 38.862)",
                         "08-28-17 (-76.943, 38.878)",
                         "05-21-09 (-76.971, 38.896)",
                         "11-22-09 (-76.978, 38.899)")

gg <- ggplot(DF, aes(x=Probability, color=Event,fill=Event)) +
  geom_density(adjust=2,alpha=0.7) +
  ylab("Posterior density") + xlab("Individual probability self-excitatory") +
  guides(fill=guide_legend(title="Event date and location"),color=guide_legend(title="Event date and location")) +
  theme_classic()
gg


ggsave(filename = "individual_se_probs.pdf",gg,device = "pdf",path="figures/",
       width = 8,height = 3)

