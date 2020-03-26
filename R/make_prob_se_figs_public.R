setwd("~/shot_spotter/")

library(ggplot2)
library(maps)
library(ggmap)
# register_google(key = "SOMETHING")


gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

# # get events data
# load("data/no_holidays.Rdata")
# X <- as.matrix(df_no_holidays[,1:2])
# times <- df_no_holidays$Time
#
# # get prob child
# load("data/prob_child.Rdata")
#
# df <- data.frame(X[,1],X[,2],prob_child1)
# colnames(df) <- c("Latitude","Longitude","Probability\nself-excitatory")
#
# gg <- ggplot(df,aes(x=Latitude,y=Longitude)) +
#   geom_point(aes(color=`Probability\nself-excitatory`,
#                  alpha=`Probability\nself-excitatory`)) +
#   scale_colour_gradient(low = "black", high = "purple") +
#   scale_alpha(range = c(0.15,0.7)) +
#   theme_classic()
#
# gg

load("data/dc_locations_and_self_excit_probs.Rdata")
colnames(df)[3] <- "Probability\nself-\nexcitatory"

#where <- geocode("washington dc")
dcMap <- qmap("washington dc", zoom=12,
              maptype="toner-background") +
  geom_point(data=df,aes(x=Latitude,y=Longitude,color=`Probability\nself-\nexcitatory`,
                         alpha=`Probability\nself-\nexcitatory`)) +
  scale_colour_distiller(palette="Spectral") +
  scale_alpha_continuous(range = c(0.3,1),guide=FALSE) +
  annotate(geom="label",x=-77.036386,y=38.892711,label="Washington D.C.") +
  theme(legend.position = c(0.13, 0.6),
        legend.background = element_rect(size=0.2, linetype="solid",
        colour ="black"))
dcMap

ggsave(filename="dc_map",plot=dcMap,device="png",path="figures/",dpi="retina")


#
###########
####################
###########
# dates and self excite probs

load("data/dates_and_self_excit_probs.Rdata")
library(scales)
library(RColorBrewer)
library(grid)
library(gridExtra)

gg <- ggplot(data = df2, aes(x=Date)) +
  stat_smooth(aes(x=Date,y=Probabilities),se=FALSE,color=brewer.pal(11,"Spectral")[2]) +
  stat_density(geom="line",aes(y=..density..*1000),adjust=2,size=1,color=brewer.pal(11,"Spectral")[10]) +
  scale_x_date(date_breaks = "2 year",
               labels = date_format("%Y")) +
  xlab("Year") + ylab("") +
  annotate(geom="text", x=df2$Date[38000],y=0.075,color=brewer.pal(11,"Spectral")[2],label="Self-excitatory probabilities") +
  annotate(geom="segment", x=df2$Date[12000],xend =df2$Date[22000],y=0.075,yend=0.075,color=brewer.pal(11,"Spectral")[2],size=1.5) +
  annotate(geom="text", x=df2$Date[37500],y=0.06,color=brewer.pal(11,"Spectral")[10],label="Gunshot density (x1000)") +
  annotate(geom="segment", x=df2$Date[12000],xend =df2$Date[22000],y=0.06,yend=0.06,color=brewer.pal(11,"Spectral")[10],size=1.5) +
  theme_classic()

gg

ggsave(plot=gg, filename = "year_se_probs",device = "pdf",path="figures/")



ggsave(filename = "combined_year_map.png", grid.arrange(dcMap,gg,ncol=2),
       device = "png",path="figures/" ,width = 9,height = 4,dpi="retina")










