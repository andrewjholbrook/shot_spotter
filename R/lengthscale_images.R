setwd("~/shot_spotter/")

library(ggplot2)
library(gridExtra)
library(grid)

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

grid_arrange_shared_legend <- function(..., nrow = 1, ncol = length(list(...)), position = c("bottom", "right")) {
  
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position = "none"))
  gl <- c(gl, nrow = nrow, ncol = ncol)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  grid.newpage()
  grid.draw(combined)
  
}


load("data/no_holidays_samples_new_param.Rdata")

# check ess
coda::effectiveSize(coda::as.mcmc(1/samps[1,]))
coda::effectiveSize(coda::as.mcmc(1/samps[4,]))
coda::effectiveSize(coda::as.mcmc(samps[5,]))
coda::effectiveSize(coda::as.mcmc(samps[6,]))


# get approx. results from previous study for comparison
sd_temp_lim <- 0.38
m_temp_lim  <- 10.25
ls_temp_lim <- rnorm(n=1000,mean=m_temp_lim,sd=sd_temp_lim)

sd_spat_lim <- 3.28
m_spat_lim  <- 127.5
ls_spat_lim <- rnorm(n=1000,mean=m_spat_lim,sd=sd_spat_lim)

Lengthscales <- c(ls_spat_lim,ls_temp_lim)
Type         <- c(rep("Spatial",1000),rep("Temporal",1000))
df           <- data.frame(Lengthscales,Type)
df$Type      <- factor(df$Type) 
df$Analysis    <- rep("Limited",2000)

####
############
####
# create lengthscales figure (spatial is km, tempral is hrs)
Lengthscales <- c(1/samps[1,],1/samps[4,])
Type         <- c(rep("Spatial",36000),rep("Temporal",36000))
df2           <- data.frame(Lengthscales,Type)
df2$Type      <- factor(df2$Type)
df2$Lengthscales[df2$Type=="Temporal"] <- df2$Lengthscales[df2$Type=="Temporal"] * 60
df2$Lengthscales[df2$Type=="Spatial"] <- df2$Lengthscales[df2$Type=="Spatial"] * 1000
df2$Analysis  <- rep("Full",72000)

df <- rbind(df,df2)
df$Analysis <- factor(df$Analysis)
df$Analysis <- factor(df$Analysis,levels = levels(df$Analysis)[2:1])

gg <- ggplot(data = df[df$Type=="Spatial",], aes(x=Lengthscales,fill=Analysis,color=Analysis)) +
  geom_density(adjust=4,alpha=0.7) +
  xlab("Spatial lengthscale (meters)") + ylab("Posterior density") +
  #ggtitle("Self-excitatory lengthscales") +
  annotate(geom = "label", x = 85, y = 0.2, label = "mean: 70",color=gg_color_hue(2)[2]) +
  annotate(geom = "label", x = 126, y = 0.2, label = "mean: 126",color=gg_color_hue(2)[1]) +
  # geom_text(aes(x=85,y=0.2,label="mean: 70",size=1),
  #           inherit.aes = FALSE,show.legend = FALSE,
  #           check_overlap = TRUE,color=gg_color_hue(2)[1])+
  # geom_text(aes(x=126,y=0.2,label="mean: 126",size=1),
  #           inherit.aes = FALSE,show.legend = FALSE,
  #           check_overlap = TRUE,color=gg_color_hue(2)[2])+
  theme_classic()

gg

gg2 <- ggplot(data = df[df$Type=="Temporal",], aes(x=Lengthscales,fill=Analysis,color=Analysis)) +
  geom_density(adjust=4,alpha=0.7) +
  xlab("Temporal lengthscale (minutes)") + ylab("") +
  # geom_text(aes(x=3.2,y=7.2,label="mean: 1.0",size=1),
  #           inherit.aes = FALSE,show.legend = FALSE,
  #           check_overlap = TRUE,color=gg_color_hue(2)[1])+
  # geom_text(aes(x=9,y=7.2,label="mean: 10",size=1),
  #           inherit.aes = FALSE,show.legend = FALSE,
  #           check_overlap = TRUE,color=gg_color_hue(2)[2])+
  annotate(geom = "label", x = 3.2, y = 8.2, label = "mean: 1.0",color=gg_color_hue(2)[2]) +
  annotate(geom = "label", x = 9, y = 8.2, label = "mean: 10",color=gg_color_hue(2)[1]) +
  theme_classic()

gg2

ggsave('compare_lengthscales.pdf',grid_arrange_shared_legend(gg,gg2,position="right"),
       device = 'pdf',path = 'figures/',
       width = 10,height=4)

