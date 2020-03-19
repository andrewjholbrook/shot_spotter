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

# get probabilities self-exciting
prob_se <- samps[5,]

# check ess
coda::effectiveSize(coda::as.mcmc(prob_se))

# get approx. results from previous study for comparison
sd_pse_lim <- 0.00252525
m_pse_lim  <- 0.125
pse_lim <- rnorm(n=1000,mean=m_pse_lim,sd=sd_pse_lim)

load("data/yes_holidays_samples.Rdata")
prob_se_yh <- (samps[5,] / samps[4,]) # old parameterization, same results

Probs    <- c(pse_lim,prob_se,prob_se_yh)
Analysis <- c(rep("Limited",1000),rep("Full",36000),rep("Full+holidays",36000))
df           <- data.frame(Probs,Analysis)
df$Analysis    <- factor(df$Analysis)
df$Analysis = factor(df$Analysis,levels(df$Analysis)[c(3,1,2)])


gg <- ggplot(data = df, aes(x=Probs,fill=Analysis,color=Analysis)) +
  geom_density(adjust=4,alpha=0.7) +
  xlab("Proportion of events self-excitatory") + ylab("Posterior density") +
  annotate(geom = "label", x = 0.125, y = 135, label = "mean:\n0.130",color=gg_color_hue(3)[1]) +
  annotate(geom = "label", x = 0.18, y = 135, label = "mean:\n0.153",color=gg_color_hue(3)[2]) +
  annotate(geom = "label", x = 0.32, y = 135, label = "mean:\n0.344",color=gg_color_hue(3)[3]) +
  theme_classic()

gg

ggsave('compare_thetas.pdf',gg,
       device = 'pdf',path = 'figures/',width = 6,height=4)

