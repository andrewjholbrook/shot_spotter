setwd('~/shot_spotter/')
library(readr)
library(xtable)
library(ggplot2)
library(reshape2)
library(scales)
library(gridExtra)
library(grid)
library(plyr)


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

########################################################################################################

time_by_N <- read_table2("finalSamples/time_by_N.txt", 
                         col_names = TRUE)
time_by_N <- time_by_N[,-c(1,3)]
colnames(time_by_N) <- c('Cores',
                         'N','Dimension',
                         'its',
                         'Likelihood',
                         'Gradient')
#time_by_N <- time_by_N[time_by_N$Dimension==2,]
time_by_N$Likelihood <- time_by_N$Likelihood / time_by_N$its
time_by_N$Gradient <- time_by_N$Gradient / time_by_N$its
#time_by_N$N <- time_by_N$N*(time_by_N$N-1)/2

#time_by_N$LIKSPEEDUP <- time_by_N$Likelihood[1:7] / time_by_N$Likelihood
#time_by_N$GradSpeedUp <- time_by_N$Gradient[1:7] / time_by_N$Gradient
time_by_N <- time_by_N[,-3]
time_by_N <- time_by_N[,-3]
#time_by_N <- time_by_N[time_by_N$N!=15000,]

# time_by_N <- time_by_N[time_by_N$Cores==1 |
#                          time_by_N$Cores==2 |
#                          time_by_N$Cores==4 |
#                          time_by_N$Cores==6 |
#                          time_by_N$Cores==8 |
#                          time_by_N$Cores==10 |
#                          time_by_N$Cores==0,]
time_by_N$Cores <- factor(time_by_N$Cores)



df <- melt(time_by_N,measure.vars = 3:4,variable.name = 'Evaluation',
           value.name = 'ms')
df$ms <- as.numeric(df$ms)
df$Seconds <- df$ms /1000
df <- df[,-4]
df$Evaluation <- factor(df$Evaluation)
#df <- df[df$N!=2000,]

df$Cores <- revalue(df$Cores, c("0"="GPU"))
df$Cores = factor(df$Cores,levels(df$Cores)[c(2:11,1)])






df1 <- df[df$Evaluation=="Likelihood",]

gg <- ggplot(df1,aes(x=N,y=Seconds,color=Cores)) +
  geom_smooth(se=FALSE) +
  ylab('Seconds per evaluation') +
  xlab('Number of data points')+
  ggtitle('Likelihood')+
  coord_cartesian(ylim=c(0,10),xlim=c(0,10000)) +
  scale_x_continuous(labels=c("0","3.1e+06","1.3e+07","2.8e+07","5e+07"))+
  theme_classic()  

df2 <- df[df$Evaluation=="Gradient",]
gg2 <- ggplot(df2,aes(x=N,y=Seconds,color=Cores)) +
  geom_smooth(se=FALSE) +
  ylab('') +
  xlab('Number of data points')+
  ggtitle('Gradient')+
  coord_cartesian(ylim=c(0,10),xlim=c(0,10000)) +
  scale_x_continuous(labels=c("0","3.1e+06","1.3e+07","2.8e+07","5e+07"))+
  theme_classic() 

ggsave('SecondsByN.pdf',grid_arrange_shared_legend(gg,gg2,position="right"),
       device = 'pdf',path = '~/FluMDS/figures/',
       width = 7,height=3)
