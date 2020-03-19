setwd("~/shot_spotter/")

library(ggplot2)
library(readr)
library(reshape2)
library(scales)
library(RColorBrewer)
library(plyr)
library(grid)
library(gridExtra)


reverselog_trans <- function(base = exp(1)) {
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)
  trans_new(paste0("reverselog-", format(base)), trans, inv, 
            log_breaks(base = base), 
            domain = c(1e-100, Inf))
}

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

###################################################################

bench_df <- read.table("~/shot_spotter/data/report.txt", quote="\"", comment.char="")
bench_df <- bench_df[,c(2,3,7)]
bench_df$V2[c(1,2,14,15)] <- 1

bench_df$V7[bench_df$V2==0] <- bench_df$V7[bench_df$V2==0] / 100
colnames(bench_df) <- c("Threads", "SIMD", "Seconds")
bench_df$Seconds <- bench_df$Seconds / 1000

bench_df$SIMD <- rep(c("Non-vectorized","SSE",rep("AVX",10),"GPU"),2)
colnames(bench_df)[1] <- "Cores"
df1 <- bench_df

df1$Speedup <- rep(0,dim(df1)[1])
df1$Seconds <- (df1$Seconds[1:(length(df1$Seconds)/2)] + df1$Seconds[(length(df1$Seconds)/2 + 1):length(df1$Seconds)]) /2
df1 <- df1[1:(length(df1$Seconds)/2),]
df1$Speedup <- df1$Seconds[3] / df1$Seconds 

df1$Cores <- as.numeric(df1$Cores)
df1$SIMD <- factor(df1$SIMD)
df2 <- df1[df1$Cores==0,]
df2 <- rbind(df2,df2,df2,df2,df2,df2,df2,df2,df2,df2)
df2$Cores <- c(1,2,4,6,8,10,12,14,16,18)
df3 <- df1[df1$Cores!=0 & df1$SIMD != "AVX",]
df <- df1[df1$Cores!=0 & df1$SIMD == "AVX",]


df$SIMD <- droplevels.factor(df$SIMD)
colnames(df)[2] <- "Method"



gg <- ggplot(df, aes(x=Cores,y=Speedup)) +
  geom_line(color=gg_color_hue(11)[7],size=1.1) +
  geom_point(data=df3[1,],mapping = aes(x=Cores,y=Speedup),inherit.aes = FALSE,color=gg_color_hue(11)[1]) +
  geom_point(data=df3[2,],mapping = aes(x=Cores,y=Speedup),inherit.aes = FALSE,color=gg_color_hue(11)[4]) +
  geom_text(aes(x=3.95,y=0.65,label="Non-vectorized"),
             inherit.aes = FALSE,show.legend = FALSE,
             check_overlap = TRUE,color=gg_color_hue(11)[1]) +
  geom_text(aes(x=2.1,y=0.85,label="SSE"),
            inherit.aes = FALSE,show.legend = FALSE,
            check_overlap = TRUE,color=gg_color_hue(11)[4]) +
  geom_text(aes(x=2.1,y=3,label="AVX"),
            inherit.aes = FALSE,show.legend = FALSE,
            check_overlap = TRUE,color=gg_color_hue(11)[7]) +
  geom_text(aes(x=2.1,y=87,label="GPU"),
            inherit.aes = FALSE,show.legend = FALSE,
            check_overlap = TRUE,color=gg_color_hue(11)[11]) +
  geom_line(data=df2,aes(x=Cores,y=Speedup),inherit.aes = FALSE, color=gg_color_hue(11)[11],
            size=1.1) +
  scale_x_continuous(breaks=c(1,2,4,6,8,10,12,14,16,18),
                     labels = c("1","2",'4','6','8','10','12',"14","16","18"))+
  scale_y_continuous(trans = "log2",breaks=c(0.5,1,2,4,8,16,32,64,128),
                     labels=c("1/2","1","2","4","8","16","32","64","128")) +
  ylab("Speedup over single-threaded AVX") +
  xlab("CPU threads") +
  theme_classic()
gg
# 
# ggsave('performFigure.pdf', gg, device = 'pdf', path = 'figures/',
#        width = 6,height=4)


time_by_N <- read_table2("data/time_by_N.txt", 
                         col_names = FALSE)
time_by_N <- time_by_N[,-c(1,3)]
colnames(time_by_N) <- c('Cores',
                         'N','Dimension',
                         'its',
                         'Likelihood',
                         'Gradient')

time_by_N$Likelihood <- time_by_N$Likelihood / time_by_N$its

time_by_N <- time_by_N[,-3]
time_by_N <- time_by_N[,-3]
time_by_N <- time_by_N[,-4]


# time_by_N <- time_by_N[time_by_N$Cores==1 |
#                          time_by_N$Cores==2 |
#                          time_by_N$Cores==4 |
#                          time_by_N$Cores==6 |
#                          time_by_N$Cores==8 |
#                          time_by_N$Cores==10 |
#                          time_by_N$Cores==0,]
time_by_N$Cores <- factor(time_by_N$Cores)

df <- time_by_N
df$Seconds <- time_by_N$Likelihood /1000
df <- df[,-3]

df$Cores <- revalue(df$Cores, c("0"="GPU"))
df$Threads = factor(df$Cores,levels(df$Cores)[c(2:11,1)])
df

gg2 <- ggplot(df,aes(x=N,y=Seconds,color=Threads)) +
  geom_smooth(se=FALSE) +
  ylab('Seconds per likelihood evaluation') +
  xlab('Number of data points')+
  coord_cartesian(ylim=c(0,10),xlim=c(10000,85000)) +
  #scale_x_continuous(labels=c("0","3.1e+06","1.3e+07","2.8e+07","5e+07"))+
  theme_classic()  
gg2


ggsave('performFigure.pdf',grid.arrange(gg,gg2,ncol=2) , device = 'pdf', path = 'figures/',
       width = 9,height=4)

