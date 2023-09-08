library(ggplot2)

first<-read.csv('example_lod.csv')
first$roll<- cumsum(first$Position)
first$Chromosome<-as.factor(first$Chromosome)

center<- first %>% group_by(Chromosome) %>% summarize(center=( max(roll) + min(roll) ) / 2 ) 
dat = data.frame(a = c(0,4000,8000,12000,16000,20000,24000,28000,32000,36000,40000,44000,48000,52000,56000,60000,64000,68000,72000,76000,80000))
dat$Chromosome = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21)

df2 <- merge(x=dat,y=first, 
             by="Chromosome")
df2$Chromosome[df2$Chromosome==1] <- "1A"
df2$Chromosome[df2$Chromosome==2] <- "1B"
df2$Chromosome[df2$Chromosome==3] <- "1D"
df2$Chromosome[df2$Chromosome==4] <- "2A"
df2$Chromosome[df2$Chromosome==5] <- "2B"
df2$Chromosome[df2$Chromosome==6] <- "2D"
df2$Chromosome[df2$Chromosome==7] <- "3A"
df2$Chromosome[df2$Chromosome==8] <- "3B"
df2$Chromosome[df2$Chromosome==9] <- "3D"
df2$Chromosome[df2$Chromosome==10] <- "4A"
df2$Chromosome[df2$Chromosome==11] <- "4B"
df2$Chromosome[df2$Chromosome==12] <- "4D"
df2$Chromosome[df2$Chromosome==13] <- "5A"
df2$Chromosome[df2$Chromosome==14] <- "5B"
df2$Chromosome[df2$Chromosome==15] <- "5D"
df2$Chromosome[df2$Chromosome==16] <- "6A"
df2$Chromosome[df2$Chromosome==17] <- "6B"
df2$Chromosome[df2$Chromosome==18] <- "6D"
df2$Chromosome[df2$Chromosome==19] <- "7A"
df2$Chromosome[df2$Chromosome==20] <- "7B"
df2$Chromosome[df2$Chromosome==21] <- "7D"

df2$Chromosome<-as.factor(df2$Chromosome)
df2$plot<-df2$a+df2$roll  

center<- df2 %>% group_by(Chromosome) %>% summarize(center=( max(plot) + min(plot) ) / 2 ) 

one<-ggplot(data = df2 , aes(x=plot, y=LOD, group=Chromosome, colour=Chromosome))+geom_line()+scale_color_manual(values = rep(c("grey", "dodgerblue4","orange2","chartreuse1","grey0","magenta2","gold"), 22 ))+geom_abline(intercept = 4.7, slope = 0)+scale_x_continuous( label = center$Chromosome , breaks = center$center) 
one+
  scale_y_continuous(expand = c(0, 0) ) +     # remove space between plot area and x axis
  
  # Custom the theme:
  theme_bw() +
  theme( 
    legend.position="none",
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )



