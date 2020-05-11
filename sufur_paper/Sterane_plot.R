library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
set.seed(50)

fileName <- "C27_30_sterane.csv"

#Read file
setwd("~/R/MSMS")
df <- read.csv(fileName, header=TRUE)

g <- ggplot(data=df, aes(x=C_, y=Rel, fill = Oil))


g + geom_bar(stat="identity", width=.7, alpha = .7) + 
  facet_grid(Oil ~ Class, scales = "free") + 
    labs(x=paste("C","#",sep=""),
       y="RMI (% of Sample)") + 
  coord_cartesian(xlim = c(26.5,30.5)) +
  theme_light(base_size = 16,base_family = "Arial") +
  theme(strip.background = element_rect(color = "gray50", fill = "white"),
        strip.text.x = element_text(colour = "black", face = "bold"),
        strip.text.y = element_text(colour = "grey50", face = "bold"),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values=c("#FF6600", "#006666"))


dev.copy(png,filename=paste(fileName,".png"), height=600, width=600, bg="transparent")
dev.off()