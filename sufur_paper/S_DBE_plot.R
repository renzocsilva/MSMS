
fileName <- "DBE_vs_S.csv"

#Read file
setwd("~/R/MSMS")
df <- read.csv(fileName, header=TRUE)

g <- ggplot(data=df, aes(x=S, y=DBE, size=INT, color=INT))
g + geom_point(alpha=.2) + 
    facet_grid(.~OIL, scales = "free") +
    scale_y_continuous(limits=c(0,10),breaks=seq(from=0,to=10,by=1)) +
    theme(strip.background = element_rect(color = "gray50", fill = "white"),
          strip.text.x = element_text(colour = "black", face = "bold"),
          strip.text.y = element_text(colour = "grey50", face = "bold"),
          plot.title = element_text(hjust = 0.5),
          legend.position="none")

labs(x="DBE number",
       y="RMI (% of Class)",
       title="C#40 Class S.1-5 DBE distribution") +
  theme_light(base_size = 16,base_family = "Arial") +
  
  



#dev.copy(png,filename=paste(fileName,".png"), height=600, width=1200, bg="transparent")
#dev.off()
