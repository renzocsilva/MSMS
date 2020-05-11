
fileName <- "C40_DBEvsS.csv"

#Read file
setwd("~/R/MSMS")
df <- read.csv(fileName, header=TRUE)

g <- ggplot(data=df, aes(x=DBE, y=RMI, fill=S))
g + geom_bar(stat="identity", width=.7, alpha=.8) + 
  facet_grid(S ~ Oil, scales = "free") +
  labs(x="DBE number",
       y="RMI (% of Sample)",
       title="C#40 Class S.1-5 DBE distribution") +
  theme_light(base_size = 16,base_family = "Arial") +
  scale_x_continuous(limits=c(0,22),breaks=seq(from=0,to=22,by=2)) +
  theme(strip.background = element_rect(color = "gray50", fill = "white"),
        strip.text.x = element_text(colour = "black", face = "bold"),
        strip.text.y = element_text(colour = "grey50", face = "bold"),
        plot.title = element_text(hjust = 0.5),
        legend.position="none")



#dev.copy(png,filename=paste(fileName,".png"), height=600, width=1200, bg="transparent")
#dev.off()
