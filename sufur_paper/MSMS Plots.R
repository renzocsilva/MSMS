#Read file
setwd("~/R/MSMS")
df <- read.csv("Peaklist.csv", header=TRUE)

#setup plot
library(ggplot2)
g<- ggplot(data=df, aes(x=m.z, y=rel, fill=parent))
g + geom_bar(stat="identity", width=.8) + 
  facet_grid(parent~.) + 
  labs(x="m/z",
       y="Relative intensity",
       title="MS/MS",
       subtitle="Class",
       caption = "...")


#