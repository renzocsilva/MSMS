library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
set.seed(50)

##USER INPUTS
#Minimum relative intensity (%)
minRel <- 20
fileName <- "Peaklist4.csv"


#Read file
setwd("~/R/MSMS")
df <- read.csv(fileName, header=TRUE)

#Remove duplicates
df <- df[complete.cases(df),]

#Change variable names and classes
colnames(df)[2] <- "mz"
df$rdb <- factor(df$rdb)
df$Formula <- df$Ion.Formula
df <- separate(data = df, col = Ion.Formula, into = c("C","Other"), sep = "H")

#Identifier
df$ID <- paste(df$Target,df$Oil)

#Add relative intensity, filter by minRel
max <- aggregate(I~ID,data = df, max)
colnames(max) <- c("ID","max")
df <- merge(df,max,intersect(names(df),names(max)))
df$rel <- round(((df$I/df$max)*100),1)
df <- df[df$rel >= 10,]

#Setup plot
g<- ggplot(data=df, aes(x=mz, y=rel, fill=rdb))
g + geom_bar(stat="identity", width=10, alpha=.5) + 
  facet_grid(Target ~ Oil) + 
  labs(x="m/z",
       y="Relative intensity",
       title="FTMS/MS spectra",
       subtitle="Class",
       caption = "...") +
  coord_cartesian(xlim = c(150, 500)) +
  geom_text_repel(aes(label=C),
                  size = 3,
                  color = "red",
                  box.padding = 0.35,
                  point.padding = 0.6,
                  direction = "both",
                  segment.color = "grey50",
                  force = 0.5) +
  theme_classic(base_size = 16)

#