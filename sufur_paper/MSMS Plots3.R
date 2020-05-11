library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
set.seed(50)

##USER INPUTS
#Minimum relative intensity (%)
minRel <- 20
fileName <- "Peaklist2.csv"


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
df$C <- as.numeric(gsub("C","",df$C))

#Identifier
df$ID <- paste(df$Target,df$Oil)

#Add relative intensity, filter by minRel
max <- aggregate(I~ID,data = df, max)
colnames(max) <- c("ID","max")
df <- merge(df,max,intersect(names(df),names(max)))
df$rel <- round(((df$I/df$max)*100),1)
df <- df[df$rel >= minRel,]

#Setup plot
g<- ggplot(data=df, aes(x=C, y=rel, fill=rdb))
g + geom_bar(stat="identity", width=.7, alpha=.5) + 
  facet_grid(Target ~ Oil) + 
  labs(x="Carbon number",
       y="Relative intensity",
       title="FTMS/MS spectra",
       subtitle="Class",
       caption = "...") +
  coord_cartesian(xlim = c(15,30)) +
  geom_text_repel(aes(label=""),
                  size = 3,
                  color = "red",
                  box.padding = 0.35,
                  point.padding = 0.6,
                  direction = "both",
                  segment.color = "grey50",
                  force = 0.5) +
  theme_classic(base_size = 16)

#