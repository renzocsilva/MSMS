library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
set.seed(50)

##USER INPUTS
#Minimum relative intensity (%)
minRel <- 10
fileName <- "C40_S3.csv"
filter_target <- TRUE
filter_carbon <- TRUE
filter_DBE <- FALSE
dbeoff <- 10
filter_S <- TRUE



#Read file
setwd("~/R/MSMS")
df <- read.csv(fileName, header=TRUE)

#Identifier
df$ID <- paste(df$Target,df$Oil)

#Remove duplicates
df <- df[complete.cases(df),]

#Filter target peak
if (filter_target == TRUE){
  df <- df[!is.element(df$Ion.Formula,df$Target),]
}
  
#Change variable names and classes
colnames(df)[2] <- "mz"
df$rdb <- factor(df$rdb)
df$Formula <- df$Ion.Formula
df$Formula <- gsub("S$","S1",df$Formula)
df$Target <- gsub("S$","S1",df$Target)

#Split Chemistry Ions
df <- separate(data = df, col = Formula, into = c("C","Other"), sep = "H")
df$C <- as.numeric(gsub("C","",df$C))
df <- separate(data = df, col = Other, into = c("H","S"), sep = "S")
df$H <- as.numeric(df$H)
df$S[is.na(df$S)] <- 0
df$S <- as.numeric(df$S)
df[is.na(df$S),df$S] <- 0
df$DBE <- (df$C-df$H/2)+1

#Split Chemistry Parent, calculate DBE
df$targetSplit <- df$Target
df <- separate(data = df, col = targetSplit, into = c("p_C","p_Other"), sep = "H")
df$p_C <- as.numeric(gsub("C","",df$p_C))
df <- separate(data = df, col = p_Other, into = c("p_H","p_S"), sep = "S")
df$p_H <- as.numeric(df$p_H)
df$p_S <- as.numeric(df$p_S)
df$p_DBE <- (df$p_C-df$p_H/2)+1
df$del_DBE <- df$p_DBE - df$DBE

#Filter by carbon
if (filter_carbon == TRUE){
  df <- df[df$C <= df$p_C,]
}

#Filter by DBE
if (filter_DBE == TRUE){
  df <- df[df$del_DBE <= abs(dbeoff),]
}

#Filter by S
if (filter_S == TRUE){
  df <- df[df$S <= df$p_S,]
}

#Add relative intensity  
maximum <- aggregate(I~ID,data = df, max)
colnames(maximum) <- c("ID","maximum")
df <- merge(df,maximum,intersect(names(df),names(maximum)))
df$rel <- round(((df$I/df$maximum)*100),1)

#Filter by minRel
df <- df[df$rel >= minRel,]

#Setup plot
g <- ggplot(data=df, aes(x=C, y=rel, fill=as.character(S)))
g + geom_bar(stat="identity", width=.7, alpha=.5) + 
  facet_grid(Target ~ Oil) + 
  labs(x="Carbon number",
       y="Relative intensity",
       title="FTMS/MS spectra",
       subtitle="Class",
       caption = "...") +
  #coord_cartesian(xlim = c(15,40)) +
  geom_text(aes(label=del_DBE),
                  size = 3,
                  color = "red",
                  box.padding = 0.0,
                  point.padding = 0.5,
                  direction = "y",
                  segment.color = "grey50",
                  force = 0.3) +
  theme_light(base_size = 16) +
  scale_x_continuous(breaks=seq(min(df$C),max(df$C)+2,2))

dev.copy(png,filename=paste(fileName,".png"), height=1200, width=800, bg="transparent")
dev.off()