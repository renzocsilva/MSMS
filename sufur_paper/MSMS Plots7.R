library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
set.seed(50)


#"C26_30_S1_DBE 1.csv"
#"C27_30_S1.csv"
#"C40_S2.csv"
#"C40_S3.csv"
#"S2.CSV"
#"S2_bridge.csv"


##USER INPUTS
#Minimum relative intensity (%)
minRel <- 10
fileName <- "C27_30_S1.csv"
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
df$Oil <- gsub("JH","Jianghan",df$Oil)
df$Oil <- gsub("RP","Rozel Point",df$Oil)

#Split Chemistry Ions
df <- separate(data = df, col = Formula, into = c("C","Other"), sep = "H")
df$C <- as.numeric(gsub("C","",df$C))
df <- separate(data = df, col = Other, into = c("H","S"), sep = "S")
df$H <- as.numeric(df$H)
df$S[is.na(df$S)] <- 0
df$S <- as.numeric(df$S)
df$DBE <- (df$C-df$H/2)+1

#Split Chemistry Parent, calculate DBE
df$targetSplit <- df$Target
df <- separate(data = df, col = targetSplit, into = c("p_C","p_Other"), sep = "H")
df$p_C <- as.numeric(gsub("C","",df$p_C))
df <- separate(data = df, col = p_Other, into = c("p_H","p_S"), sep = "S")
df$p_H <- as.numeric(df$p_H)
df$p_S <- as.numeric(df$p_S)
df$p_DBE <- (df$p_C-df$p_H/2)+1

#Calculate parent-daughter differentials
df$del_DBE <- df$DBE - df$p_DBE
df$del_C <- df$C - df$p_C
df$del_S <- df$S - df$p_S

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
df$del_S <- as.character(df$del_S)
df$Target<-paste("S",df$p_S," DBE",df$p_DBE," C#",df$p_C, sep = "")
label_label <- paste("DELTA", " DBE")

g <- ggplot(data=df, aes(x=del_C, y=rel, fill=del_S))
g + geom_bar(stat="identity", width=1, alpha=.5, color = "gray50") + 
  facet_grid(Target ~ Oil) + #scales = "free"
  labs(x=expression(paste(Delta," C",sep="")),
       y="Relative intensity (%)",
       title="FTICR-MS/MS spectra",
       fill = expression(paste(Delta, "S"))
       #="Class",
       ) +
  #coord_cartesian(xlim = c(15,40)) +
  geom_text_repel(aes(label=del_DBE),
                  size = 4,
                  color = "red",
                  box.padding = 0.0,
                  point.padding = 0.0,
                  direction = "y",
                  segment.color = "grey50",
                  force = 0.01) +
  theme_light(base_size = 16,base_family = "Arial") +
  scale_x_continuous(breaks=seq(min(df$del_C),max(df$del_C),2)) +
  geom_vline(xintercept=0, size = 4, color = "gray25", alpha=.25) +
  annotate("text",
           x = -14,
           y = 90,
           size = 5,
           label = expression(paste(Delta, " DBE")),
           color = "red", parse = TRUE) +
  theme(strip.background = element_rect(color = "gray50", fill = "white"),
        strip.text.x = element_text(colour = "black", face = "bold"),
        strip.text.y = element_text(colour = "grey50", face = "bold"),
        plot.title = element_text(hjust = 0.5))

dev.copy(png,filename=paste(fileName,".png"), height=800, width=800, bg="transparent")
dev.off()
