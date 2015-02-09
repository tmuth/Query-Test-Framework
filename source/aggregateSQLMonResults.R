library(XML)
library(ggplot2)
library(scales)
library(reshape)
library(gridExtra)
library(tools)
library(stringr)
library(sjPlot)
library(lubridate)
library(plyr)

#filePattern <- "^.+\\.html"
filePattern <- "^d7.+\\.Rda$"


sqlDataMonFiles <- list.files(pattern=filePattern,recursive=TRUE)


sqlMonResults.agg <- data.frame()
for (f in sqlDataMonFiles) {
  load(f) 
  
  sqlMonResults.agg <- rbind(sqlMonResults.agg,sqlMonResults_DF)
}


sqlMonResults.agg$IMO <- "no"
sqlMonResults.agg[with(sqlMonResults.agg, grepl("IMO", test)),]$IMO<-"yes"

sqlMonResults.agg <- ddply(sqlMonResults.agg,.(name),transform,testnum = as.integer(as.factor(test)))



sqlMonResults.agg.melt <-  melt(sqlMonResults.agg, id.var = c("name","testnum","IMO"), measure.var = c("sqlplus.elap"))
sqlMonResults.agg.melt$IMO <- factor(sqlMonResults.agg.melt$IMO)

#sqlMonResults.agg <- subset(sqlMonResults.agg,testnum %in% c(2,3,4,6,7,8))
#sqlMonResults.agg.melt <- subset(sqlMonResults.agg.melt,testnum %in% c(2,3,4,6,7,8))
sqlMonResults.agg <- subset(sqlMonResults.agg,testnum > 1 & testnum != 10)
sqlMonResults.agg.melt <- subset(sqlMonResults.agg.melt,testnum > 1 & testnum != 10)

pdf("SQL-MON-Aggregates.pdf", width = 11, height = 8.5, useDingbats=FALSE)

p1 <- ggplot(data=sqlMonResults.agg.melt,aes(x=name,y=value,group=testnum))+
  geom_bar(position="dodge",stat="identity",aes(fill=IMO),color="#cccccc")+
  geom_text(aes(label=value),size=2.5,stat='identity',position = position_dodge(width=1))+
  xlab("Query Name")+
  ylab("Seconds")+
  labs(title="2 runs In-Memory vs 1 run No In-Memory")

p1 


p2 <- ggplot(data=sqlMonResults.agg.melt,aes(x=testnum,y=value,group=name))+
  geom_bar(position="dodge",stat="identity",aes(fill=IMO),color="#cccccc")+
  #geom_text(aes(label=value),size=2.5,stat='identity',position = position_dodge(width=1))+
  facet_wrap(~ name,scales = "free_y")+
  xlab("Query Name")+
  ylab("Seconds")+
  labs(title="2 runs In-Memory vs 1 run No In-Memory")


p2

dev.off()
