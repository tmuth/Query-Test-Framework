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
library(dplyr)
library(ggthemes)

#filePattern <- "^.+\\.html"
filePattern <- "^olap.+\\.Rda$"


sqlDataMonFiles <- list.files(pattern=filePattern,recursive=TRUE)


sqlMonResults.agg <- data.frame()
for (f in sqlDataMonFiles) {
  load(f) 
  
  sqlMonResults.agg <- rbind(sqlMonResults.agg,sqlMonResults_DF)
}

#sqlMonResults_DF  <- subset( sqlMonResults_DF , select = -c(sql) )
#write.csv(x=sqlMonResults_DF,file="sqlmon.stats.csv",row.names=FALSE)

sqlMonResults.agg  <- subset( sqlMonResults.agg , select = -c(sql) )

sqlMonResults.agg$name <- str_extract(sqlMonResults.agg$name,'^[0-9]+')



# label the tests
sqlMonResults.agg$IMC <- "no"
sqlMonResults.agg[with(sqlMonResults.agg, grepl("\\.IMC", test)),]$IMC<-"yes"
sqlMonResults.agg$vector <- "yes"
sqlMonResults.agg[with(sqlMonResults.agg, grepl("novector", test)),]$vector<-"no"

sqlMonResults.agg$test.name <- paste(
  ifelse(sqlMonResults.agg$IMC == 'yes','IMC','No.IMC'),
  ifelse(sqlMonResults.agg$vector == 'yes','Vector','No.Vector'),
  sep='-')

#sqlMonResults.agg <- ddply(sqlMonResults.agg,.(name),transform,testnum = as.integer(as.factor(test)))

sqlMonResults.agg <- sqlMonResults.agg  %>%
  group_by(test) %>%
  mutate(
    min_start=min(start)) %>%
  ungroup() %>%
  group_by(name) %>%
  arrange(min_start) %>%
  mutate(testnum = as.integer(as.factor(min_start))) %>%
  ungroup() %>%
  arrange(name,testnum)




sqlMonResults.agg$duration <- as.numeric(sqlMonResults.agg$duration)
# remove outliers
sqlMonResults.agg <- sqlMonResults.agg  %>%
  group_by(name,test.name) %>%
  mutate(
         dur_sd=sd(duration),
         dur_median=median(duration),
         dur_mad=mad(duration)) %>%
  ungroup()
  


sqlMonResults.agg$dur_mad <- ifelse(sqlMonResults.agg$dur_mad==0,sqlMonResults.agg$dur_median,sqlMonResults.agg$dur_mad)

sqlMonResults.agg <- sqlMonResults.agg %>%
  filter(duration < (dur_median+(3*dur_mad)))


sqlMonResults.agg <- sqlMonResults.agg  %>%
  group_by(testnum) %>%
  mutate(
    max_dop=max(parallel_degree),
    avg_dop=mean(parallel_degree)
    ) %>%
  ungroup()


sqlMonResults.agg$name <- factor(as.numeric(sqlMonResults.agg$name))

sqlMonResults.agg <- subset(sqlMonResults.agg,name != '18a')

sqlMonResults.agg.narrow <- sqlMonResults.agg[ , which(names(sqlMonResults.agg) %in% 
                                                         c("name","duration","test.name","dur_median","dur_mad"))]


sqlMonResults.agg.melt <-  melt(sqlMonResults.agg, id.var = c("name","testnum","test.name"), measure.var = c("duration"))
sqlMonResults.agg.melt$test.name <- factor(sqlMonResults.agg.melt$test.name)




#sqlMonResults.agg <- subset(sqlMonResults.agg,testnum %in% c(2,3,4,6,7,8))
#sqlMonResults.agg.melt <- subset(sqlMonResults.agg.melt,testnum %in% c(2,3,4,6,7,8))
#sqlMonResults.agg <- subset(sqlMonResults.agg,testnum > 1 & testnum != 10)
#sqlMonResults.agg <- subset(sqlMonResults.agg,name != '17a' & name != '17')
#sqlMonResults.agg.melt <- subset(sqlMonResults.agg.melt,testnum > 1 & testnum != 10)
#sqlMonResults.agg.melt <- subset(sqlMonResults.agg.melt,name != '17a' & name != '17')





sqlMonResults.agg.by.q <- sqlMonResults.agg %>%
  ungroup() %>%
  group_by(name,test.name) %>%
  summarise(avg_time=mean(duration)) %>%
  arrange(name)

sqlMonResults.dbTime.agg.by.q <- sqlMonResults.agg %>%
  ungroup() %>%
  group_by(name,test.name) %>%
  summarise(avg_dbTime=mean(dbTime)) %>%
  arrange(name)

sqlMonResults.dop.agg.by.q <- sqlMonResults.agg %>%
  ungroup() %>%
  group_by(name,test.name) %>%
  summarise(avg_dop=mean(parallel_degree)) %>%
  arrange(name)



sqlMonResults.agg.by.q2 <- sqlMonResults.agg.by.q %>%
  filter(test.name %in% c("IMC-Vector","No.IMC-No.Vector")) %>%
  group_by(name) %>%
  arrange(avg_time) %>%
  mutate(rank = as.integer(as.factor(avg_time))) %>%
  filter(test.name=='IMC-Vector' & rank == 2)


sqlMonResults.agg.by.test.name <- sqlMonResults.agg.by.q %>%
  group_by(test.name) %>%
  summarise(total_time=sum(avg_time))

sqlMonResults.agg.dbTime.by.test.name <- sqlMonResults.dbTime.agg.by.q %>%
  group_by(test.name) %>%
  summarise(total_dbTime=sum(avg_dbTime))

sqlMonResults.agg.dop.by.test.name <- sqlMonResults.dop.agg.by.q  %>%
  group_by(test.name) %>%
  summarise(total_dop=sum(avg_dop))


number_ticks <- function(n) {function(limits) pretty(limits, n)}




myTheme <- theme_stata(scheme = "s2color") +
  #myTheme <- theme_few() +
  #myTheme <- theme_bw() +
  
  theme(legend.position =    "bottom",
        #plot.margin =        unit(c(3, 3, 3, 3), "lines"),
        axis.title.y = element_text(vjust = .6),
        text =               element_text(family="sans",face = "plain",
                                          colour = "black", size = 8,
                                          hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9),
        axis.text.y =       element_text(angle = 0),
        panel.margin =       unit(0.25, "lines"),
        panel.grid.major = element_line(colour="#dedede", size = 0.2,linetype = "dotted"),
        panel.grid.minor = element_line(colour="#dedede", size = 0.1,linetype = "dotted"),
        axis.text.x=element_text(angle=-30, hjust=-.1,vjust=1,size=6),
        legend.key.size =    unit(0.6, "lines")
  )

theme_set(myTheme)



pdf("SQL-MON-Aggregates-summary.pdf", width = 11, height = 8.5, useDingbats=FALSE)

p1 <- ggplot(data=sqlMonResults.agg.by.test.name,aes(x=test.name,y=total_time,group=test.name))+
  geom_bar(position="dodge",stat="identity",aes(fill=test.name),color="#cccccc")+
  geom_text(aes(label=round(total_time,1)),size=2,alpha=0.4,stat='identity',position = position_dodge(width=1))+
  #geom_text(aes(label=value),size=2.5,stat='identity',position = position_dodge(width=1))+
  #xlab("Query Name")+
  ylab("Sum of Average Query Times")+
  labs(title="In-Memory vs No In-Memory - All Queries - Elapsed Time")+
  scale_y_continuous(breaks=number_ticks(10))+
  scale_fill_stata()
#   scale_y_continuous(breaks = round(seq(1, max(sqlMonResults.agg.by.imo$total_time), by = 5),1),
#                      minor_breaks = round(seq(1, max(sqlMonResults.agg.by.imo$total_time), by = 1),1))

p1 



p1.2 <- ggplot(data=sqlMonResults.agg.dbTime.by.test.name,aes(x=test.name,y=total_dbTime,group=test.name))+
  geom_bar(position="dodge",stat="identity",aes(fill=test.name),color="#cccccc")+
  geom_text(aes(label=round(total_dbTime,1)),size=2,alpha=0.4,stat='identity',position = position_dodge(width=1))+
  #geom_text(aes(label=value),size=2.5,stat='identity',position = position_dodge(width=1))+
  #xlab("Query Name")+
  ylab("Sum of Average Query Database Time")+
  labs(title="In-Memory vs No In-Memory - All Queries - Database Time")+
  scale_y_continuous(breaks=number_ticks(10))+
  scale_fill_stata()
#   scale_y_continuous(breaks = round(seq(1, max(sqlMonResults.agg.by.imo$total_time), by = 5),1),
#                      minor_breaks = round(seq(1, max(sqlMonResults.agg.by.imo$total_time), by = 1),1))

p1.2 


p1.3 <- ggplot(data=sqlMonResults.agg.dop.by.test.name,aes(x=test.name,y=total_dop,group=test.name))+
  geom_bar(position="dodge",stat="identity",aes(fill=test.name),color="#cccccc")+
  geom_text(aes(label=round(total_dop,1)),size=2,alpha=0.4,stat='identity',position = position_dodge(width=1))+
  #geom_text(aes(label=value),size=2.5,stat='identity',position = position_dodge(width=1))+
  #xlab("Query Name")+
  ylab("Degree of Parallelism")+
  labs(title="In-Memory vs No In-Memory - All Queries - Sum of Degree of Parallelism (DOP)")+
  scale_y_continuous(breaks=number_ticks(10))+
  scale_fill_stata()
#   scale_y_continuous(breaks = round(seq(1, max(sqlMonResults.agg.by.imo$total_time), by = 5),1),
#                      minor_breaks = round(seq(1, max(sqlMonResults.agg.by.imo$total_time), by = 1),1))

p1.3 

p1.4 <- ggplot(data=sqlMonResults.dop.agg.by.q,aes(x=test.name,y=avg_dop,group=test.name))+
  geom_boxplot(aes(colour=test.name))+
  geom_jitter(alpha=.5,size=1,position = position_jitter(width = .2,height=0),aes(colour="gray"))+
  #geom_text(aes(label=round(total_dop,1)),size=2,alpha=0.4,stat='identity',position = position_dodge(width=1))+
  #geom_text(aes(label=value),size=2.5,stat='identity',position = position_dodge(width=1))+
  #xlab("Query Name")+
  ylab("Degree of Parallelism")+
  labs(title="In-Memory vs No In-Memory - All Queries - DOP by Test by Query Boxplot")+
  scale_y_continuous(breaks=number_ticks(10))+
  scale_color_stata()
#   scale_y_continuous(breaks = round(seq(1, max(sqlMonResults.agg.by.imo$total_time), by = 5),1),
#                      minor_breaks = round(seq(1, max(sqlMonResults.agg.by.imo$total_time), by = 1),1))

p1.4

sqlMonResults.agg.by.q.tmp <- subset(sqlMonResults.agg.by.q,as.numeric(name) > 50)

p2 <- ggplot(data=sqlMonResults.agg.by.q.tmp,aes(x=name,y=avg_time,group=test.name))+
  geom_bar(position="dodge",stat="identity",aes(fill=test.name),color="#cccccc")+
  #geom_text(aes(label=value),size=2.5,stat='identity',position = position_dodge(width=1))+
  geom_text(aes(label=round(avg_time,1)),size=2,alpha=0.4,stat='identity',position = position_dodge(width=1))+
  #xlab("Query Name")+
  ylab("Average Query Time")+
  labs(title="In-Memory vs No In-Memory - Avg Time by Query")+
  scale_y_continuous(breaks=number_ticks(10))+
  scale_fill_stata()
#   scale_y_continuous(breaks = round(seq(1, max(sqlMonResults.agg.by.q$avg_time), by = 5),1),
#                      minor_breaks = round(seq(1, max(sqlMonResults.agg.by.q$avg_time), by = 1),1))

p2




plot2 <- function(DF_IN,title_in){
  p <- ggplot(data=DF_IN,aes(x=test.name,y=avg_time,group=test.name))+
    geom_bar(position="dodge",stat="identity",aes(fill=test.name),color="#cccccc")+
    #geom_text(aes(label=value),size=2.5,stat='identity',position = position_dodge(width=1))+
    geom_text(aes(label=round(avg_time,1)),size=2,alpha=0.4,stat='identity',position = position_dodge(width=1))+
    #xlab("Query Name")+
    facet_wrap(~ name,scales = "free_y")+
    ylab("Average Query Time (seconds)")+
    labs(title=title_in)+
    scale_y_continuous(breaks=number_ticks(10))+
    scale_fill_stata()+
    theme(axis.text.x=element_text(angle=-30, hjust=-.1,vjust=1,size=6))
  return(p)
}

plotTitle <- "In-Memory vs No In-Memory - Avg Time - Facet by Query"
plot2(subset(sqlMonResults.agg.by.q,as.numeric(name) <= 50),plotTitle)
plot2(subset(sqlMonResults.agg.by.q,as.numeric(name) > 50),plotTitle)

sqlMonResults.agg.by.q.tmp <- subset(sqlMonResults.agg.by.q, name %in% sqlMonResults.agg.by.q2$name)

plotTitle <- "Tests Where No-IMC-No-Vector Is Faster Than IMC-Vector"
plot2(sqlMonResults.agg.by.q.tmp,plotTitle)






plot4 <- function(DF_IN){
  p <- ggplot(data=DF_IN ,aes(x=testnum,y=value,group=name))+
    geom_bar(position="dodge",stat="identity",aes(fill=test.name),color="#cccccc")+
    geom_text(aes(label=value),size=2,alpha=0.4,stat='identity',position = position_dodge(width=1))+
    #geom_text(aes(label=testnum),size=1,alpha=0.2,stat='identity',position = position_dodge(width=1))+
    facet_wrap(~ name,scales = "free_y")+
    scale_x_discrete()+
    scale_y_continuous(breaks=number_ticks(10))+
    #   scale_y_continuous(breaks = round(seq(1, max(sqlMonResults.agg.melt$value), by = 5),1),
    #                      minor_breaks = round(seq(1, max(sqlMonResults.agg.melt$value), by = 1),1))+
    ylab("Seconds")+
    labs(title="In-Memory vs No In-Memory")+
    scale_fill_stata()
  
  return(p)
}

plot4(subset(sqlMonResults.agg.melt,as.numeric(name) <= 33))



dev.off()

# ggsave(plot=p1,file="agg.avg.time.png",dpi = 300,width=16, height=9)
# ggsave(plot=p2,file="avg-time-by-query.png",dpi = 300,width=16, height=9)
# ggsave(plot=p3,file="by-query-by-test.png",dpi = 300,width=16, height=9)
# ggsave(plot=p4,file="by-query-facet-by-test.png",dpi = 300,width=16, height=9)
# 









