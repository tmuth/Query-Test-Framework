#install.packages("XML")
library(XML)
library(ggplot2)
library(scales)
library(reshape)

#filePattern <- "^.+\\.html"
filePattern <- "^.+\\.html$"
#filePattern <- "^sqlmon_b.+(\\.html|\\.htm)$"
#filePattern <- "^sqlmon_q.+(\\.html|\\.htm)$"
#filePattern <- "^sqlmon_.+(\\.html|\\.htm)$"

#setwd("M:/Dropbox/MyFiles/Code Samples/Parallel Execution/Parallel Query Tests on X3-8/px-test1")
#setwd("M:/Dropbox/MyFiles/Code Samples/SQL-Test-Harness/retail-d56-queries")

#namePattern <- "sqlmon_rtl([a-zA-Z0-9_-]+)\\..*"
#namePattern <- "sqlmon_b_([a-zA-Z0-9_-]+)\\..*"
#namePattern <- "sqlmon_q([a-zA-Z0-9_-]+)\\..*"
#namePattern <- "sqlmon_caltrans_sql(0-9_-]+)\\..*"
namePattern <- "sqlmon_b_px([0-9]+)\\..*"

sqlMonResults_DF <- data.frame()

parse_SQL_Monitor <- function(fileName){

  output_DF <- data.frame('file'=f)
  
  
  get_stat <- function(df,SEARCH_VAL){
    if(nrow(subset(df,key==SEARCH_VAL)) == 0){
      return(NA)
    }
    else{
      tempDF <- subset(df,key==SEARCH_VAL)
      return(type.convert(as.vector(tempDF[,'val'])))
    }
  }
  
  addOutputColumn <- function(colname,colval){
    output_DF[,colname] <<- colval
  }
    
  
  doc <- xmlParse(fileName, getDTD = F)
  #myDoc <<- doc
  #x <-- base64Decode(type.convert(xmlValue(doc[['//report']])))
  
  reportDF <- data.frame()
  
  tryCatch(  reportDF <- data.frame('sql_id'=xmlValue(doc[["//report_parameters/sql_id"]])),
             error = function(e) {
             }
  )
  
  if(nrow(reportDF)==0){
    return(FALSE)
  }
  
  #reportDF <- data.frame('sql_id'=xmlValue(doc[["//report_parameters/sql_id"]]))
  addXmlValue <- function(colname,xpath){
    reportDF[,colname] <<- type.convert(xmlValue(doc[[xpath]]))
  }
  

  
  
  
  
  addOutputColumn('name',gsub(pattern = namePattern, replacement="\\1", f))
  
  addXmlValue('duration','//target/duration')
  
  
  output_DF <- cbind(output_DF,reportDF)
  
  
  
  optimizerVars <- getNodeSet(doc,"//optimizer_env/param")
  
  optDFKeys <- data.frame(key=sapply(optimizerVars, xmlGetAttr, "name"))
  optDFVals <- data.frame(val=sapply(optimizerVars, xmlValue))
  optimizerEnvDF <- cbind(optDFKeys,optDFVals)
  rm(optDFKeys)
  rm(optDFVals)
  
  
  
  
  addOutputColumn('parallel_query_mode',get_stat(optimizerEnvDF,'parallel_query_mode'))
  #addOutputColumn('parallel_degree',get_stat(optimizerEnvDF,'parallel_degree'))
  addOutputColumn('parallel_query_forced_dop',get_stat(optimizerEnvDF,'parallel_query_forced_dop'))
  addOutputColumn('cell_offload_processing',get_stat(optimizerEnvDF,'cell_offload_processing'))
  
  
  rptStats <- getNodeSet(doc,"//sql_monitor_report/stats/stat")
  
  statDFKeys <- data.frame(key=sapply(rptStats, xmlGetAttr, "name"))
  statFVals <- data.frame(val=sapply(rptStats, xmlValue))
  reportStatsDF <- cbind(statDFKeys,statFVals)
  rm(statDFKeys)
  rm(statFVals)
  
  
  
  readMB <- round(get_stat(reportStatsDF,'read_bytes')/1024/1024,1)
  readGB <- round(get_stat(reportStatsDF,'read_bytes')/1024/1024/1024,1)
  
  readMBperSec <- round(readMB/output_DF[1,]$duration,1)
  readGBperSec <- round(readGB/output_DF[1,]$duration,1)
  
  addOutputColumn('readGB',readGB)
  addOutputColumn('readMBperSec',readMBperSec)
  addOutputColumn('readGBperSec',readGBperSec)
  addOutputColumn('readMBperSecCore',round(readMBperSec/output_DF[1,]$servers_allocated,1))
  interconnectGB <- round(readGB /  get_stat(reportStatsDF,'cell_offload_efficiency'),1)
  addOutputColumn('interconnectGB', interconnectGB)
  addOutputColumn('offloadEfficiency', round(1-(interconnectGB / readGB),2))
  
  
  
  elapsedMS <- get_stat(reportStatsDF,'elapsed_time')
  addOutputColumn('dbTimePctCPU',round(get_stat(reportStatsDF,'cpu_time')/elapsedMS,2))
  addOutputColumn('dbTimePctIO',round(get_stat(reportStatsDF,'user_io_wait_time')/elapsedMS,2))
  addOutputColumn('dbTimePctOther',round(get_stat(reportStatsDF,'other_wait_time')/elapsedMS,2))
  addOutputColumn('dbTimePctCluster',round(get_stat(reportStatsDF,'cluster_wait_time')/elapsedMS,2))
  addOutputColumn('dbTimePctConcurrency',round(get_stat(reportStatsDF,'concurrency_wait_time')/elapsedMS,2))
  addOutputColumn('dbTimePctApp',round(get_stat(reportStatsDF,'application_wait_time')/elapsedMS,2))
  
  
  addOutputColumn('dbTimePctQueuing', round(get_stat(reportStatsDF,'queuing_time')/elapsedMS,2))
  addOutputColumn('dbTimePctQueuing', round(get_stat(reportStatsDF,'queuing_time')/elapsedMS,2))
  addOutputColumn('dbTimePctQueuing', round(get_stat(reportStatsDF,'queuing_time')/elapsedMS,2))
  
  sqlMonResults_DF <<- rbind(sqlMonResults_DF,output_DF)
}

# parse_SQL_Monitor("sqlmon_b_px_52.html")
# parse_SQL_Monitor("sqlmon_b_px_50.html")



sqlMonFiles <- list.files(pattern=filePattern)


for (f in sqlMonFiles) {
  print(f)
  #parse_SQL_Monitor(f)
  tryCatch(parse_SQL_Monitor(f), 
           error = function(e) {
             #traceback()
             print(paste0("Error in ",f,": ",e))
 
           }
  )
}


#sqlMonResults_DF <- sqlMonResults_DF[with(sqlMonResults_DF, order(servers_allocated)), ]
write.csv(sqlMonResults_DF,"sql-mon-results-DF.csv")


#sqlMonResults_DF <- subset(sqlMonResults_DF,parallel_degree <= 100)
###########################################################################
# The rest is graphing of the data frame we created
###########################################################################


pdf("SQL-Monitor-Graphs2.pdf", width = 11, height = 8.5,useDingbats=FALSE)

custScalesX<-scale_x_discrete(breaks=trans_breaks("identity", function(x) x, n=30))
custScalesY<-scale_y_discrete(breaks=trans_breaks("identity", function(x) x, n=30))
  

ggplot(data=sqlMonResults_DF,aes(x=parallel_degree,y=duration))+
  geom_point()+
  geom_text(aes(label=duration,vjust=1.2),size=3)+
  ylab("seconds")+xlab("parallel degree")+
  #custScalesX+custScalesY+
  labs(title="Parallel Degree vs Time")


ggplot(data=sqlMonResults_DF,aes(x=parallel_degree,y=readGBperSec))+
  geom_point(shape=1)+
  geom_text(aes(label=readGBperSec,vjust=1.2),size=3)+
  xlab("parallel degree")+
  labs(title="Parallel Degree vs Read GB/s")+
  custScalesX+custScalesY


ggplot(data=sqlMonResults_DF,aes(x=name,y=readGBperSec))+
  geom_point(shape=1)+
  geom_text(aes(label=readGBperSec,vjust=1.2),size=3)+
  xlab("query")+
  labs(title="Queries by Average Read GB/s")
  

ggplot(data=sqlMonResults_DF,aes(x=parallel_degree,y=readMBperSecCore))+
  geom_point(shape=1)+
  xlab("parallel degree")+
  labs(title="Parallel Degree vs Read MB/s/core")+
  custScalesX+custScalesY

#sqlMonResults_DF$duration2 <- as.character(sqlMonResults_DF$duration)
#sqlMonResults_DF$duration2 <- factor(sqlMonResults_DF$duration2 , levels=unique(sqlMonResults_DF$duration2 ), ordered=TRUE)

ggplot(data=sqlMonResults_DF,aes(y=reorder(name,duration),x=duration))+
  geom_point()+
  xlab("duration")+
  ylab("name")+
  #custScalesX+
  labs(title="Queries by Time (seconds)")


ggplot(data=sqlMonResults_DF,aes(x=reorder(name,offloadEfficiency),y=offloadEfficiency))+
  geom_point()+
  geom_text(aes(label=(paste0((offloadEfficiency*100),"%")),vjust=1.2),size=3)+
  xlab("Query")+
  ylab("Offload Efficiency")+
  labs(title="Queries by Offload Efficiency")+
  scale_y_continuous(labels = percent_format())

ggplot(data=sqlMonResults_DF,aes(x=reorder(name,offloadEfficiency),y=offloadEfficiency))+
  geom_point()+
  geom_text(aes(label=(paste0((offloadEfficiency*100),"%")),vjust=1.2),size=3)+
  xlab("Query")+
  ylab("Offload Efficiency")+
  labs(title="Queries by Offload Efficiency")+
  scale_y_continuous(labels = percent_format())

ggplot(data=sqlMonResults_DF,aes(x=reorder(name,readGB),y=readGB))+
  geom_point()+
  geom_text(aes(label=readGB,vjust=1.2),size=3)+
  xlab("Query")+
  ylab("GB")+
  labs(title="Queries by Physical I/O")
  
#sqlMonResults_DF.melt <- melt(sqlMonResults_DF, id.var = c("name"), measure.var = c("dbTimePctCPU", "dbTimePctIO","dbTimePctOther", "dbTimePctCluster","dbTimePctApp","dbTimePctQueuing"))
sqlMonResults_DF.melt <- melt(sqlMonResults_DF, id.var = c("name"), measure.var = c("dbTimePctCPU", "dbTimePctIO","dbTimePctOther", "dbTimePctCluster","dbTimePctApp","dbTimePctQueuing"))
# We need to change these names and they are "factors" which we can't change
sqlMonResults_DF.melt<- transform(sqlMonResults_DF.melt, variable = as.character(variable))
sqlMonResults_DF.melt[with(sqlMonResults_DF.melt, grepl("dbTimePctCPU", variable)),]$variable<-"CPU"
sqlMonResults_DF.melt[with(sqlMonResults_DF.melt, grepl("dbTimePctIO", variable)),]$variable<-"I/O"
sqlMonResults_DF.melt[with(sqlMonResults_DF.melt, grepl("dbTimePctOther", variable)),]$variable<-"Other"
sqlMonResults_DF.melt[with(sqlMonResults_DF.melt, grepl("dbTimePctCluster", variable)),]$variable<-"Cluster"
sqlMonResults_DF.melt[with(sqlMonResults_DF.melt, grepl("dbTimePctApp", variable)),]$variable<-"Application"
sqlMonResults_DF.melt[with(sqlMonResults_DF.melt, grepl("dbTimePctQueuing", variable)),]$variable<-"Queuing"

aas_colors <- c("Administrative" = "#6c6e69", "Application" = "#bf2a05", "Cluster" = "#ccc4af", "Commit" = "#e36a05",
                "Concurrency" = "#8a1b07","Configuration" = "#5a4611","CPU" = "#05cc04","Network" = "#9b9b7a",
                "Other" = "#f06fad","Scheduler" = "#97f797","Queuing" = "#c4b69c",
                "I/O" = "#054ae1")
gg_aas_colors <- scale_fill_manual("", values = aas_colors)

ggplot(data=sqlMonResults_DF.melt)+
  geom_bar(width=1.8,aes(x=name,y=value,fill=variable),stat='identity',position='stack')+
  geom_text(aes(label=(value*100),x=name,y=value,vjust=1),size=2.5,stat='identity',position='stack')+
  scale_y_continuous(labels = percent_format())+
  scale_x_discrete()+
  gg_aas_colors+
  xlab("Query")+
  ylab("% Wait")+
  labs(title="Wait % by Query by Category")+
  theme(axis.text.x=element_text(angle=-90, hjust=0,vjust=0,size=8))


ggplot(data=sqlMonResults_DF.melt)+
  geom_bar(aes(x=name,y=value,fill=variable),stat='identity',position='stack')+
  geom_text(aes(label=(value*100),x=name,y=value,vjust=1),size=2.5,stat='identity',position='stack')+
  scale_y_continuous(labels = percent_format())+
  gg_aas_colors+
  xlab("Query")+
  ylab("% Wait")+
  labs(title="Wait % by Query by Category")


ggplot(data=sqlMonResults_DF.melt)+
  geom_bar(aes(x=parallel_degree,y=value,fill=variable),stat='identity',position='stack')+
  geom_text(aes(label=(value*100),x=parallel_degree,y=value,vjust=1),size=2.5,stat='identity',position='stack')+
  scale_y_continuous(labels = percent_format())+
  gg_aas_colors+
  xlab("Query")+
  ylab("% Wait")+
  labs(title="Wait % by Query by Category")

ggplot(data=sqlMonResults_DF.melt)+
  geom_bar(aes(x=parallel_degree,y=value,fill=variable),stat='identity',position='stack')+
  facet_grid(variable ~ .)+
  geom_text(aes(label=(value*100),x=parallel_degree,y=value,vjust=-.2),size=2.5,stat='identity',position='stack')+
  scale_y_continuous(labels = percent_format())+
  gg_aas_colors+
  xlab("Query")+
  ylab("% Wait")+
  labs(title="Wait % by Query by Category")

dev.off()








############################################################


fileName <- "sqlmon_b_px_16.html"
doc <- xmlParse(fileName, getDTD = F)

activity_detail <- getNodeSet(doc,"//activity_detail")
detailAttributes <- data.frame(detailAttributes)
str(activity_detail)




detailAttributes <- xmlAttrs(doc[["//activity_detail"]])
detailAttributes <- data.frame(name=names(detailAttributes),value=detailAttributes,row.names=NULL)
activity_detail <- getNodeSet(doc,"//activity_detail/bucket")

foo <- data.frame(key=sapply(activity_detail, xmlGetAttr, "bucket"))



