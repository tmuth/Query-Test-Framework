#install.packages("XML")
#library(devtools)
#install_github('ramnathv/rCharts')
#devtools::install_github('ramnathv/rCharts', ref='dev')
library(rCharts)

library(XML)
library(ggplot2)
library(scales)
library(reshape)
library(gridExtra)
library(tools)
library(stringr)
library(sjPlot)
library(lubridate) 
library(ggthemes) 
library(dplyr) 


filePattern <- "^.+\\.html$"
#filePattern <- "^rtl16a.html$"
#filePattern <- "^sqlmon_b.+(\\.html|\\.htm)$"
#filePattern <- "^sqlmon_q.+(\\.html|\\.htm)$"
#filePattern <- "^sqlmon_.+(\\.html|\\.htm)$"

#setwd("M:/Dropbox/MyFiles/Code Samples/Parallel Execution/Parallel Query Tests on X3-8/px-test1")
#setwd("M:/Dropbox/MyFiles/Code Samples/SQL-Test-Harness/retail-d56-queries")

#namePattern <- "sqlmon_rtl([a-zA-Z0-9_-]+)\\..*"
#namePattern <- "sqlmon_b_([a-zA-Z0-9_-]+)\\..*"
#namePattern <- "sqlmon_q([a-zA-Z0-9_-]+)\\..*"
#namePattern <- "rtl([0-9a]+)\\..*" # remove "rtl", then use a number, ie rtl3.html = 3, rtl7a.html = 7a
#namePattern <- "([0-9]+)\\..*" # Leading number indicates name, ie 12.123abcde.foo.bar.html becomes 12
namePattern <- ".+\\.([0-9]+)\\..{1,7}$"
#reportStatsDF.2 <- data.frame()
sqlMonResults_DF <- data.frame()
out_DF <- data.frame()
the_doc <- NULL
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
    
  tryCatch( 
    doc <- xmlParse(fileName, getDTD = F),
    error = function(e) {
      return(FALSE)
    }
  )
  
  the_doc <<- doc
  #myDoc <<- doc
  #x <-- base64Decode(type.convert(xmlValue(doc[['//report']])))
  
  reportDF <- data.frame()
  
  tryCatch(  reportDF <- data.frame('sql_id'=xmlValue(doc[["//report_parameters/sql_id"]])),
             error = function(e) {
               return(FALSE)
             }
  )
  
  
  
  if(nrow(reportDF)==0){
    return(FALSE)
  }
  
  #reportDF <- data.frame('sql_id'=xmlValue(doc[["//report_parameters/sql_id"]]))
  addXmlValue <- function(colname,xpath){
    reportDF[,colname] <<- type.convert(xmlValue(doc[[xpath]]))
  }
  

  
  ##################################################
  # extract from text file
  
  theFile <- readLines(paste0(file_path_sans_ext(fileName),'.txt'))
  theFileTXT <- paste(theFile, collapse='\n')
  strTmp <- str_extract(theFileTXT, perl('\nElapsed:.+\n'))
  strTmp2 <- str_extract(strTmp,perl('[0-9]{2}:[0-9]{2}:[0-9]{2}\\.[0-9]{2}'))
  
  options(digits.secs = 2)
  sqlplusElapsedTime <- period_to_seconds(hms(strTmp2))
  
  
    ##############
    # the following is to get the data out of the v$mystat section. its just a sample at this point
    # should add the header / footer info from awr-miner, then use that routine to exract this data. 
  
    #foo <- str_extract(theFileTXT,'\nNAME.+VALUE.+')
    #foo2 <- str_extract(foo,'.+?[0-9]+ rows selected\\.')
  
    ##############
  
  
  rm(theFileTXT)
  rm(strTmp)
  rm(strTmp2)
  
  #
  ##################################################
  
  
  
  addOutputColumn('name',gsub(pattern = namePattern, replacement="\\1", f))
  addXmlValue('user','//target/user')
  addXmlValue('duration','//target/duration')
  addXmlValue('start','//report_parameters/interval_start')
  
  
  
  
  
  output_DF <- cbind(output_DF,reportDF)
  addOutputColumn('sqlplus.elap',sqlplusElapsedTime)
  
  
  optimizerVars <- getNodeSet(doc,"//optimizer_env/param")
  
  optDFKeys <- data.frame(key=sapply(optimizerVars, xmlGetAttr, "name"))
  optDFVals <- data.frame(val=sapply(optimizerVars, xmlValue))
  optimizerEnvDF <- cbind(optDFKeys,optDFVals)
  rm(optDFKeys)
  rm(optDFVals)
  
  #addOutputColumn('parallel_query_mode',get_stat(optimizerEnvDF,'parallel_query_mode'))
  addOutputColumn('parallel_degree',get_stat(optimizerEnvDF,'parallel_degree'))
  #addOutputColumn('parallel_query_forced_dop',get_stat(optimizerEnvDF,'parallel_query_forced_dop'))
  #addOutputColumn('cell_offload_processing',get_stat(optimizerEnvDF,'cell_offload_processing'))
  
  
  otherXML <- getNodeSet(doc,"//other_xml/info")
  otherXMLKeys <- data.frame(key=sapply(otherXML, xmlGetAttr, "type"))
  otherXMLVals <- data.frame(val=sapply(otherXML, xmlValue))
  otherXMLDF <- cbind(otherXMLKeys,otherXMLVals)
  rm(otherXMLKeys)
  rm(otherXMLVals)
  
  
  addOutputColumn('px_inmem',get_stat(otherXMLDF,'px_in_memory'))
  addOutputColumn('in_mem_option',get_stat(otherXMLDF,'px_in_memory_imc'))
  addOutputColumn('cardinality_feedback',get_stat(otherXMLDF,'cardinality_feedback'))
  addOutputColumn('dyn_samp',get_stat(otherXMLDF,'dynamic_sampling'))

  
  
  rptStats <- getNodeSet(doc,"//sql_monitor_report/stats/stat")
  
  statDFKeys <- data.frame(key=sapply(rptStats, xmlGetAttr, "name"))
  statFVals <- data.frame(val=sapply(rptStats, xmlValue))
  reportStatsDF <- cbind(statDFKeys,statFVals)
  
  #reportStatsDF.2 <<- reportStatsDF
  
  rm(statDFKeys)
  rm(statFVals)
  
  writeBytes <- sum(as.numeric(xpathSApply(doc,"//stats[@type='monitor']/stat[@name='write_bytes']",xmlValue)))
  writeGB <- round(writeBytes/1024/1024/1024,1)
  writeMB <- round(writeBytes/1024/1024,2)
  
  
  
  readMB <- round(get_stat(reportStatsDF,'read_bytes')/1024/1024,1)
  readGB <- round(get_stat(reportStatsDF,'read_bytes')/1024/1024/1024,1)
  
  readMBperSec <- round(readMB/output_DF[1,]$duration,1)
  readGBperSec <- round(readGB/output_DF[1,]$duration,1)
  
  addOutputColumn('readGB',readGB)
  addOutputColumn('writeGB',writeGB)
  addOutputColumn('writeMB',writeMB)
  #print(readGB)
  addOutputColumn('readMBperSec',readMBperSec)
  addOutputColumn('readGBperSec',readGBperSec)
  addOutputColumn('readMBperSecCore',round(readMBperSec/output_DF[1,]$servers_allocated,1))
  
  io_inter_bytes <- sum(as.numeric(xpathSApply(doc,"//stats[@type='plan_monitor']/stat[@name='io_inter_bytes']",xmlValue)))
  io_inter_bytes_MB <- round(io_inter_bytes/1024/1024)
  
  
  cellOffloadEfficiency <- round((readMB-io_inter_bytes_MB)/readMB,2)
  
  
#   cellOffloadEfficiency <- get_stat(reportStatsDF,'cell_offload_efficiency')
#   if((is.na(cellOffloadEfficiency))){
#     interconnectGB <- NA
#   }else{
#     interconnectGB <- round(readGB /  cellOffloadEfficiency,1)
#   }
  addOutputColumn('interconnectGB', (io_inter_bytes_MB/1024))
  addOutputColumn('offloadEfficiency', cellOffloadEfficiency)
  
  #addOutputColumn('offloadEfficiency', round(1-(interconnectGB / readGB),2))
  
  
  activityStats <- getNodeSet(doc,"//sql_monitor_report/activity_sampled/activity")
  activityDFKeys <- sapply(activityStats, xmlGetAttr, "class")
  activityDFAttr <- sapply(activityStats, xmlGetAttr, "event",default=" ")
  activityDFVals <- sapply(activityStats, xmlValue)
  activityStatsDF <- as.data.frame(cbind(key=unlist(activityDFKeys),val=unlist(activityDFVals),attr=unlist(activityDFAttr)))
  activityStatsDF$key <- as.character(activityStatsDF$key)
  activityStatsDF$key <- ifelse(activityStatsDF$key == "Cpu" & activityStatsDF$attr == "in memory","Cpu inmemory",activityStatsDF$key)
  
  
  elapsedMicroS <- get_stat(reportStatsDF,'elapsed_time')
  elapsedSec <- elapsedMicroS/1000000
  addOutputColumn('dbTime',elapsedSec)
  
  
  if(!is.na(get_stat(activityStatsDF,"Cpu inmemory"))){
    #print("yep")
    activityStatsTotalCPU <- get_stat(activityStatsDF,"Cpu") + get_stat(activityStatsDF,"Cpu inmemory")  
    
    cpuPct <- round(get_stat(reportStatsDF,'cpu_time')/elapsedMicroS,2)
    
    cpuNonImcPct <- (get_stat(activityStatsDF,"Cpu")/activityStatsTotalCPU)*(get_stat(reportStatsDF,'cpu_time')/elapsedMicroS)
    cpuImcPct <- (get_stat(activityStatsDF,"Cpu inmemory")/activityStatsTotalCPU)*(get_stat(reportStatsDF,'cpu_time')/elapsedMicroS)
    cpuNonImcPct <- round(cpuNonImcPct,2)
    cpuImcPct <- round(cpuImcPct,2)
    
    addOutputColumn('dbTimePctCPU',cpuNonImcPct) 
    addOutputColumn('dbTimePctCPUimc',cpuImcPct) 
    
    #print(cpuNonImcPct)
    #print(cpuImcPct)
  }else{
    addOutputColumn('dbTimePctCPU',round(get_stat(reportStatsDF,'cpu_time')/elapsedMicroS,2))  
    addOutputColumn('dbTimePctCPUimc',0) 
  }
  
  
  
  addOutputColumn('dbTimePctIO',round(get_stat(reportStatsDF,'user_io_wait_time')/elapsedMicroS,2))
  addOutputColumn('dbTimePctOther',round(get_stat(reportStatsDF,'other_wait_time')/elapsedMicroS,2))
  addOutputColumn('dbTimePctCluster',round(get_stat(reportStatsDF,'cluster_wait_time')/elapsedMicroS,2))
  addOutputColumn('dbTimePctConcurrency',round(get_stat(reportStatsDF,'concurrency_wait_time')/elapsedMicroS,2))
  addOutputColumn('dbTimePctApp',round(get_stat(reportStatsDF,'application_wait_time')/elapsedMicroS,2))
  
  
  addOutputColumn('dbTimePctQueuing', round(get_stat(reportStatsDF,'queuing_time')/elapsedMicroS,2))
  addOutputColumn('dbTimePctQueuing', round(get_stat(reportStatsDF,'queuing_time')/elapsedMicroS,2))
  addOutputColumn('dbTimePctQueuing', round(get_stat(reportStatsDF,'queuing_time')/elapsedMicroS,2))
  

sql = xpathApply(doc, "//sql_fulltext", xmlValue)
addOutputColumn('sql',sql)
#print(sql)  


print(length(names(sqlMonResults_DF)))
print(length(names(output_DF)))
  
  sqlMonResults_DF <<- rbind(sqlMonResults_DF,output_DF)
  print("y")
  out_DF <<- output_DF
}

# parse_SQL_Monitor("sqlmon_b_px_52.html")
# parse_SQL_Monitor("sqlmon_b_px_50.html")



sqlMonFiles <- list.files(pattern=filePattern)


for (f in sqlMonFiles) {
  
#   if(f != 'summary.html'){
#     print(f)
#     parse_SQL_Monitor(f)  
#   }
#   
  if(f != 'summary.html'){
    print(f)
    tryCatch(parse_SQL_Monitor(f), 
             error = function(e) {
               #traceback()
               print(paste0("Error in ",f,": ",e))
   
             }
    )
  }
}





sqlMonResults_DF$test <- basename(getwd())
sqlMonResults_DF$start <- mdy_hms(as.character(sqlMonResults_DF$start))

save(sqlMonResults_DF,file=paste(basename(getwd()),".Rda"))

#sqlMonResults_DF <- sqlMonResults_DF[with(sqlMonResults_DF, order(servers_allocated)), ]
write.csv( subset( sqlMonResults_DF , select = -c(sql) ),"sql-mon-results-DF.csv")


#sqlMonResults_DF <- subset(sqlMonResults_DF,parallel_degree <= 100)
###########################################################################
# The rest is graphing of the data frame we created
###########################################################################


data_frame_col_not_null <- function(df_in, column_in, min_rows_in = 1){
  #return(TRUE)
  #foo <<- df_in
  
  if((column_in %in% names(df_in))){
    # the relevant column is in the data frame
    #print(nrow(df_in[!(is.na(df_in[[column_in]]))]))
    if(nrow(df_in[!(is.na(df_in[[column_in]])),]) >= min_rows_in){
      # there are at least min_rows_in number of rows in this df that are not NA
      return(TRUE)
    }
    else{
      # the column exists, but there are not min_rows_in rows that are not NA (common case would be a column with all NAs)
      return(FALSE)
    }
  }
  else{
    # the relevant column is NOT in the data frame
    return(FALSE)
  }
  # fall through, defaults to false
  return(FALSE)
}


plotIt <- function(plot_in){
  try(
    print(plot_in),
    silent=TRUE
  )
}

testName <- sqlMonResults_DF$test[1]









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







#########


#pdf("SQL-Monitor-Graphs2.pdf", width = 11, height = 8.5, useDingbats=FALSE)
pdf(paste0("SQL-MON-",testName,".pdf"), width = 11, height = 8.5, useDingbats=FALSE)


txt <- tableGrob(sqlMonResults_DF[,1:14],show.rownames = FALSE, gpar.coretext = gpar(fontsize=8),gpar.coltext = gpar(fontsize=7),padding.v = unit(1, "mm"),padding.h = unit(2, "mm"),show.colnames = TRUE,col.just = "left")
grid.arrange(txt,ncol = 1, widths=c(1))

custScalesX<-scale_x_discrete(breaks=trans_breaks("identity", function(x) x, n=30))
custScalesY<-scale_y_discrete(breaks=trans_breaks("identity", function(x) x, n=30))
  
if(data_frame_col_not_null(sqlMonResults_DF,'parallel_degree')){
  p <- ggplot(data=sqlMonResults_DF,aes(x=parallel_degree, y=duration))+
    geom_point()+
    geom_text(aes(label=duration,vjust=1.2),size=3)+
    ylab("seconds")+xlab("parallel degree")+
    scale_x_continuous(breaks = round(seq(2, max(sqlMonResults_DF$parallel_degree), by = 4),1),
                       minor_breaks = round(seq(2, max(sqlMonResults_DF$parallel_degree), by = 2),1))+
    theme(axis.text.x =       element_text(angle = -90))+
    labs(title="Parallel Degree vs Time")
  
  plotIt(p)
}
# 
# 
# if(data_frame_col_not_null(sqlMonResults_DF,'parallel_degree')){
#   p <- ggplot(data=sqlMonResults_DF,aes(x=parallel_degree,y=readGBperSec))+
#     geom_point(shape=1)+
#     geom_text(aes(label=readGBperSec,vjust=1.2),size=3)+
#     xlab("parallel degree")+
#     labs(title="Parallel Degree vs Read GB/s")+
#     custScalesX+custScalesY
#   
#   plotIt(p)
# }
# 
# p <- ggplot(data=sqlMonResults_DF,aes(x=name,y=readGBperSec))+
#   geom_point(shape=1)+
#   geom_text(aes(label=readGBperSec,vjust=1.2),size=3)+
#   xlab("query")+
#   labs(title=paste0("Queries by Average Read GB/s for ",testName))
#   labs(title="Queries by Average Read GB/s")
#   
# plotIt(p)
# 
# if(data_frame_col_not_null(sqlMonResults_DF,'parallel_degree')){
# p <- ggplot(data=sqlMonResults_DF,aes(x=parallel_degree,y=readMBperSecCore))+
#     geom_point(shape=1)+
#     xlab("parallel degree")+
#     labs(title="Parallel Degree vs Read MB/s/core")+
#     custScalesX+custScalesY
#   
#   #sqlMonResults_DF$duration2 <- as.character(sqlMonResults_DF$duration)
#   #sqlMonResults_DF$duration2 <- factor(sqlMonResults_DF$duration2 , levels=unique(sqlMonResults_DF$duration2 ), ordered=TRUE)
#   
#   plotIt(p)
# }

p <- ggplot(data=sqlMonResults_DF,aes(y=reorder(name,duration),x=duration))+
  geom_point()+
  geom_text(aes(label=duration,vjust=1.2),size=3)+
  xlab("duration")+
  ylab("name")+
  #custScalesX+
  labs(title="Queries by Time (seconds)")


plotIt(p)

#if(sum(sqlMonResults_DF$offloadEfficiency,na.rm=TRUE)>0){
  p <- ggplot(data=sqlMonResults_DF,aes(x=reorder(name,offloadEfficiency),y=offloadEfficiency))+
    geom_point()+
    geom_text(aes(label=(paste0((offloadEfficiency*100),"%")),vjust=1.2),size=3)+
    xlab("Query")+
    ylab("Offload Efficiency")+
    labs(title="Queries by Offload Efficiency")+
    scale_y_continuous(labels = percent_format())
  
  plotIt(p)
#}

if(sum(sqlMonResults_DF$readGB,na.rm=TRUE)>1){
p <- ggplot(data=sqlMonResults_DF,aes(x=reorder(name,readGB),y=readGB))+
  geom_point()+
  geom_text(aes(label=readGB,vjust=1.2),size=3)+
  xlab("Query")+
  ylab("GB")+
  labs(title="Queries by Physical I/O")

plotIt(p)
}
  
#sqlMonResults_DF.melt <- melt(sqlMonResults_DF, id.var = c("name"), measure.var = c("dbTimePctCPU", "dbTimePctIO","dbTimePctOther", "dbTimePctCluster","dbTimePctApp","dbTimePctQueuing"))
sqlMonResults_DF.melt <- melt(sqlMonResults_DF, id.var = c("name"), 
                              measure.var = c("dbTimePctCPU","dbTimePctCPUimc", "dbTimePctIO","dbTimePctOther", "dbTimePctCluster","dbTimePctApp","dbTimePctQueuing"))
# We need to change these names and they are "factors" which we can't change

replace.variable <- function(search_in,replace_in){
  idx <- with(sqlMonResults_DF.melt, grepl(search_in, variable))
  if(length(idx[idx==TRUE])>0){
    sqlMonResults_DF.melt[idx,]$variable <<- replace_in
  }
  
}

sqlMonResults_DF.melt<- transform(sqlMonResults_DF.melt, variable = as.character(variable))
replace.variable("^dbTimePctCPU$","CPU")
replace.variable("^dbTimePctCPUimc$","CPU inmemory")
replace.variable("^dbTimePctIO$","I/O")
replace.variable("^dbTimePctOther$","Other")
replace.variable("^dbTimePctCluster$","Cluster")
replace.variable("^dbTimePctApp$","Application")
replace.variable("^dbTimePctQueuing$","Queuing")

#unique(sqlMonResults_DF.melt$variable)

aas_colors <- c("Administrative" = "#6c6e69", "Application" = "#bf2a05", "Cluster" = "#ccc4af", "Commit" = "#e36a05",
                "Concurrency" = "#8a1b07","Configuration" = "#5a4611","CPU" = "#05cc04", "CPU inmemory" = "#006600",
                "Network" = "#9b9b7a",
                "Other" = "#f06fad","Scheduler" = "#97f797","Queuing" = "#c4b69c",
                "I/O" = "#054ae1")
gg_aas_colors <- scale_fill_manual("", values = aas_colors)





p <- ggplot(data=sqlMonResults_DF.melt)+
  geom_bar(aes(x=name,y=value,fill=variable),stat='identity',position='stack')+
  geom_text(aes(label=(value*100),x=name,y=value,vjust=1),size=2.5,stat='identity',position='stack')+
  scale_y_continuous(labels = percent_format())+
  gg_aas_colors+
  xlab("Query")+
  ylab("% Wait")+
  labs(title="Wait % by Query by Category")+
  theme(axis.text.x=element_text(angle=-90, hjust=0,vjust=0,size=8))


plotIt(p)



sqlMonResults_DF.melt2 <- melt(sqlMonResults_DF, id.var = c("name"), measure.var = c("duration", "sqlplus.elap"))
#x.melt$variable<-gsub( "os_cpu_max" , "OS CPU Max" , x.melt$variable)

sqlMonResults_DF.melt2$variable <- gsub( "duration" , "sqlmon.elap" , sqlMonResults_DF.melt2$variable)

ggplot(data=sqlMonResults_DF.melt2,aes(x=name,y=value,group=variable))+
  geom_bar(position="dodge",stat="identity",aes(fill=variable))+
  geom_text(aes(label=value),size=2.5,stat='identity',position='dodge')+
  xlab("Query Name")+
  ylab("Seconds")+
  labs(title="SQL Monitor Timing vs SQL*Plus Timing (optimizer_adaptive_features=false)")



dev.off()

SUMMARY_DF_TMP <- sqlMonResults_DF[,1:15]

SUMMARY_DF_TMP <- sqlMonResults_DF %>%
  select(file,name,sql_id,duration,sqlplus.elap,parallel_degree,dyn_samp,readGB)

SUMMARY_DF_TMP$link.html <- paste0('<a href="',SUMMARY_DF_TMP$file,'">',SUMMARY_DF_TMP$file,'</a>')
SUMMARY_DF_TMP$link.txt <- paste0('<a href="',file_path_sans_ext(SUMMARY_DF_TMP$file),'.txt">',
                                  file_path_sans_ext(SUMMARY_DF_TMP$file),'.txt</a>')

#SUMMARY_DF_TMP <- SUMMARY_DF_TMP[order(SUMMARY_DF_TMP$duration,decreasing = TRUE),]
#SUMMARY_DF_TMP <- SUMMARY_DF_TMP[order(SUMMARY_DF_TMP$name,decreasing = FALSE),]
#sjt.df(SUMMARY_DF_TMP,file=paste0("summary.html"),describe=FALSE,alternateRowColors=TRUE)


dt <- dTable(
  SUMMARY_DF_TMP,
  #bScrollInfinite = T,
  #bScrollCollapse = T,
  #sScrollY = "200px",
  #sScrollY = "800px",
  bLengthChange = F,
  bPaginate = F
  #sPaging = F
  #width = "500px"
)

dt
dt$save('summary.html', cdn=FALSE,standalone = TRUE)

rm(SUMMARY_DF_TMP)

sql_outfile <- "sql_text.sql"
write('',sql_outfile,append=FALSE)

for(i in 1:nrow(sqlMonResults_DF)) {
  row <- sqlMonResults_DF[i,]
  write(paste0('-- ',row$file),file="sql_text.sql",append=TRUE)
  write(paste0(row$sql),file="sql_text.sql",append=TRUE)
  write(paste0(''),file="sql_text.sql",append=TRUE)
  write(paste0(''),file="sql_text.sql",append=TRUE)
  # do stuff with row
}






  



##################










# optimizerVars <- getNodeSet(doc,"//optimizer_env/param")
# 
# optDFKeys <- data.frame(key=sapply(optimizerVars, xmlGetAttr, "name"))
# optDFVals <- data.frame(val=sapply(optimizerVars, xmlValue))
# optimizerEnvDF <- cbind(optDFKeys,optDFVals)
# rm(optDFKeys)
# rm(optDFVals)
# 
# 
# testVars <- getNodeSet(the_doc,"//stats[@type='plan_monitor']/stat")
# testVars <- getNodeSet(the_doc,"//stats/stat")
# testVarsKeys <- data.frame(key=sapply(testVars, xmlGetAttr, "name"))
# testVarsVals <- data.frame(val=sapply(testVars, xmlValue))
# testVarsDF <- cbind(testVarsKeys,testVarsVals)
# rm(optDFKeys)
# rm(optDFVals)
# 
# 
# testVars <- getNodeSet(the_doc,"//stats[@type='plan_monitor']/stat")
# 
# testVarsName <- xpathSApply(the_doc,"//stats[@type='plan_monitor']/stat",xmlGetAttr,'name')
# testVarsVals <- as.numeric(xpathSApply(the_doc,"//stats[@type='plan_monitor']/stat[@name='io_inter_bytes']",xmlValue))
# testVarsVals <- as.numeric(xpathSApply(the_doc,"//stats[@type='monitor']/stat[@name='write_bytes']",xmlValue))
# testVarsVals.sum <- sum(testVarsVals)
# 
# str(testVars)

sqlMonResults_DF <- subset(sqlMonResults_DF,parallel_degree > 0)

p <- ggplot(data=sqlMonResults_DF,aes(x=parallel_degree, y=duration))+
  geom_point()+
  geom_text(aes(label=duration,vjust=1.2),size=3)+
  ylab("seconds")+xlab("parallel degree")+
  labs(title="Parallel Degree vs Time")+
  scale_x_continuous(breaks = round(seq(2, max(sqlMonResults_DF$parallel_degree), by = 4),1),
                     minor_breaks = round(seq(2, max(sqlMonResults_DF$parallel_degree), by = 2),1))+
  theme(axis.text.x =       element_text(angle = -90))
p

ggsave(plot=p,file="parallel-degree-vs-time.png",dpi = 300,width=16, height=9)




