library(tidyverse)
library(readbulk)
library(gsheet)
 
#put your google sheet link from the questionnaire here:
pd<-gsheet2tbl('https://docs.google.com/spreadsheets/d/19oiOujc73TEMqXbrQVdUmuttcx3M-0MnbajXwKrMxrk/edit?usp=sharing')

#import of log files
df = readbulk::read_bulk('logFilesData', sep=',', na.strings = 'NA', stringsAsFactors=FALSE,row.names = NULL)

# colnames(df)<-c(cols[2:length(cols)-1])
df$PID<-as.factor(substr(df$File,1,2))
df$condition<-as.factor(substr(df$File,4,5))
#further rows missing depending on which factors are in the filename
col<-ncol(df)
#move important columns up to the front
df<-df[,c(1,2,col,3:(col-1))]
#the below commmand is not working yet as it depends on what columns there will be in your log files
df %>% group_by(PID,InputBlockNo,InputNo,TrialNumber)%>%summarize(sumOfTime=sum(SequenceTime_ms,na.rm = TRUE))%>%view()
  

