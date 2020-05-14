library(tidyverse)
library(readbulk)
library(gsheet)
library(lubridate)
library(reshape2)
 
#put your google sheet link from the questionnaire here:
pd<-gsheet2tbl('https://docs.google.com/spreadsheets/d/19oiOujc73TEMqXbrQVdUmuttcx3M-0MnbajXwKrMxrk/edit?usp=sharing')

#import of log files
df = readbulk::read_bulk('cleanedLogFiles', sep=',', na.strings = 'NA', stringsAsFactors=FALSE,row.names = NULL)
pr = readbulk::read_bulk('Questionnaire data', sep=',', na.strings = 'NA', stringsAsFactors=FALSE,row.names = NULL)

#maybe not needed but just as a demontration how to quickly reshape data dcast does the reverse
prlong<-pr%>%melt(id.vars=c("Timestamp","Condition","Participant.no."))


# colnames(df)<-c(cols[2:length(cols)-1])
df$PID<-as.factor(substr(df$File,1,2))
df$condition<-as.factor(substr(df$File,4,5))
#further rows missing depending on which factors are in the filename
col<-ncol(df)
#move important columns up to the front
df<-df[,c(1,2,col,3:(col-1))]
df$seconds<-period_to_seconds(hms(substr(df$Timestamp,1,8)))
df$BlockOutcome<-as.numeric(ifelse(df$BlockResult=="Win",1,ifelse(df$BlockResult=="Loss",0,NA)))
#the below commmand is not working yet as it depends on what columns there will be in your log files
df %>% group_by(PID,InputBlockNo,InputNo,TrialNumber,condition)%>%summarize(sumOfTime=sum(SequenceTime_ms,na.rm = TRUE))%>%view()
dfs<-df %>% group_by(PID,InputBlockNo,condition)%>%summarize(sumOfTime=sum(SequenceTime_ms,na.rm = TRUE),succRate=count(!is.na(WinCount))/(count(!is.na(WinCount))+count(!is.na(LossCount))))
dfs<-df %>% group_by(PID,condition)%>%summarize(sumOfTime=max(seconds)-min(seconds),succRate=mean(BlockOutcome,na.rm=TRUE),attempts=max(TrialNumber,na.rm = TRUE))

df %>% group_by(PID,condition)%>%summarize(sumOfTime=max(seconds)-min(seconds),succRate=mean(BlockOutcome,na.rm=TRUE),attempts=max(TrialNumber,na.rm = TRUE))%>%pivot_wider(names_from = condition, values_from = c(succRate,sumOfTime,attempts))%>%view()

