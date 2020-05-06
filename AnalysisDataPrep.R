library(tidyverse)
library(readbulk)
library(gsheet)

#put your google sheet link from the questionnaire here:
pd<-gsheet2tbl('https://docs.google.com/spreadsheets/d/19oiOujc73TEMqXbrQVdUmuttcx3M-0MnbajXwKrMxrk/edit?usp=sharing')

#import of log files
df = readbulk::read_bulk('logFiles', sep=',', na.strings = 'none', stringsAsFactors=FALSE,row.names = NULL)
col<-ncol(df)

df$PID<-as.factor(substr(df$File,1,2))
df$condition<-as.factor(substr(df$File,4,5))
#further rows missing depending on which factors are in the filename

#move important columns up to the front
df<-df[,c(1,2,(col+1):(col+2),3:col)]
#the below commmand is not working yet as it depends on what columns there will be in your log files
# df %>% group_by(PID,condition,)%>%summarize(sumOfTime=sum(timeSinceStart))

