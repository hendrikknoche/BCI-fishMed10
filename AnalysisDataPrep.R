library(tidyverse)
library(readbulk)

df = readbulk::read_bulk('logFiles', sep=',', na.strings = 'none', stringsAsFactors=FALSE,row.names = NULL)
col<-ncol(df)
df$PID<-as.factor(substr(df$File,1,2))
df$condition<-as.factor(substr(df$File,4,5))

df<-df[,c(1,2,(col+1):(col+2),3:col)]
#the below commmand is not working yet as it depends on what columns there will be in your log files
# df %>% group_by(PID,condition,)%>%summarize(sumOfTime=sum(timeSinceStart))