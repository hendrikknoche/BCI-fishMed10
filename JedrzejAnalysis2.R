library(pgirmess)
library(tidyverse)
library(reshape2)

#reads the datafile
data = readbulk::read_bulk('Questionnaire data', sep=';', na.strings = 'NA', stringsAsFactors=FALSE,row.names = NULL)

#renames the columns to usable names
data <- data %>% rename("ID" = "Participant.no.")
data <- data %>% rename("PC" = "I.felt.in.control.of.the.fisherman.s.actions.")
data <- data %>% rename("R_PC" = "I.felt.in.control.....while.trying.to.reel.in.the.fish..performing.the.key.sequence...")
data <- data %>% rename("T_PC" = "I.felt.in.control..0...when.the.fish.was.tugging.away.from.me..moving.a.column.away...")
data <- data %>% rename("E_PC" = "I.felt.in.control.....when.the.fish.escaped..")
data <- data %>% rename("Compare_PC" = "In.this.condition...")
data <- data %>% rename("FR" = "How.much.frustration.did.you.feel.....during.this.condition..")
data <- data %>% rename("OFR" = "How.much.frustration.did.you.feel.....overall.since.we.started.the.experiment..")
data <- data %>% rename("R_FR" = "How.much.frustration.did.you.feel.....while.you.were.trying.to.reel.in.the.fish..perform.the.key.sequence..")
data <- data %>% rename("T_FR" = "How.much.frustration.did.you.feel.....while.the.fish.was.tugging.away.from.you..moving.a.column.away..")
data <- data %>% rename("E_FR" = "How.much.frustration.did.you.feel.....when.the.fish.escaped..")
data <- data %>% rename("Compare_FR" = "In.this.condition....1")
data <- data %>% rename("Estimate" = "How.likely.do.you.think.you.were.to.succeed.in.reeling.the.fish.up.by.one.lane.in.this.condition..Provide.the.answer.on.a.scale.of.1...100.")
data <- data %>% rename("PC_Sham" = "I.felt.in.control.when.I.got.help.from.the.other.character.")
data <- data %>% rename("FR_Sham" = "I.felt.frustrated.when.I.got.help.from.the.other.character.")
data <- data %>% rename("PC_AS" = "I.felt.in.control.when.when.my.character.reeled.the.fish.up.by.two.lanes.")
data <- data %>% rename("FR_AS" = "I.felt.frustrated.when.my.character.reeled.the.fish.up.by.two.lanes.")
data <- data %>% rename("PC_AF" = "I.felt.in.control.when.the.big.clamp.prevented.the.fish.from.swimming.away.from.me.")
data <- data %>% rename("FR_AF" = "I.felt.frustrated.when.the.big.clamp.prevented.the.fish.from.swimming.away.from.me.")


# names(data)[20] <- "FR_AS"

check <- data %>% 
  select(c("ID", "FR_Sham", "FR_AS", "FR_AF"))%>%melt(id.vars="ID")#%>%
  filter(!is.na(value))%>%
  #pivot_wider(names_from = variable, values_from = value)%>%
  #select(-ID)%>%as.matrix()%>%
  #friedman.test()

data %>% 
  select(c("ID", "FR_Sham", "FR_AS", "FR_AF"))%>%melt(id.vars="ID")%>%
  filter(!is.na(value))%>%
  pivot_wider(names_from = variable, values_from = value)%>%
  select(-ID)%>%as.matrix()%>%
  friedmanmc()

data %>% 
  select(c("ID", "Blame","Condition"))%>%pivot_wider(names_from = Condition, values_from = Blame)

Blame_FR <- data %>% 
  select(c("ID", "Blame","Condition", "FR_Sham", "FR_AS", "FR_AF"))#%>%melt(id.vars=c("ID","Blame"))%>%
 # filter(!is.na(value))%>%
  #pivot_wider(names_from = variable, values_from = value)#%>%
 # select(-ID)%>%as.matrix()
 # friedman.test()%>%

Blame_FR <- data[,c("ID", "Blame", "FR_Sham", "FR_AS", "FR_AF")]
Blame_FR$FR_Sham<-ifelse(is.na(Blame_FR$FR_AS), Blame_FR$FR_Sham, Blame_FR$FR_AS)
Blame_FR$FR_AS<-NULL
Blame_FR$FR_Sham<-ifelse(is.na(Blame_FR$FR_AF), Blame_FR$FR_Sham, Blame_FR$FR_AF)
Blame_FR$FR_AF<-NULL
Blame_FR<-reshape(Blame_FR, idvar='FR_Sham', timevar='ID', direction='wide')


















# #create a dataframe to analyze PC depending on PAM employed
# PCPAM<-data[,c(2,3,17,19,21)]
# #remove rows with condition 1 results (they're blank)
# PCPAM<-PCPAM[-c(1:17),]
# #merge all of PCPAM data into one column
# PCPAM$PCPAM2<-ifelse(is.na(PCPAM$PCPAM3), PCPAM$PCPAM2, PCPAM$PCPAM3)
# PCPAM$PCPAM3<-NULL
# PCPAM$PCPAM2<-ifelse(is.na(PCPAM$PCPAM4), PCPAM$PCPAM2, PCPAM$PCPAM4)
# PCPAM$PCPAM4<-NULL
# #reshape data so that the conditions become the grouping variables
# FriedmanPCPAM<-reshape(PCPAM, idvar='ID', timevar='Condition', direction='wide')
# FriedmanPCPAM$ID<-NULL
# #renaming the columns to actual conditions
# names(FriedmanPCPAM)[1] <- "con2"
# names(FriedmanPCPAM)[2] <- "con3"
# names(FriedmanPCPAM)[3] <- "con4"
# #running the Friedman test and posthoc
# friedman.test(as.matrix(FriedmanPCPAM))
# friedmanmc(as.matrix(FriedmanPCPAM))


