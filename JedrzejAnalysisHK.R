
library(pgirmess)
# library(MASS)
library(tidyverse)
library(reshape2)


# reads the datafile
datax <- readbulk::read_bulk("Questionnaire data", sep = ";", na.strings = "NA", stringsAsFactors = FALSE, row.names = NULL)
# renames the columns to usable names
# data <- data %>% rename(new_name = old_name)
names(data)[3] <- "ID"
names(data)[4] <- "C_PC"
names(data)[5] <- "R_PC"
names(data)[6] <- "T_PC"
names(data)[7] <- "E_PC"
names(data)[8] <- "compare_PC"
names(data)[9] <- "C_FR"
names(data)[10] <- "OFR"
names(data)[11] <- "C_FR_R"
names(data)[12] <- "C_FR_T"
names(data)[13] <- "C_FR_E"
names(data)[14] <- "compare_FR"
names(data)[15] <- "esitmate"
names(data)[17] <- "O_PC_PAM2"
names(data)[18] <- "O_FR_PAM2"
names(data)[19] <- "O_PC_PAM3"
names(data)[20] <- "O_FR_PAM3"
names(data)[21] <- "O_PC_PAM4"
names(data)[22] <- "O_FR_PAM4"

# names(data)[20] <- "FR_AS"

Condition <- c(1, 2, 3, 4)
condNames <- c("ctrl", "sham", "AS", "AF")
conditions <- data.frame(Condition, condNames)
data <- data %>% merge(conditions, by = "Condition")



dataBL<-data %>%
  filter(Condition==1) %>%
  select(c("ID", "C_FR", "C_PC", "R_PC")) %>%
  pivot_longer(
    names_sep = "_",
    cols = c("C_FR", "C_PC", "R_PC"),
    names_to = c("context", "measure"),
    values_to = "BL"
  ) 


df<-data %>%
  select(c("ID", "Condition", "condNames", "C_FR", "C_PC", "R_PC")) %>%
  pivot_longer(
    names_sep = "_",
    cols = c("C_FR", "C_PC", "R_PC"),
    names_to = c("context", "measure"),
    values_to = "value"
  ) %>% merge(dataBL,by=c("ID","context","measure"))%>%mutate(rScore=value-BL) 

dfMeans<-df %>% group_by(context,measure,condNames)%>%dplyr::summarize(avg=mean(value))
dfMeansR<-  df %>% group_by(context,measure,condNames)%>%dplyr::summarize(avg=mean(rScore))
  
ggplot(df,aes(x=condNames,y=rScore))+geom_jitter(width=0.1)+theme_bw()+geom_point(data=dfMeansR,aes(x=condNames,y=avg,color="red",size=1,alpha=.5))+facet_grid(rows=vars(context),cols=vars(measure))
ggplot(df,aes(x=condNames,y=value))+geom_jitter(width=0.1)+theme_bw()+geom_point(data=dfMeans,aes(x=condNames,y=avg,color="red",size=1,alpha=.5))+facet_grid(rows=vars(context,),cols=vars(measure))





data %>%
  select(c("ID", "FRPAM2", "FRPAM3", "FRPAM4")) %>%
  melt(id.vars = "ID") %>%
  filter(!is.na(value)) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  select(-ID) %>%
  as.matrix() %>%
  friedman.test()

data %>%
  select(c("ID", "FRPAM2", "FRPAM3", "FRPAM4")) %>%
  melt(id.vars = "ID") %>%
  filter(!is.na(value)) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  select(-ID) %>%
  as.matrix() %>%
  friedmanmc()

data %>%
  select(c("ID", "Blame", "Condition")) %>%
  pivot_wider(names_from = Condition, values_from = Blame)

data %>%
  select(c("ID", "Blame", "Condition", "FRPAM2", "FRPAM3", "FRPAM4")) %>%
  melt(id.vars = c("ID", "Blame"))
filter(!is.na(value)) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  select(-ID) %>%
  as.matrix() %>%
  friedmanmc()

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
