library(pgirmess)
library(tidyverse)
library(reshape2)
library(MASS)
library(tidyr)
library(car)
library(ggplot2)
library(normalr)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}


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
data$Blame<-ifelse(is.na(data$Blame),"neutral",data$Blame)
data$Condition<-as.factor(data$Condition)



#Means for the Estimate depending after which playthrough they were specified.
Estimate_Playthrough <- data[,c("ID","Estimate","Playthrough_Order")]
#Turn format of the data to wide
Estimate_Playthrough <- spread(Estimate_Playthrough,Playthrough_Order, Estimate)
Estimate_Playthrough$ID<-NULL
summary(Estimate_Playthrough)
friedman.test(as.matrix(Estimate_Playthrough))


#Means for the FR depending on which playthrough they were specified.
FR_Playthrough <- data[,c("ID","FR","Playthrough_Order")]
#Boxplot for FR depending after which playthrough they were specified.
FR_Playthrough_boxplot <- ggplot(FR_Playthrough, aes(as.character(Playthrough_Order), FR))
FR_Playthrough_boxplot + geom_boxplot() + labs(x = as.character("Playthrough Order"), y = "Frustration")
#Turn format of the data to wide
FR_Playthrough <- spread(FR_Playthrough,Playthrough_Order, FR)
FR_Playthrough$ID<-NULL
summary(FR_Playthrough)
friedman.test(as.matrix(FR_Playthrough))

#Means for the PC depending after which playthrough they were specified.
PC_Playthrough <- data[,c("ID","PC","Playthrough_Order")]
#Boxplot for PC depending after which playthrough they were specified.
PC_Playthrough_boxplot <- ggplot(PC_Playthrough, aes(as.character(Playthrough_Order), PC))
PC_Playthrough_boxplot + geom_boxplot() + labs(x = as.character("Playthrough Order"), y = "Perceived Control")
#Turn format of the data to wide
PC_Playthrough <- spread(PC_Playthrough,Playthrough_Order, PC)
PC_Playthrough$ID<-NULL
summary(PC_Playthrough)
friedman.test(as.matrix(PC_Playthrough))



#Wilcox test between R_PC and Estimate
R_PC_var <- c(data$R_PC)
Estimate_var <- c(data$Estimate)
R_PC_var <- normalize(R_PC_var)
Estimate_var <- normalize(Estimate_var)
R_PC_Estimate_data <- data.frame(coding_var= rep(c("R_PC","Estimate"), each = 64), score = c(R_PC_var, Estimate_var))
wilcox.test(as.numeric(R_PC_Estimate_data$score) ~ as.numeric(R_PC_Estimate_data$coding_var))

#Boxplots between R_PC and Estimate
R_PC_Estimate_boxplot <- ggplot(R_PC_Estimate_data, aes(coding_var, score), inherit.aes = FALSE)
R_PC_Estimate_boxplot + geom_boxplot() + labs(x = "", y = "Normalized scores/estimates")



#Scatterplot between frustration and perceived control within all conditions
scatter_FR_PC <-ggplot(data, aes(FR, PC))
scatter_FR_PC + geom_point() + geom_smooth() + labs(x="Frustration", y="Perceived Control")

#scatterplot between frustration and perceived control within Sham
scatter_FR_PC_Sham_data <- data[,c("Condition","PC_Sham","FR_Sham")]
scatter_FR_PC_Sham_data<- scatter_FR_PC_Sham_data%>%filter(!is.na(FR_Sham))
scatter_FR_PC_Sham <-ggplot(scatter_FR_PC_Sham_data, aes(PC_Sham, FR_Sham))
scatter_FR_PC_Sham + geom_point() + geom_smooth() + labs(x="Perceived Control", y="Frustration")

#scatterplot between frustration and perceived control within AS
scatter_FR_PC_AS_data <- data[,c("Condition","PC_AS","FR_AS")]
scatter_FR_PC_AS_data<- scatter_FR_PC_AS_data%>%filter(!is.na(FR_AS))
scatter_FR_PC_AS <-ggplot(scatter_FR_PC_AS_data, aes(PC_AS, FR_AS))
scatter_FR_PC_AS + geom_point() + geom_smooth() + labs(x="Perceived control", y="Frustration")

#scatterplot between frustration and perceived control within AF
scatter_FR_PC_AF_data <- data[,c("Condition","PC_AF","FR_AF")]
scatter_FR_PC_AF_data<- scatter_FR_PC_AF_data%>%filter(!is.na(FR_AF))
scatter_FR_PC_AF <-ggplot(scatter_FR_PC_AF_data, aes(PC_AF, FR_AF))
scatter_FR_PC_AF + geom_point() + geom_smooth() + labs(x="Perceived control", y="Frustration")



#Did frustration with the PAMs depend on the Blame factor within individual PAMs
Blame_FR_PAM <- data[,c("ID","Condition", "Blame", "FR_Sham", "FR_AS", "FR_AF")]
#moving everything into one column
Blame_FR_PAM$FR_Sham<-ifelse(is.na(Blame_FR_PAM$FR_AS), Blame_FR_PAM$FR_Sham, Blame_FR_PAM$FR_AS)
Blame_FR_PAM$FR_AS<-NULL
Blame_FR_PAM$FR_Sham<-ifelse(is.na(Blame_FR_PAM$FR_AF), Blame_FR_PAM$FR_Sham, Blame_FR_PAM$FR_AF)
Blame_FR_PAM$FR_AF<-NULL
#Blame_FR <- Blame_FR%>%filter(!is.na(FR_Sham))%>%pivot_wider(names_from = "Blame", values_from = "FR_Sham")
B2<-Blame_FR_PAM[,c(2,3,4)]%>%filter(!Blame=="neutral" & Condition=="2")
B3<-Blame_FR_PAM[,c(2,3,4)]%>%filter(!Blame=="neutral" & Condition=="3")
B3 <- B3 %>% rename("FR_AS" = "FR_Sham")
B4<-Blame_FR_PAM[,c(2,3,4)]%>%filter(!Blame=="neutral" & Condition=="4")
B4 <- B4 %>% rename("FR_AF" = "FR_Sham")
wilcox.test(as.numeric(B2$FR_Sham) ~ B2$Blame)
wilcox.test(as.numeric(B3$FR_AS) ~ B3$Blame)
wilcox.test(as.numeric(B4$FR_AF) ~ B4$Blame)


  
#Did frustration depend on the Blame factor
Blame_FR <- data[,c("Condition", "Blame", "FR")]
#Control
B_FR1<-Blame_FR[,c(1,2,3)]%>%filter(!Blame=="neutral" & Condition=="1")
wilcox.test(as.numeric(B_FR1$FR) ~ B_FR1$Blame)
#Sham
B_FR2<-Blame_FR[,c(1,2,3)]%>%filter(!Blame=="neutral" & Condition=="2")
wilcox.test(as.numeric(B_FR2$FR) ~ B_FR2$Blame)
#AS
B_FR3<-Blame_FR[,c(1,2,3)]%>%filter(!Blame=="neutral" & Condition=="3")
wilcox.test(as.numeric(B_FR3$FR) ~ B_FR3$Blame)
#AF
B_FR4<-Blame_FR[,c(1,2,3)]%>%filter(!Blame=="neutral" & Condition=="4")
wilcox.test(as.numeric(B_FR4$FR) ~ B_FR4$Blame)



#Did perceived control depend on the Blame factor
Blame_PC <- data[,c("Condition", "Blame", "PC")]
#Control
B_PC1<-Blame_PC[,c(1,2,3)]%>%filter(!Blame=="neutral" & Condition=="1")
wilcox.test(as.numeric(B_PC1$PC) ~ B_PC1$Blame)
#Sham
B_PC2<-Blame_PC[,c(1,2,3)]%>%filter(!Blame=="neutral" & Condition=="2")
wilcox.test(as.numeric(B_PC2$PC) ~ B_PC2$Blame)
#AS
B_PC3<-Blame_PC[,c(1,2,3)]%>%filter(!Blame=="neutral" & Condition=="3")
wilcox.test(as.numeric(B_PC3$PC) ~ B_PC3$Blame)
#AF
B_PC4<-Blame_PC[,c(1,2,3)]%>%filter(!Blame=="neutral" & Condition=="4")
wilcox.test(as.numeric(B_PC4$PC) ~ B_PC4$Blame)




#Friedman test checking whether Blame attribution changed depending on the playthrough order
#Data prep
Blame_Conditions_or_Playthrough <- data[,c("ID","Blame","Playthrough_Order","Condition")]
#Change Blame attribution from char to numeric
Blame_Conditions_or_Playthrough$Blame[Blame_Conditions_or_Playthrough$Blame == "neutral"] <- 0
Blame_Conditions_or_Playthrough$Blame[Blame_Conditions_or_Playthrough$Blame == "Self"] <- 1
Blame_Conditions_or_Playthrough$Blame[Blame_Conditions_or_Playthrough$Blame == "System"] <- -1
Blame_Conditions_or_Playthrough$Blame<-as.numeric(Blame_Conditions_or_Playthrough$Blame)
#Get rid of the Condition column
Blame_Conditions_or_Playthrough$Condition<-NULL
#Turn format of the data to wide
Blame_Conditions_or_Playthrough <- spread(Blame_Conditions_or_Playthrough,Playthrough_Order, Blame)
Blame_Conditions_or_Playthrough$ID<-NULL
friedman.test(as.matrix(Blame_Conditions_or_Playthrough))
friedmanmc(as.matrix(Blame_Conditions_or_Playthrough))


#Friedman test checking whether Blame attribution changed between different conditions
#Data prep
Blame_Conditions_or_Playthrough <- data[,c("ID","Blame","Playthrough_Order","Condition")]
#Change Blame attribution from char to numeric
Blame_Conditions_or_Playthrough$Blame[Blame_Conditions_or_Playthrough$Blame == "neutral"] <- 0
Blame_Conditions_or_Playthrough$Blame[Blame_Conditions_or_Playthrough$Blame == "Self"] <- 1
Blame_Conditions_or_Playthrough$Blame[Blame_Conditions_or_Playthrough$Blame == "System"] <- -1
Blame_Conditions_or_Playthrough$Blame<-as.numeric(Blame_Conditions_or_Playthrough$Blame)
#Get rid of the Plauthrough_Order column
Blame_Conditions_or_Playthrough$Playthrough_Order<-NULL
#Turn format of the data to wide
Blame_Conditions_or_Playthrough <- spread(Blame_Conditions_or_Playthrough,Condition, Blame)
Blame_Conditions_or_Playthrough$ID<-NULL
friedman.test(as.matrix(Blame_Conditions_or_Playthrough))
friedmanmc(as.matrix(Blame_Conditions_or_Playthrough))



#Boxplots for Blame attribution depending on the condition
Blame_Conditions <- data[,c("Blame","Condition")]
#Change Blame attribution from char to numeric
Blame_Conditions$Blame[Blame_Conditions$Blame == "neutral"] <- 0
Blame_Conditions$Blame[Blame_Conditions$Blame == "Self"] <- 1
Blame_Conditions$Blame[Blame_Conditions$Blame == "System"] <- -1
Blame_Conditions$Blame<-as.numeric(Blame_Conditions$Blame)
BC1<-Blame_Conditions[,c(1,2)]%>%filter( Condition=="1")
BC1$Condition<-NULL
BC2<-Blame_Conditions[,c(1,2)]%>%filter( Condition=="2")
BC2$Condition<-NULL
BC3<-Blame_Conditions[,c(1,2)]%>%filter( Condition=="3")
BC3$Condition<-NULL
BC4<-Blame_Conditions[,c(1,2)]%>%filter( Condition=="4")
BC4$Condition<-NULL
BC1_var <- c(BC1$Blame)
BC2_var <- c(BC2$Blame)
BC3_var <- c(BC3$Blame)
BC4_var <- c(BC4$Blame)
Blame_Conditions_long <- data.frame(coding_var= rep(c("Control", "Sham", "AS", "AF"), each = 16), score = c(BC1_var, BC2_var, BC3_var, BC4_var))
Blame_Conditions <- ggplot(Blame_Conditions_long, aes(coding_var, score), inherit.aes = FALSE)
Blame_Conditions + geom_boxplot() + labs(x = "", y = "Blame scores")













#Friedman tests


data %>% 
  select(c("ID", "FR_Sham", "FR_AS", "FR_AF"))%>%melt(id.vars="ID")%>%
  filter(!is.na(value))%>%
  pivot_wider(names_from = variable, values_from = value)%>%
  select(-ID)%>%as.matrix()%>%
  friedman.test()

data %>% 
  select(c("ID", "FR_Sham", "FR_AS", "FR_AF"))%>%melt(id.vars="ID")%>%
  filter(!is.na(value))%>%
  pivot_wider(names_from = variable, values_from = value)%>%
  select(-ID)%>%as.matrix()%>%
  friedmanmc()

data %>% 
  select(c("ID", "Blame","Condition"))%>%pivot_wider(names_from = Condition, values_from = Blame)

Blame_FR <- data %>% 
  select(c("ID", "Blame","Condition", "FR_Sham", "FR_AS", "FR_AF"))%>%melt(id.vars=c("ID","Blame"))%>%
  filter(!is.na(value))%>%
  pivot_wider(names_from = variable, values_from = value)%>%
  select(-ID)%>%as.matrix()%>%
  friedman.test()


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


