library(pgirmess)
library(tidyverse)
library(reshape2)
#library(MASS)
library(tidyr)
library(car)
library(ggplot2)
library(normalr)
library(dplyr)
library(clinfun)
library(pastecs)
library(QuantPsyc)
library(Hmisc)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

rFromWilcox<-function(wilcoxModel, N){
  z<- qnorm(wilcoxModel$p.value/2)
  r<- z/ sqrt(N)
  cat(wilcoxModel$data.name, "Effect Size, r = ", r)
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



#Wilcox test between R_PC and Estimate
R_PC_var <- c(data$R_PC)
Estimate_var <- c(data$Estimate)
R_PC_var <- normalize(R_PC_var)
Estimate_var <- normalize(Estimate_var)
R_PC_Estimate_data <- data.frame(coding_var= rep(c("R_PC","Estimate"), each = 64), score = c(R_PC_var, Estimate_var))
wilcox.test(as.numeric(R_PC_Estimate_data$score) ~ as.numeric(R_PC_Estimate_data$coding_var))

#Boxplots between R_PC and Estimate
R_PC_Estimate_boxplot <- ggplot(R_PC_Estimate_data, aes(coding_var, score), inherit.aes = FALSE)
R_PC_Estimate_boxplot + geom_jitter(width = 0.05, height = 0.05) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", colour = "red") + 
stat_summary(fun.y = mean, geom = "point", colour = "red", size = 4) + labs(x = "", y = "Normalized scores/estimates")



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
FR_Playthrough_boxplot + geom_boxplot() + geom_jitter(width = 0.15, height = 0.1) + labs(x = as.character("Playthrough Order"), y = "Frustration")
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


#Scatterplot between frustration and perceived control within all conditions
scatter_FR_PC_data <- data[,c("Blame", "Condition","PC","FR")]
#Ridiculous way of changing names within in a column (we're in a hurry)
scatter_FR_PC_data$Condition[which(scatter_FR_PC_data$Condition == "1")] = as.character("4.Control")
scatter_FR_PC_data$Condition<-ifelse(is.na(scatter_FR_PC_data$Condition),"4.Control",scatter_FR_PC_data$Condition)
scatter_FR_PC_data$Condition[which(scatter_FR_PC_data$Condition == "2")] = as.character("2.Sham")
scatter_FR_PC_data$Condition<-ifelse(is.na(scatter_FR_PC_data$Condition),"1.Sham",scatter_FR_PC_data$Condition)
scatter_FR_PC_data$Condition[which(scatter_FR_PC_data$Condition == "3")] = as.character("3.AS")
scatter_FR_PC_data$Condition<-ifelse(is.na(scatter_FR_PC_data$Condition),"2.AS",scatter_FR_PC_data$Condition)
scatter_FR_PC_data$Condition[which(scatter_FR_PC_data$Condition == "4")] = as.character("4.AF")
scatter_FR_PC_data$Condition<-ifelse(is.na(scatter_FR_PC_data$Condition),"3.AF",scatter_FR_PC_data$Condition)
scatter_FR_PC <-ggplot(scatter_FR_PC_data, aes(PC, FR, color=Blame, shape=Blame))
scatter_FR_PC + geom_point() + xlim(1,7) + ylim(1,7) + geom_smooth(method=lm, se=FALSE) +geom_jitter(width = .1)+ labs(x="Perceived Control", y="Frustration")+theme_bw()
#Sgnificance Test for Linear Regression
FR_PC.lm = lm(PC ~ FR, data=scatter_FR_PC_data)
summary(FR_PC.lm)
lm.beta(FR_PC.lm)
#Sgnificance Test for Linear Regression for Sham
scatter_FR_PC_Sham_data<- scatter_FR_PC_data%>%filter(Condition=="2.Sham")
FR_PC.lm = lm(PC ~ FR, data=scatter_FR_PC_Sham_data)
summary(FR_PC.lm)
lm.beta(FR_PC.lm)

#Sgnificance Test for Linear Regression for Self
scatter_FR_PC_Self_data<- scatter_FR_PC_data%>%filter(Blame=="Self")
FR_PC.lm = lm(PC ~ FR, data=scatter_FR_PC_Self_data)
summary(FR_PC.lm)
lm.beta(FR_PC.lm)
#Sgnificance Test for Linear Regression for System
scatter_FR_PC_System_data<- scatter_FR_PC_data%>%filter(Blame=="System")
FR_PC.lm = lm(PC ~ FR, data=scatter_FR_PC_System_data)
summary(FR_PC.lm)
lm.beta(FR_PC.lm)
#Sgnificance Test for Linear Regression for Neutral
scatter_FR_PC_Neutral_data<- scatter_FR_PC_data%>%filter(Blame=="neutral")
FR_PC.lm = lm(PC ~ FR, data=scatter_FR_PC_Neutral_data)
summary(FR_PC.lm)
lm.beta(FR_PC.lm)

#scatterplot between frustration and perceived control in regard ot PAM, wihtin the three PAM conditions
scatter_data_PAM <- data[,c("Condition","PC_Sham","FR_Sham","PC_AS","FR_AS","PC_AF","FR_AF")]
scatter_data_PAM<-scatter_data_PAM[-c(1:16),]
scatter_data_PAM$PC_Sham<-ifelse(is.na(scatter_data_PAM$PC_AS), scatter_data_PAM$PC_Sham, scatter_data_PAM$PC_AS)
scatter_data_PAM$PC_AS<-NULL
scatter_data_PAM$PC_Sham<-ifelse(is.na(scatter_data_PAM$PC_AF), scatter_data_PAM$PC_Sham, scatter_data_PAM$PC_AF)
scatter_data_PAM$PC_AF<-NULL
scatter_data_PAM$FR_Sham<-ifelse(is.na(scatter_data_PAM$FR_AS), scatter_data_PAM$FR_Sham, scatter_data_PAM$FR_AS)
scatter_data_PAM$FR_AS<-NULL
scatter_data_PAM$FR_Sham<-ifelse(is.na(scatter_data_PAM$FR_AF), scatter_data_PAM$FR_Sham, scatter_data_PAM$FR_AF)
scatter_data_PAM$FR_AF<-NULL
scatter_data_PAM <- scatter_data_PAM%>%rename("FR" = "FR_Sham")
scatter_data_PAM <- scatter_data_PAM %>% rename("PC" = "PC_Sham")
#Ridiculous way of changing names within in a column (we're in a hurry)
scatter_data_PAM$Condition[which(scatter_data_PAM$Condition == "2")] = as.character("1.Sham")
scatter_data_PAM$Condition<-ifelse(is.na(scatter_data_PAM$Condition),"1.Sham",scatter_data_PAM$Condition)
scatter_data_PAM$Condition[which(scatter_data_PAM$Condition == "3")] = as.character("2.AS")
scatter_data_PAM$Condition<-ifelse(is.na(scatter_data_PAM$Condition),"2.AS",scatter_data_PAM$Condition)
scatter_data_PAM$Condition[which(scatter_data_PAM$Condition == "4")] = as.character("3.AF")
scatter_data_PAM$Condition<-ifelse(is.na(scatter_data_PAM$Condition),"3.AF",scatter_data_PAM$Condition)
scatter_data_PAM_plot <-ggplot(scatter_data_PAM , aes(PC, FR, color=Condition, shape=Condition))
scatter_data_PAM_plot + geom_point() + xlim(1,7) + ylim(1,7) + geom_smooth(method=lm, se=FALSE) +geom_jitter(width = .1)+ labs(x="Perceived Control", y="Frustration")+theme_bw()
#Sgnificance Test for Linear Regression
FR_PC_PAM.lm = lm(PC ~ FR, data=scatter_data_PAM)
summary(FR_PC_PAM.lm)
lm.beta(FR_PC_PAM.lm)
#Sgnificance Test for Linear Regression for Sham within PAMs
scatter_Sham_data<- scatter_data_PAM%>%filter(Condition=="1.Sham")
FR_PC.lm = lm(PC ~ FR, data=scatter_Sham_data)
summary(FR_PC.lm)
lm.beta(FR_PC.lm)


#scatterplot between frustration and perceived control within Sham
scatter_FR_PC_Sham_data <- data[,c("Condition","PC_Sham","FR_Sham")]
scatter_FR_PC_Sham_data<- scatter_FR_PC_Sham_data%>%filter(!is.na(FR_Sham))
scatter_FR_PC_Sham <-ggplot(scatter_FR_PC_Sham_data, aes(PC_Sham, FR_Sham))
scatter_FR_PC_Sham + geom_point() + xlim(1,7) + ylim(1,7) + geom_smooth(method=lm, se=FALSE) +geom_jitter(width = .1)+ labs(x="Perceived Control", y="Frustration")+theme_bw()

#scatterplot between frustration and perceived control within AS
scatter_FR_PC_AS_data <- data[,c("Condition","PC_AS","FR_AS")]
scatter_FR_PC_AS_data<- scatter_FR_PC_AS_data%>%filter(!is.na(FR_AS))
scatter_FR_PC_AS <-ggplot(scatter_FR_PC_AS_data, aes(PC_AS, FR_AS))
scatter_FR_PC_AS + geom_point() + xlim(1,7) + ylim(1,7) + geom_smooth(method=lm, se=FALSE) + geom_jitter(width = .1)+ labs(x="Perceived control", y="Frustration")+theme_bw()

#scatterplot between frustration and perceived control within AF
scatter_FR_PC_AF_data <- data[,c("Condition","PC_AF","FR_AF")]
scatter_FR_PC_AF_data<- scatter_FR_PC_AF_data%>%filter(!is.na(FR_AF))
scatter_FR_PC_AF <-ggplot(scatter_FR_PC_AF_data, aes(PC_AF, FR_AF))
scatter_FR_PC_AF + geom_point() + xlim(1,7) + ylim(1,7) + geom_smooth(method=lm, se=FALSE) +geom_jitter(width = .1)+ labs(x="Perceived control", y="Frustration")+theme_bw()





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



#Did perceived control with the PAMs depend on the Blame factor within individual PAMs
Blame_PC_PAM <- data[,c("ID","Condition", "Blame", "PC_Sham", "PC_AS", "PC_AF")]
#moving everything into one column
Blame_PC_PAM$PC_Sham<-ifelse(is.na(Blame_PC_PAM$PC_AS), Blame_PC_PAM$PC_Sham, Blame_PC_PAM$PC_AS)
Blame_PC_PAM$PC_AS<-NULL
Blame_PC_PAM$PC_Sham<-ifelse(is.na(Blame_PC_PAM$PC_AF), Blame_PC_PAM$PC_Sham, Blame_PC_PAM$PC_AF)
Blame_PC_PAM$PC_AF<-NULL
C2<-Blame_PC_PAM[,c(2,3,4)]%>%filter(!Blame=="neutral" & Condition=="2")
C3<-Blame_PC_PAM[,c(2,3,4)]%>%filter(!Blame=="neutral" & Condition=="3")
C3 <- C3 %>% rename("PC_AS" = "PC_Sham")
C4<-Blame_PC_PAM[,c(2,3,4)]%>%filter(!Blame=="neutral" & Condition=="4")
C4 <- C4 %>% rename("PC_AF" = "PC_Sham")
wilcox.test(as.numeric(C2$PC_Sham) ~ C2$Blame)
wilcox.test(as.numeric(C3$PC_AS) ~ C3$Blame)
wilcox.test(as.numeric(C4$PC_AF) ~ C4$Blame)

#Did perceived control depend on the Blame factor
Blame_PC <- data[,c("ID","Condition", "Blame", "PC")]
#Control
B_PC1<-Blame_PC[,c(2,3,4)]%>%filter(!Blame=="neutral" & Condition=="1")
wilcox.test(as.numeric(B_PC1$PC) ~ B_PC1$Blame)
#Sham
B_PC2<-Blame_PC[,c(2,3,4)]%>%filter(!Blame=="neutral" & Condition=="2")
wilcox.test(as.numeric(B_PC2$PC) ~ B_PC2$Blame)
#AS
B_PC3<-Blame_PC[,c(2,3,4)]%>%filter(!Blame=="neutral" & Condition=="3")
wilcox.test(as.numeric(B_PC3$PC) ~ B_PC3$Blame)
#AF
B_PC4<-Blame_PC[,c(2,3,4)]%>%filter(!Blame=="neutral" & Condition=="4")
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
Blame_Condition_wilcox_model <- wilcox.test(Blame_Conditions_or_Playthrough$"2", Blame_Conditions_or_Playthrough$"3", paired=TRUE, correct=FALSE)
Blame_Condition_wilcox_model
rFromWilcox(Blame_Condition_wilcox_model, 32)

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
Blame_Conditions_long <- data.frame(coding_var= rep(c("1 - Control", "2 - Sham", "3 - AS", "4 - AF"), each = 16), score = c(BC1_var, BC2_var, BC3_var, BC4_var))
Blame_Conditions <- ggplot(Blame_Conditions_long, aes(coding_var, score), inherit.aes = FALSE)
Blame_Conditions + geom_boxplot() + geom_jitter(width = 0.3, height = 0.05)  + labs(x = "", y = "Blame scores")

