library(caret)
library(xlsx)
library(ggplot2)
library(dplyr)
library(tidyr)
data <- read.csv(file.choose())

data <- subset(data, data$RENT > 0)

data$HHSEX_NEW <- as.integer(data$HHSEX)
data$HHRACE_NEW <- as.integer(data$HHRACE)
data$HHMAR_NEW <-as.integer(data$HHMAR)

#Exclude all obs with not applicable var
data_new <- subset(data,!data$HHSEX_NEW == 1)
#data_new3 <- subset(data_new, !data_new$RATINGNH == -6) #missing=8 after subsetting
data_new <- subset(data_new, data_new$HHRACE_NEW > 1)

#data_new <- subset(data_new, data_new$FINROOMS > 0 & data_new$BEDROOMS > 0)

# Crowding ratio
data_new$CROWD <- data_new$NUMPEOPLE / data_new$TOTROOMS


#### At this point, obs in data_new are renters that have
#### been asked the HHSEX, HHRACE,HHGRAD question, n = 7174

## Householder characteristics

# HHMAR_NEW = 1 if married, 0 else
data_new$HHMAR_NEW <- ifelse(data_new$HHMAR_NEW %in% c(2,3), 1, 0)

# HHSEX_NEW = 1 if female, 0 otherwise
data_new$HHSEX_NEW <- ifelse(data_new$HHSEX_NEW == 3, 1, 0)

# HHIMMIGRANT = 1 if foreign born, 0 else
data_new$HHCITSHP_NEW <- droplevels.factor(data_new$HHCITSHP)
data_new$HHCITIZEN <- as.integer(data_new$HHCITSHP_NEW)
data_new$HHIMMIGRANT <- ifelse(data_new$HHCITIZEN %in% c(1,2,3),0,1)

##############################################################
###### Determine binary control vars w/ missing values and recode ###


# Ordinal turned into binary, if rent subsidy =1, 0 else, missing = -1
data_new$RENTSUB_NEW <- droplevels.factor(data_new$RENTSUB)
data_new$RENTSUB_NEW <- as.integer(data_new$RENTSUB_NEW)
data_new$RENTSUB_NEW[data_new$RENTSUB_NEW == 1] <- -1
data_new$RENTSUB_NEW[data_new$RENTSUB_NEW == 2] <- 1
data_new$RENTSUB_NEW[data_new$RENTSUB_NEW == 3] <- 1
data_new$RENTSUB_NEW[data_new$RENTSUB_NEW == 4] <- 1
data_new$RENTSUB_NEW[data_new$RENTSUB_NEW == 5] <- 1
data_new$RENTSUB_NEW[data_new$RENTSUB_NEW == 6] <- 1
data_new$RENTSUB_NEW[data_new$RENTSUB_NEW == 7] <- 1
data_new$RENTSUB_NEW[data_new$RENTSUB_NEW == 8] <- 1
data_new$RENTSUB_NEW[data_new$RENTSUB_NEW == 9] <- 0

### Binary var, missing = -1, yes = 1, no = 0
data_new$FS[data_new$FS == "'-6'"] <- "'2'"
data_new$FS <- droplevels.factor(data_new$FS)
data_new$FS <- as.integer(data_new$FS)
data_new$FS[data_new$FS == 3] <- 0
data_new$FS[data_new$FS == 1] <- -1
data_new$FS[data_new$FS == 2] <- 1

### Binary var, missing = -1, yes = 1, no = 0
data_new$RENTCNTRL[data_new$RENTCNTRL == "'-6'"] <- "'2'"
data_new$RENTCNTRL <- droplevels.factor(data_new$RENTCNTRL)
data_new$RENTCNTRL <- as.integer(data_new$RENTCNTRL)
data_new$RENTCNTRL[data_new$RENTCNTRL == 3] <- 0
data_new$RENTCNTRL[data_new$RENTCNTRL == 1] <- -1
data_new$RENTCNTRL[data_new$RENTCNTRL == 2] <- 1

### Binary var, missing = -1, yes = 1, no = 0
data_new$NHQSCHOOL[data_new$NHQSCHOOL == "'-6'"] <- "'2'"
data_new$NHQSCHOOL <- droplevels.factor(data_new$NHQSCHOOL)
data_new$NHQSCHOOL <- as.integer(data_new$NHQSCHOOL)
data_new$NHQSCHOOL[data_new$NHQSCHOOL == 3] <- 0
data_new$NHQSCHOOL[data_new$NHQSCHOOL == 1] <- -1
data_new$NHQSCHOOL[data_new$NHQSCHOOL == 2] <- 1

### Binary var, missing = -1, yes = 1, no = 0
data_new$NHQPCRIME[data_new$NHQPCRIME == "'-6'"] <- "'2'"
data_new$NHQPCRIME <- droplevels.factor(data_new$NHQPCRIME)
data_new$NHQPCRIME <- as.integer(data_new$NHQPCRIME)
data_new$NHQPCRIME[data_new$NHQPCRIME == 3] <- 0
data_new$NHQPCRIME[data_new$NHQPCRIME == 1] <- -1
data_new$NHQPCRIME[data_new$NHQPCRIME == 2] <- 1

### Binary var, missing = -1, yes = 1, no = 0
data_new$NHQSCRIME[data_new$NHQSCRIME == "'-6'"] <- "'2'"
data_new$NHQSCRIME <- droplevels.factor(data_new$NHQSCRIME)
data_new$NHQSCRIME <- as.integer(data_new$NHQSCRIME)
data_new$NHQSCRIME[data_new$NHQSCRIME == 3] <- 0
data_new$NHQSCRIME[data_new$NHQSCRIME == 1] <- -1
data_new$NHQSCRIME[data_new$NHQSCRIME == 2] <- 1

### Binary var, missing = -1, one or more barred-window build = 1, no = 0
data_new$NEARBARCL[data_new$NEARBARCL == "'-6'"] <- "'2'"
data_new$NEARBARCL[data_new$NEARBARCL == "'2'"] <- "'1'"
data_new$NEARBARCL <- droplevels.factor(data_new$NEARBARCL)
data_new$NEARBARCL <- as.integer(data_new$NEARBARCL)
data_new$NEARBARCL[data_new$NEARBARCL == 3] <- 0
data_new$NEARBARCL[data_new$NEARBARCL == 1] <- -1
data_new$NEARBARCL[data_new$NEARBARCL == 2] <- 1

### Binary var, missing = -1, one or more abandoned = 1, no aban/ or any build nearby = 0
data_new$NEARABAND[data_new$NEARABAND == "'-6'"] <- "'3'"
data_new$NEARABAND[data_new$NEARABAND == "'2'"] <- "'1'"
data_new$NEARABAND[data_new$NEARABAND == "'3'"] <- "'4'"
data_new$NEARABAND <- droplevels.factor(data_new$NEARABAND)
data_new$NEARABAND <- as.integer(data_new$NEARABAND)
data_new$NEARABAND[data_new$NEARABAND == 3] <- 0
data_new$NEARABAND[data_new$NEARABAND == 1] <- -1
data_new$NEARABAND[data_new$NEARABAND == 2] <- 1

###### One Shot non-missing categorical vars

# One shot geographic flag OMB13CBSA into 9 indicator vectors; base Baltimore 
dmy <- dummyVars(" ~ OMB13CBSA", data = data_new, fullRank=T)
data_geography_oneshot <- data.frame(predict(dmy, newdata = data_new))
names(data_geography_oneshot) <- c("Birmingham", "LasVegas", "Minneapolis","OklahomaCity","Richmond",
                                   "Rochester","SanAntonio","SanJose","Tampa")
data_new <- cbind(data_new,data_geography_oneshot)

# Oneshot HOUSEHOLD TYPE, no missing data, base married-couple
data_new$HSHLDTYPE <-droplevels.factor(data_new$HSHLDTYPE)
dmy1 <- dummyVars(" ~ HSHLDTYPE", data = data_new, fullRank=T)
data_householdtype_oneshot <- data.frame(predict(dmy1, newdata = data_new))
names(data_householdtype_oneshot) <- c("Male.NoWife","Female.NoHusband","Male.Alone",
                                       "Male.NotAlone,","Female.Alone","Female.NotAlone")
data_new <- cbind(data_new,data_householdtype_oneshot)

# Oneshot HUDSUB, base is public housing tents
data_new$HUDSUB <- droplevels.factor(data_new$HUDSUB)
dmy2 <- dummyVars(" ~ HUDSUB", data = data_new, fullRank=T)
data_HUDsub_oneshot <- data.frame(predict(dmy2, newdata = data_new))
names(data_HUDsub_oneshot) <- c("VoucherRecepient","AllOther")
data_new <- cbind(data_new,data_HUDsub_oneshot)

# One shot BLD, base is trailer/mobile home
dmy3 <- dummyVars(" ~ BLD", data = data_new, fullRank=T)
data_bld_oneshot <- data.frame(predict(dmy3, newdata = data_new))
names(data_bld_oneshot)<-c("OneFamilyHomeDetached","OneFamilyHomeAttached","2Apartments","3to4Apartments","5to9Apartments",
                           "10to19Apartments","20to49Apartments","50orMoreApartments","RVBoatVanEtc")
data_new <- cbind(data_new,data_bld_oneshot)

# LOTSIZE, base case, not applicable, because HH is condo/multi-family unit
dmy4 <- dummyVars(" ~ LOTSIZE", data = data_new, fullRank=T)
data_lotsize_oneshot <- data.frame(predict(dmy4, newdata = data_new))
names(data_lotsize_oneshot)<-c("LessThan8thAcre","8thTo4thAcre","4thToHalfAcre","HalfToFullAcre",
                               "OneToFiveAcres","FiveToTenAcres","TenOrMoreAcres")
data_new <- cbind(data_new,data_lotsize_oneshot)

# UPKEEP, base case is < 3 upkeep problems
dmy5 <- dummyVars(" ~ UPKEEP", data = data_new, fullRank=T)
data_upkeep_oneshot <- data.frame(predict(dmy5, newdata = data_new))
names(data_upkeep_oneshot)<-c("3to4UpkeepProblems","5orMoreUpkeepProblems")
data_new <- cbind(data_new, data_upkeep_oneshot)

# ADEQUACY, base case is Adequate
dmy6 <- dummyVars(" ~ ADEQUACY", data = data_new, fullRank=T)
data_adequacy_oneshot <- data.frame(predict(dmy6, newdata = data_new))
names(data_adequacy_oneshot)<-c("ModeratelyInadequate","SeverelyInadequate")
data_new <- cbind(data_new, data_adequacy_oneshot)


#### HHRACE MANIPULATION ####

# HHRACE_POC = 1 if POC, else 0 (white)
data_new$HHPOC <- ifelse(data_new$HHRACE_NEW == 2, 0, 1)


data_new$HHSPAN_new <- as.integer(data_new$HHSPAN)
# Hispanic indicator, 1 if Hispanic, 0 else
data_new$HHSPAN_new <- ifelse(data_new$HHSPAN_new == 3, 0, 1)

# All races, White=2, Black=3, American Indian/Native=4, Asian=5, Hawaiian/PacIslander=6, Mixed Race=7
data_new$HHRACE_NEW3 <- data_new$HHRACE_NEW
data_new$HHRACE_NEW3 <-recode(data_new$HHRACE_NEW3,"1"=1, "2"=2,"3"=3,"4"=4,"5"=5,"6"=4, "7"=7, "8"=7, "9"=7, "10"=7, "11"=7,"12"=7,"13"=7,"14"=7,"15"=7,"16"=7,"17"=7,"18"=7,"19"=7,"20"=7)
# collapse Natives/PacIslanders into '4', "others"
data_new$HHRACEALL <- as.factor(data_new$HHRACE_NEW3)
data_new$HHRACEALL <- droplevels.factor(data_new$HHRACEALL)


# Base case is White
dmy5 <- dummyVars(" ~ HHRACEALL", data = data_new, fullRank=T)
data_hhraceall_oneshot <- data.frame(predict(dmy5, newdata = data_new))
names(data_hhraceall_oneshot)<-c("Black","Other","Asian",
                                  "MixedRace")
data_new <- cbind(data_new, data_hhraceall_oneshot)

# Crowding ratio
data_new$CROWD <- data_new$NUMPEOPLE / data_new$TOTROOMS
data_new$CROWD1 <- data_new$NUMPEOPLE / (data_new$BEDROOMS + data_new$FINROOMS)
data_new$ROOMS <- data_new$BEDROOMS + data_new$FINROOMS

#data_new <- subset(data_new, data_new$BEDROOMS > 0)


### Ordinal var, missing = -1
data_new$UNIT_SIZE_NEW <- as.integer(data_new$UNITSIZE)
data_new$UNIT_SIZE_NEW[data_new$UNIT_SIZE_NEW == 1] <- -1
data_new$UNIT_SIZE_NEW[data_new$UNIT_SIZE_NEW == 2] <- 1
data_new$UNIT_SIZE_NEW[data_new$UNIT_SIZE_NEW == 3] <- 2
data_new$UNIT_SIZE_NEW[data_new$UNIT_SIZE_NEW == 4] <- 3
data_new$UNIT_SIZE_NEW[data_new$UNIT_SIZE_NEW == 5] <- 4
data_new$UNIT_SIZE_NEW[data_new$UNIT_SIZE_NEW == 6] <- 5
data_new$UNIT_SIZE_NEW[data_new$UNIT_SIZE_NEW == 7] <- 6
data_new$UNIT_SIZE_NEW[data_new$UNIT_SIZE_NEW == 8] <- 7
data_new$UNIT_SIZE_NEW[data_new$UNIT_SIZE_NEW == 9] <- 8
data_new$UNIT_SIZE_NEW[data_new$UNIT_SIZE_NEW == 10] <- 9


#data_graph <- subset(data_new, data_new$RENT < 2500)
#
#data_graph$Treated.Control <- data_graph$HHRACE_NEW1*data_graph$HHSEX_NEW
#
#colnames(data_new) <- make.unique(names(data_new))
#RENT <- data_new$RENT
#c <- ggplot(data_graph, aes(data_graph$RENT)) + geom_density()
#c
#d <- ggplot(data_graph, aes(x = factor(data_graph$HHSEX), y=data_graph$RENT, fill = HHSEX)) + geom_violin(alpha=0.7) + geom_boxplot(width=0.1) + scale_x_discrete(labels = c('Male','Female')) + scale_y_continuous(breaks=seq(0,2500,100)) +  xlab("Sex") + ylab('Rent, 2017 Dollars') + theme(legend.position = "none", axis.text.x = element_text(face="bold", size = 10))                                                                                                                                                                                                                                                                                                                                                    
#e <- ggplot(data_graph, aes(x = factor(data_graph$HHRACE_NEW3), y=data_graph$RENT, fill = factor(HHRACE_NEW3))) + geom_violin(alpha=0.7) + geom_boxplot(width=0.1) + scale_x_discrete(labels = c('White','Black','Asian','Native American','Pac. Islander','Mixed')) + scale_y_continuous(breaks=seq(0,2500,100))  + xlab("Race") + ylab('Rent, 2017 Dollars') +theme(legend.position = "none", axis.text.x = element_text(face="bold", size=10)) + scale_fill_brewer(palette="BuPu") + ggtitle("Rent of Householder by Race, 2017 Dollars") + coord_flip() 
#f <- ggplot(data_graph, aes(x = factor(data_graph$Treated.Control), y=data_graph$RENT, fill = Treated.Control)) + geom_violin(alpha=0.7) + geom_boxplot(width=0.1) + scale_x_discrete(labels = c('White Male','Female of Color')) + scale_y_continuous(breaks=seq(0,2500,100)) +  xlab("Race/Sex") + ylab('Rent, 2017 Dollars') + theme(legend.position = "none", axis.text.x = element_text(face="bold", size = 10)) + coord_flip()                                                                                                                                                                                                                                                                                                                                             
#f

data_new$OMB13CBSA <- as.integer(data_new$OMB13CBSA)
data_new$HHGRAD <- as.integer(data_new$HHGRAD)
data_new$HSHLDTYPE <- as.integer(data_new$HSHLDTYPE)
data_new$BLD <- as.integer(data_new$BLD)
data_new$LOTSIZE <- as.integer(data_new$LOTSIZE)
data_new$UPKEEP <- as.integer(data_new$UPKEEP)
data_new$ADEQUACY <- as.integer(data_new$ADEQUACY)
data_new$HUDSUB <- as.integer(data_new$HUDSUB)

data_new1 <- subset(data_new, data_new$HINCP > 0)

data_new1$MHINCP  <- data_new1$HINCP / 12
data_new1$CB <- (data_new1$RENT + data_new1$UTILAMT) / (data_new1$MHINCP)
data_new1 <- subset(data_new1, data_new1$CB < 1)
data_new1 <- subset(data_new1, data_new1$RENT >= 100)
data_new1$COSTBURDEN <- ifelse(data_new1$CB >= .3, 1, 0)

#data_new1 <- subset(data_new1, data_new1$MHINCP < 5000)
#data_new1 <- subset(data_new1, data_new1$MHINCP > 100)

#data_new1 <- subset(data_new1, data_new1$CB > 100)

data_use <- data_new1[,c("RENT","OMB13CBSA", "CROWD1","CB","COSTBURDEN", "HHPOC", "MHINCP" ,"HHGRAD", "HHSPAN_new", "HHMAR_NEW","HHRACEALL","HHSEX_NEW","HHAGE", "HINCP", "UTILAMT", "HHIMMIGRANT","HSHLDTYPE","RATINGHS", "RATINGNH", "HUDSUB","BLD","LOTSIZE",
                     "UPKEEP","ADEQUACY","NUMPEOPLE","TOTROOMS","CROWD","UNIT_SIZE_NEW","RENTSUB_NEW","FS","RENTCNTRL","NHQSCHOOL",
                     "NHQPCRIME","NHQSCRIME","NEARBARCL","NEARABAND","WEIGHT")]

ggplot(data_use, aes(x=factor(data_use$OMB13CBSA), y=data_use$RENT)) + stat_summary(fun.y="mean", geom="bar")

# =1 if has hud sub, 0 if not
data_use$HUDSUBT <- ifelse(data_use$HUDSUB == 3, 1, 0)
# = 1 if inadequate housing, 0 if adequate
data_use$ADEQUACYT <- ifelse(data_use$ADEQUACY == 1, 0, 1)


data_ttest <- data_use[,c("RENT","COSTBURDEN", "CROWD", "NUMPEOPLE","TOTROOMS", "HHSEX_NEW","HHRACEALL", "MHINCP","UTILAMT","HHIMMIGRANT", "HHSPAN_new","HHAGE","HHGRAD","HHMAR_NEW", "HUDSUBT","ADEQUACYT") ]

t.test(data_ttest$RENT[data_ttest$HHSEX_NEW==1],data_ttest$RENT[data_ttest$HHSEX_NEW==0])
t.test(data_ttest$CROWD[data_ttest$HHSEX_NEW==1],data_ttest$CROWD[data_ttest$HHSEX_NEW==0])
t.test(data_ttest$COSTBURDEN[data_ttest$HHSEX_NEW==1],data_ttest$COSTBURDEN[data_ttest$HHSEX_NEW==0])
table(data_ttest$HHRACEALL==2,data_ttest$HHSEX_NEW)

data_ttestWHITE <- subset(data_ttest, data_ttest$HHRACEALL==2)
t.test(data_ttestWHITE$HHSEX_NEW==1,data_ttestWHITE$HHSEX_NEW==0)

data_ttestBLACK <- subset(data_ttest, data_ttest$HHRACEALL==3)
t.test(data_ttestBLACK$HHSEX_NEW==1,data_ttestBLACK$HHSEX_NEW==0)

data_ttestASIAN <- subset(data_ttest, data_ttest$HHRACEALL==5)
t.test(data_ttestASIAN$HHSEX_NEW==1,data_ttestASIAN$HHSEX_NEW==0)

data_ttestOTHER <- subset(data_ttest, data_ttest$HHRACEALL==4)
t.test(data_ttestOTHER$HHSEX_NEW==1,data_ttestOTHER$HHSEX_NEW==0)

data_ttestMIXED <- subset(data_ttest, data_ttest$HHRACEALL==7)
t.test(data_ttestMIXED$HHSEX_NEW==1,data_ttestMIXED$HHSEX_NEW==0)

data_ttestHISP <- subset(data_ttest, data_ttest$HHSPAN_new==1)
t.test(data_ttestHISP$HHSEX_NEW==1,data_ttestHISP$HHSEX_NEW==0)

data_ttestIM <- subset(data_ttest, data_ttest$HHIMMIGRANT==1)
t.test(data_ttestIM$HHSEX_NEW==1,data_ttestIM$HHSEX_NEW==0)

data_ttestMR <- subset(data_ttest, data_ttest$HHMAR_NEW==1)
t.test(data_ttestMR$HHSEX_NEW==1,data_ttestMR$HHSEX_NEW==0)

data_ttestMR <- subset(data_ttest, data_ttest$HHMAR_NEW==1)
t.test(data_ttestMR$HHSEX_NEW==1,data_ttestMR$HHSEX_NEW==0)

data_ttestHUD <- subset(data_ttest, data_ttest$HUDSUBT==1)
t.test(data_ttestHUD$HHSEX_NEW==1,data_ttestHUD$HHSEX_NEW==0)

data_ttestAD <- subset(data_ttest, data_ttest$ADEQUACYT==1)
t.test(data_ttestAD$HHSEX_NEW==1,data_ttestAD$HHSEX_NEW==0)


t.test(data_ttest$HHAGE[data_ttest$HHSEX_NEW==1],data_ttest$HHAGE[data_ttest$HHSEX_NEW==0])

t.test(data_ttest$MHINCP[data_ttest$HHSEX_NEW==1],data_ttest$MHINCP[data_ttest$HHSEX_NEW==0])
t.test(data_ttest$UTILAMT[data_ttest$HHSEX_NEW==1],data_ttest$UTILAMT[data_ttest$HHSEX_NEW==0])
t.test(data_ttest$HHGRAD[data_ttest$HHSEX_NEW==1],data_ttest$HHGRAD[data_ttest$HHSEX_NEW==0])

t.test(data_ttest$HHGRAD[data_ttest$HHSEX_NEW==1],data_ttest$HHGRAD[data_ttest$HHSEX_NEW==0])
t.test(data_ttest$TOTROOMS[data_ttest$HHSEX_NEW==1],data_ttest$TOTROOMS[data_ttest$HHSEX_NEW==0])
t.test(data_ttest$NUMPEOPLE[data_ttest$HHSEX_NEW==1],data_ttest$NUMPEOPLE[data_ttest$HHSEX_NEW==0])

library(broom)

t.test(data=data_ttest, RENT)


#write.csv(data_use, "AHSFINALDATASET.csv", row.names=F)
