data_a<- read.csv("C:/Users/micha/Desktop/RESEARCH/psam_pusa.csv")
data_b<- read.csv("C:/Users/micha/Desktop/RESEARCH/psam_pusb.csv")


total <- rbind(data_a,data_b)

#Descriptive Statistics Table

# WAGP - Get rid of observations with NA or 0 
total$WAGP<- as.integer(total$WAGP)
total<-subset(total,!is.na(total$WAGP))
total<-subset(total,!total$WAGP==0)

# PWGTP - Turn weights into integer
total$PWGTP<- as.integer(total$PWGTP)

# SEX - Male=0, Female=1
total$SEX<-ifelse(total$SEX==1,0,1)

# AGE - Turn age into integer
total$AGEP<-as.integer(total$AGEP)

# MIL - Served in military = 1, otherwise 0
total$MIL<-ifelse(total$MIL==4,0,1)

# DIS - able-bodied=0, otherwise 1
total$DIS<-ifelse(total$DIS==2,0,1)

# CIT - citizen=0, otherwise 1
total$CIT<-ifelse(total$CIT==5,1,0)

# HINS3 - Not on medicare=0,  otherwise 1
total$HINS3 <- ifelse(total$HINS3==2,0,1)

# HINS4 - Not on medicaid=0,  otherwise 1
total$HINS4 <- ifelse(total$HINS4==2,0,1)

# MARHM - Not married=0,  otherwise 1
total$MARHM <- ifelse(total$MARHM==2,0,1)

# HICOV - Have health insurance=0,  otherwise 1
total$HICOV <- ifelse(total$HICOV==1,0,1)

# NATIVITY - Native born=0, otherwise 1
total$NATIVITY<-as.integer(total$NATIVITY)
total$NATIVITY<-ifelse(total$NATIVITY==1,0,1)

### SCHL - Disaggregate into 4 indicators, one for each higher ed category, no higher ed is base group ###

# PHD - education.copyPHD - No PHD=0, otherwise 1
education.copyPHD<-as.integer(total$SCHL)
total<-cbind(total,education.copyPHD)
total$education.copyPHD <- ifelse(total$education.copyPHD==24,1,0)
total$PHD <- PHD
PHD<-total$education.copyPHD

# MA - total$education.copyMA - No MA=0, otherwise 1
education.copyMA<-as.integer(total$SCHL)
total<-cbind(total,education.copyMA)
total$education.copyMA <- ifelse(total$education.copyMA==22,1,0)
total$MA <- MA
MA<-total$education.copyMA

# BA - total$education.copyBA - No BA=0, otherwise 1
education.copyBA <- as.integer(total$SCHL)
total<-cbind(total,education.copyBA)
total$education.copyBA <- ifelse(total$education.copyBA==21,1,0)
total$BA <- BA
BA<-total$education.copyBA

# AA - total$education.copyAA - No AA=0, otherwise 1
education.copyAA <- as.integer(total$SCHL)
total<-cbind(total,education.copyAA)
total$education.copyAA <- ifelse(total$education.copyAA==20,1,0)
total$AA <- AA
AA<-total$education.copyAA

# total$WKHP - Usual hours worked last week, as integer, values=1:99
workhours<-as.integer(total$WKHP)

### RAC1P, disaggregate into 8 indicators, one for each category, white is base group ###

# HISP - Hispanic=1, otherwise 0
total$HISP<-as.integer(total$HISP)
total$HISP<-ifelse(total$HISP==1,0,1)

# race.black - African-American=1, otherwise 0
race.black<-as.integer(total$RAC1P)
total<-cbind(total,race.black)
race.black<-ifelse(race.black==2,1,0)

# race.native - American Native/Alaskan Native/Other not specified Native tribe=1
# otherwise 0
race.native<-as.integer(total$RAC1P)
total<-cbind(total,race.native)
race.native<- ifelse(race.native %in% c(3,4,5),1,0)

# race.asian - Asian=1, otherwise 0
race.asian<-as.integer(total$RAC1P)
total<-cbind(total,race.asian)
race.asian<-ifelse(race.asian==6,1,0)

# race.hawaiinOrPIslander - Native Hawaiian and Other Pacific Islander=1, otherwise 0
race.hawaiinOrPIslander<-as.integer(total$RAC1P)
total<-cbind(total,race.hawaiinOrPIslander)
race.hawaiinOrPIslander<- ifelse(race.hawaiinOrPIslander==7,1,0)

# race.other - other race not listed=1, otherwise 0
race.other<-as.integer(total$RAC1P)
total<-cbind(total,race.other)
race.other<- ifelse(race.other==8,1,0)

# race.mixed - two or more races=1, otherwise 0
race.mixed<-as.integer(total$RAC1P)
total<-cbind(total,race.mixed)
race.mixed<- ifelse(race.mixed==9,1,0)

# race.notWhite - Not white=1, otherwise 0
race.notWhiteNew<-as.integer(total$RAC1P)
total<-cbind(total,race.notWhiteNew)
race.notWhiteNew<-ifelse(race.notWhiteNew %in% c(2:9),1,0)
total$race.notWhiteNew <- race.notWhiteNew

# AGEP variable squared 
total$ageSquared <- total$AGEP*total$AGEP

# Model - Multiplicative 1
y<-lm(log(total$WAGP)~total$SEX*total$DIS*race.notWhiteNew + total$NATIVITY +
        total$HINS3 + total$HINS4 + total$MARHM + total$CIT +
        total$MIL + total$AGEP + I(total$AGEP^2)+ AA + BA + MA + PHD + workhours, data=total, weights=total$PWGTP)
summary(y)

# Model - Additative 1
x<-lm(log(total$WAGP)~total$SEX+total$DIS+race.notWhite + total$NATIVITY +
        total$HINS3 + total$HINS4 + total$MARHM + total$CIT +
        total$MIL + total$AGEP + I(total$AGEP^2)+ AA + BA + MA + PHD + workhours, data=total, weights=total$PWGTP)
summary(x)

# Model - Multiplicative 2
w<-lm(log(total$WAGP)~total$SEX*race.notWhite+ total$NATIVITY +
        total$HINS3 + total$HINS4 + total$MARHM + total$CIT +
        total$MIL + total$AGEP + I(total$AGEP^2)+ AA + BA + MA + PHD + workhours, data=total, weights=total$PWGTP)
summary(w)

# Model - Additative 2
z<-lm(log(total$WAGP)~total$SEX+race.notWhite + total$NATIVITY +
        total$HINS3 + total$HINS4 + total$MARHM + total$CIT +
        total$MIL + total$AGEP + I(total$AGEP^2)+ AA + BA + MA + PHD + workhours, data=total, weights=total$PWGTP)
summary(z)

# List of features and the outcome used in model
namesList <- c("WAGP", "SEX", "DIS", "race.notWhiteNew", "NATIVITY", "HINS3", "HINS4", 
               "MARHM", "CIT", "MIL", "AGEP", "ageSquared", "AA", "BA", "MA", "PHD", "workhours")

# Observations used in model
fitObs <- total[rownames(y$model),]
modelOutcomeFeatures <- fitObs[,colnames(fitObs) %in% namesList]

# Summary of observations used in model
summary(modelOutcomeFeatures)
