library(haven)
library(tidyverse)
library(dplyr)
library(nnet)

#This code gives point estimates of the obs disparity, red disparity and res disparity
#This model includes education as a factor. If not adjusting for education, take out D_EDUC from models
#Take note of the race included in the data1 statement. If stratifying, change that statement to include one or the other
#If Stratifying by race, take out W1_D_RACE_SUMMARY from models
#At the end of this code, update the write.csv file name for current date


#load and clean data
data <- read_sas(".../mhc_khstarbw.sas7bdat", 
                 NULL)
variable.names(data)
table(data$W1_D_RACE_SUMMARY)
data$ID<-as.character(data$STUDYID)
dataO<-data %>% filter(W1_INTERVIEW_AGE_PHI>=65)

#This step is filtering for both black and white. If stratifying, update this statement to include one or the other
data1<- data0 %>% filter(W1_D_RACE_SUMMARY=="Black" | W1_D_RACE_SUMMARY=="White")
table(data1$W1_D_RACE_SUMMARY)


data1$HiBP<-ifelse(data1$MHC_HYP==1, 1, 0)
data1$LowBP<-ifelse(data1$MHC_HYP==0, 1, 0)
data1$HiEduc<-ifelse(data1$PARENT_COL==1, 1, 0)
data1$LowEduc<-ifelse(data1$PARENT_COL==0, 1, 0)

data1$Female<-ifelse(data1$W1_D_GENDER==2, 1, 0)
data1$D_EDUC<-ifelse(data1$W1_D_EDUCATION==1 | data1$W1_D_EDUCATION==2 | data1$W1_D_EDUCATION==9 | data1$W1_D_EDUCATION==10 | data1$W1_D_EDUCATION==7, "LEQ Highschool Grad", 
                     ifelse(data1$W1_D_EDUCATION==3 | data1$W1_D_EDUCATION==4, "Tech/Trade or Some College", ifelse(data1$W1_D_EDUCATION==5 | data1$W1_D_EDUCATION==6, "College Grad or Grad School", "Not Sure")))

rm(modeldomain)
rm(BOOTST)


hist(data1$W1_SENAS_vrmem)

modeldomain<- function(Y) {
  #EHL: ANALYSIS STARTS HERE
  
  data2 <- data1[complete.cases(data1[,c(Y)]), ]
  
  #Estimate p(HiEduc) and p(HiEduc|age) (Eq 7b)
  
  pHiEduc<- nrow(data2 %>% filter(PARENT_COL==1))/nrow(data2)
  
  modHiEduc<- glm(HiEduc ~ W1_INTERVIEW_AGE_PHI, data=data2, family=binomial(link="logit"))
  
  data2$predHiEduccond<-predict.glm(modHiEduc, newdata=data2, type="response")
  
  
  #Estimate p(LowEduc) and p(LowEduc|age) (Eq 8b)
  
  pLowEduc <- nrow(data2 %>% filter(PARENT_COL==0))/nrow(data2)
  
  modLowEduc <- glm(LowEduc ~ W1_INTERVIEW_AGE_PHI, data=data2, family=binomial(link="logit"))
  
  data2$predLowEduccond<-predict.glm(modLowEduc, newdata=data2, type="response")
  
  
  #Estimate p(M|LowEduc, age, sex: target-allowable variables) (Numerator of Eq 9b)
  

  
  modHiBPHiEduc <-glm(HiBP ~ W1_INTERVIEW_AGE_PHI+Female, family=binomial(link="logit"), data=data2 %>% filter(PARENT_COL==1))
  
  data2[,c("pHiBPHiEduc")]<-predict.glm(modHiBPHiEduc, newdata=data2, type="response")
  
  modLowBPHiEduc <-glm(LowBP ~ W1_INTERVIEW_AGE_PHI+Female, family=binomial(link="logit"), data=data2 %>% filter(PARENT_COL==1))
  
  data2[, c("pLowBPHiEduc")]<-predict(modLowBPHiEduc, newdata=data2, type="response")
  
  
data2$num<-ifelse(data2$MHC_HYP==0, data2$pLowBPHiEduc, ifelse(data2$MHC_HYP==1,  data2$pHiBPHiEduc, 0))
  
  summary(data2$num)
  
  
  #Estimate p(M|High Education, age, sex, non-allowables: education and race) (Denominator of Eq 9b)
  
  modHiBPLowEduc <-glm(HiBP ~ W1_INTERVIEW_AGE_PHI + Female+D_EDUC+W1_D_RACE_SUMMARY, family=binomial(link="logit"), data=data2 %>% filter(PARENT_COL==0))  #Take out D_EDUC here if not adjusting for education
  
  
  data2[,c("pHiBPLowEduc")]<-predict(modHiBPLowEduc, newdata=data2, type="response")
  
  modLowBPLowEduc <-glm(LowBP ~ W1_INTERVIEW_AGE_PHI + Female+D_EDUC + W1_D_RACE_SUMMARY , family=binomial(link="logit"), data=data2 %>% filter(PARENT_COL==0)) #Take out D_EDUC here if not adjusting for education
  
  data2[, c("pLowBPLowEduc")]<-predict(modLowBPLowEduc, newdata=data2, type="response")
  
  
  data2$denom<-ifelse(data2$MHC_HYP==0, data2$pLowBPLowEduc, ifelse(data2$MHC_HYP==1,  data2$pHiBPLowEduc, 0))
  
  summary(data2$denom)
  
  #Calculate final weights
  
  data2 <- data2 %>% mutate(w0 = case_when(PARENT_COL==0 ~ pLowEduc/predLowEduccond, TRUE ~ 0),
                            w0prime = case_when(PARENT_COL==1 ~ pHiEduc/predHiEduccond, TRUE ~ 0),
                            w0rmpw = case_when(PARENT_COL==0 ~ (num/denom)*(pLowEduc/predLowEduccond), TRUE ~ 0))
  
  #Estimate p(Y) given w0
  which(is.na(data2$w0))
  obsLowEduc<-weighted.mean(as.numeric(unlist(data2[, c(Y)])), w = data2$w0, na.rm = T) 
  obsLowEduc
  
  #compare to crude
  data2 %>% filter(LowBP==1) %>% dplyr::summarise(MEANSENAS=mean(data2[,c(Y)]))
  
  #check by comparing to age-stand with categories
  data2$age_cat<-cut(data2$W1_INTERVIEW_AGE_PHI,
                     breaks=c(64, 70, 75, 80, 85, 90, 95, 150),
                     labels=c('65-70', '70-75', '75-80', '80-85', '85-90', '90-95', '95+'))
  
  
  stdpop <- data2 %>% group_by(age_cat) %>% 
    dplyr::summarise(n = n())
  stdpop$prop<-stdpop$n/nrow(data2)
  
  stzdLowEduc<-data2 %>% filter(LowEduc==1) %>%  group_by(age_cat, Female) %>% 
    dplyr::summarise(meanY = mean(data2[,c(Y)])) %>% 
    left_join(stdpop , by = "age_cat", "Female") %>% dplyr::summarise(Yobs = crossprod(meanY, prop))
  
  stzdLowEduc
  
  
  #Estiamte p(Y) given w'0
  obsHiEduc<-weighted.mean(as.numeric(unlist(data2[, c(Y)])), w = data2$w0prime, na.rm = T) 
  obsHiEduc
  #compare to crude
  
  data2 %>% filter(LowEduc==0) %>% dplyr::summarise(MEANSENAS=mean(data2[,c(Y)]))
  
  #check by comparing to age-stand in categories
  stzdHiEduc<-data2 %>% filter(HiEduc==1)%>%  group_by(age_cat, Female) %>% 
    dplyr::summarise(meanY = mean(data2[,c(Y)])) %>%  
    left_join(stdpop , by = "age_cat", "Female") %>% dplyr::summarise(Yobs = crossprod(meanY, prop))
  
  
  #Estimate p(Y) given wrpmr
  cfLowEduc<-weighted.mean(as.numeric(unlist(data2[, c(Y)])), w = data2$w0rmpw, na.rm = T) 
  cfLowEduc
  
  #Obs disparity (Eq 1):
  obsDisp<-obsLowEduc - obsHiEduc
  obsDisp
  
  #Red disparity (Eq 2):
  redDisp<-obsLowEduc - cfLowEduc
  redDisp
  
  #Resid disparity (Eq 2):
  resDisp<-cfLowEduc - obsHiEduc
  resDisp
  
  
  new_row=c(Y, obsDisp, redDisp, resDisp)
  return(new_row)
  
}
#modeldomain(Y="W1_SENAS_EXEC_Z")
#modeldomain(Y="W1_SENAS_SEM_Z")
#modeldomain(Y="W1_SENAS_VRMEM_Z")
#modeldomain(Y="W1_SENAS_exec")
#modeldomain(Y="W1_SENAS_sem")
#modeldomain(Y="W1_SENAS_vrmem")

outcomelist<-c("W1_SENAS_EXEC_Z", "W1_SENAS_SEM_Z", "W1_SENAS_VRMEM_Z", 
               "W1_SENAS_exec", "W1_SENAS_sem", "W1_SENAS_vrmem")
for (j in 1:length(outcomelist)){
  Ytemp1<-outcomelist[j]
  
  resB<-modeldomain(Y=Ytemp1)
  if (j==1){totalresultsB<-resB} else totalresultsB<-rbind(totalresultsB,resB)
  
}

totalresultsC<-as.data.frame(totalresultsB)
names(totalresultsC)<-c("Outcome", "obsDisp", "redDisp", "resDisp")

#Update the name
write.csv(totalresultsC, ".../TAMresultsEFEXBW.csv") 

