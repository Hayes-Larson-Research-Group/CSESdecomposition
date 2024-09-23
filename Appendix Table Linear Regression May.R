library(haven)
library(tidyverse)
library(dplyr)
library(nnet)

#load and clean data

data <- read_sas(".../mhc_khstarbw.sas7bdat", 
                 NULL)
variable.names(data)
table(data$W1_D_RACE_SUMMARY)
data$ID<-as.character(data$STUDYID)
dataO<-data %>% filter(W1_INTERVIEW_AGE_PHI>=65)
data1<- dataO %>% filter(W1_D_RACE_SUMMARY=="Black" | W1_D_RACE_SUMMARY=="White")

data1$HiBP<-ifelse(data1$MHC_HYP==1, 1, 0)
data1$LowBP<-ifelse(data1$MHC_HYP==0, 1, 0)
data1$HiEduc<-ifelse(data1$PARENT_COL==1, 1, 0)
data1$LowEduc<-ifelse(data1$PARENT_COL==0, 1, 0)

data1$Female<-ifelse(data1$W1_D_GENDER==2, 1, 0)
data1$D_EDUC<-ifelse(data1$W1_D_EDUCATION==1 | data1$W1_D_EDUCATION==2 | data1$W1_D_EDUCATION==9 | data1$W1_D_EDUCATION==10 | data1$W1_D_EDUCATION==7, "LEQ Highschool Grad", 
                     ifelse(data1$W1_D_EDUCATION==3 | data1$W1_D_EDUCATION==4, "Tech/Trade or Some College", ifelse(data1$W1_D_EDUCATION==5 | data1$W1_D_EDUCATION==6, "College Grad or Grad School", "Not Sure")))


data1$MARITAL_STATUS_D<-ifelse(data1$W1_MARITAL_STATUS==1, "Married", 
                               ifelse(data1$W1_MARITAL_STATUS==6, "Unmarried couple",
                                      ifelse(data1$W1_MARITAL_STATUS==4, "Separated",
                                             ifelse(data1$W1_MARITAL_STATUS==2, "Divorced",
                                                    ifelse(data1$W1_MARITAL_STATUS==3, "Widowed",
                                                           ifelse(data1$W1_MARITAL_STATUS==5, "Never Married", "Missing"))))))

data1$MARITAL_STATUS_D1<-ifelse(data1$W1_MARITAL_STATUS==1 | data1$W1_MARITAL_STATUS==6, "Married or Couples",
                                ifelse(data1$W1_MARITAL_STATUS==4 | data1$W1_MARITAL_STATUS==2 | data1$W1_MARITAL_STATUS==3, "Separated, Divorced or Widowed",
                                       ifelse(data1$W1_MARITAL_STATUS==5, "Never Married", "Missing")))


EXEC.lm <- lm(W1_SENAS_EXEC_Z ~ HiBP+W1_INTERVIEW_AGE_PHI+Female+W1_D_RACE_SUMMARY, data = data1)

summary(EXEC.lm)
confint(EXEC.lm)

SEM.lm <- lm(W1_SENAS_SEM_Z ~ HiBP+W1_INTERVIEW_AGE_PHI+Female+W1_D_RACE_SUMMARY, data = data1)

summary(SEM.lm)
confint(SEM.lm)

VRMEM.lm <- lm(W1_SENAS_VRMEM_Z ~ HiBP+W1_INTERVIEW_AGE_PHI+Female+W1_D_RACE_SUMMARY, data = data1)

summary(VRMEM.lm)
confint(VRMEM.lm)



#Adding in CSES
EXEC.lm <- lm(W1_SENAS_EXEC_Z ~ HiBP+HiEduc+W1_INTERVIEW_AGE_PHI+Female+W1_D_RACE_SUMMARY, data = data1)

summary(EXEC.lm)
confint(EXEC.lm)

SEM.lm <- lm(W1_SENAS_SEM_Z ~ HiBP+HiEduc+W1_INTERVIEW_AGE_PHI+Female+W1_D_RACE_SUMMARY, data = data1)

summary(SEM.lm)
confint(SEM.lm)

VRMEM.lm <- lm(W1_SENAS_VRMEM_Z ~ HiBP+HiEduc+W1_INTERVIEW_AGE_PHI+Female+W1_D_RACE_SUMMARY, data = data1)

summary(VRMEM.lm)
confint(VRMEM.lm)


data1B<- data1 %>% filter(W1_D_RACE_SUMMARY=="Black" )
data1W<- data1 %>% filter(W1_D_RACE_SUMMARY=="White")


#Among Black Individuals 

EXEC.lmB <- lm(W1_SENAS_EXEC_Z ~ HiBP+W1_INTERVIEW_AGE_PHI+Female, data = data1B)

summary(EXEC.lmB)
confint(EXEC.lmB)

SEM.lmB <- lm(W1_SENAS_SEM_Z ~ HiBP+W1_INTERVIEW_AGE_PHI+Female, data = data1B)

summary(SEM.lmB)
confint(SEM.lmB)

VRMEM.lmB <- lm(W1_SENAS_VRMEM_Z ~ HiBP+W1_INTERVIEW_AGE_PHI+Female, data = data1B)

summary(VRMEM.lmB)
confint(VRMEM.lmB)


#Adding in CSES
EXEC.lmB <- lm(W1_SENAS_EXEC_Z ~ HiBP+HiEduc+W1_INTERVIEW_AGE_PHI+Female, data = data1B)

summary(EXEC.lmB)
confint(EXEC.lmB)

SEM.lmB <- lm(W1_SENAS_SEM_Z ~ HiBP+HiEduc+W1_INTERVIEW_AGE_PHI+Female, data = data1B)

summary(SEM.lmB)
confint(SEM.lmB)

VRMEM.lmB <- lm(W1_SENAS_VRMEM_Z ~ HiBP+HiEduc+W1_INTERVIEW_AGE_PHI+Female, data = data1B)

summary(VRMEM.lmB)
confint(VRMEM.lmB)




#Among White individuals 

EXEC.lmW <- lm(W1_SENAS_EXEC_Z ~ HiBP+W1_INTERVIEW_AGE_PHI+Female, data = data1W)

summary(EXEC.lmW)
confint(EXEC.lmW)

SEM.lmW <- lm(W1_SENAS_SEM_Z ~ HiBP+W1_INTERVIEW_AGE_PHI+Female, data = data1W)

summary(SEM.lmW)
confint(SEM.lmW)

VRMEM.lmW <- lm(W1_SENAS_VRMEM_Z ~ HiBP+W1_INTERVIEW_AGE_PHI+Female, data = data1W)

summary(VRMEM.lmW)
confint(VRMEM.lmW)



#Adding in CSES
EXEC.lmW <- lm(W1_SENAS_EXEC_Z ~ HiBP+HiEduc+W1_INTERVIEW_AGE_PHI+Female, data = data1W)

summary(EXEC.lmW)
confint(EXEC.lmW)

SEM.lmW <- lm(W1_SENAS_SEM_Z ~ HiBP+HiEduc+W1_INTERVIEW_AGE_PHI+Female, data = data1W)

summary(SEM.lmW)
confint(SEM.lmW)

VRMEM.lmW <- lm(W1_SENAS_VRMEM_Z ~ HiBP+HiEduc+W1_INTERVIEW_AGE_PHI+Female, data = data1W)

summary(VRMEM.lmW)
confint(VRMEM.lmW)


#Logistic Regression of Hi BP
HiBP.GLM <-glm(HiBP ~ HiEduc+W1_INTERVIEW_AGE_PHI+Female+W1_D_RACE_SUMMARY, family=binomial(link="logit"), data=data1)
summary(HiBP.GLM)
confint(HiBP.GLM)
#Among Black
HiBP.GLMB <-glm(HiBP ~ HiEduc+W1_INTERVIEW_AGE_PHI+Female, family=binomial(link="logit"), data=data1B)
summary(HiBP.GLMB)
confint(HiBP.GLMB)

#Among White
HiBP.GLMW <-glm(HiBP ~ HiEduc+W1_INTERVIEW_AGE_PHI+Female, family=binomial(link="logit"), data=data1W)
summary(HiBP.GLMW)
confint(HiBP.GLMW)