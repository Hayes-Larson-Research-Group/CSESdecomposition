library(haven)
library(tidyverse)
library(dplyr)
library(nnet)
library(survey)
library(gtsummary)
library(gt)
#load and clean data

data <- read_sas(".../mhc_khstarbw.sas7bdat", 
                 NULL)
variable.names(data)
table(data$W1_D_RACE_SUMMARY)
data$ID<-as.character(data$STUDYID)
dataO<-data %>% filter(W1_INTERVIEW_AGE_PHI>=65)
data1<- dataO %>% filter(W1_D_RACE_SUMMARY=="Black" | W1_D_RACE_SUMMARY=="White")
table(data1$W1_D_RACE_SUMMARY)


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



data1$PARENT_COL_RACE<-ifelse(data1$PARENT_COL==1 & data1$W1_D_RACE_SUMMARY=="Black", "High Parental Education and Black", ifelse(data1$PARENT_COL==1 & data1$W1_D_RACE_SUMMARY=="White", "High Parental Education and White", ifelse(data1$PARENT_COL==0 & data1$W1_D_RACE_SUMMARY=="Black", "Low Parental Education and Black", ifelse(data1$PARENT_COL==0 & data1$W1_D_RACE_SUMMARY=="White", "Low Parental Education and White", NA))))
data1 <- data1 %>%
  labelled::set_variable_labels( PARENT_COL="Parental Education", W1_D_RACE_SUMMARY="Race", W1_INTERVIEW_AGE_PHI="Interview Age", D_EDUC="Education Category ", MARITAL_STATUS_D1="Marital Status", W1_D_GENDER="Sex",
                                 MHC_HYP="Midlife Hypertension", PARENT_COL_RACE="Parental Education and Race")




svy.d <- data1 %>% 
  select(
    PARENT_COL, W1_D_RACE_SUMMARY, W1_INTERVIEW_AGE_PHI, D_EDUC, MARITAL_STATUS_D1, W1_D_GENDER,
    MHC_HYP) %>% 
  svydesign(~ 1, data = .)

chis_combined <- tbl_svysummary(
  data = svy.d, 
  by = PARENT_COL, 
  statistic = list(W1_D_RACE_SUMMARY ~ "{n_unweighted} ({p})", 
                   W1_INTERVIEW_AGE_PHI ~ "{mean} ({sd})", 
                   W1_D_GENDER ~ "{n_unweighted} ({p})",
                   D_EDUC ~ "{n_unweighted} ({p})",
                   MARITAL_STATUS_D1 ~ "{n_unweighted} ({p})",
                   MHC_HYP ~ "{n_unweighted} ({p})"
                   
  ), 
  missing_text = "Missing", 
  digits = list(W1_D_RACE_SUMMARY~0, W1_INTERVIEW_AGE_PHI ~ 0, W1_D_GENDER ~ 0, D_EDUC ~ 0, MARITAL_STATUS_D1~0, MHC_HYP~0)
) %>% add_overall()



gtsave(chis_combined %>% as_gt, file="/bd-fs-mnt/sasg_project/adhoc/dor/rsch/Dementia/projects/AH/Tamare Mediation/TAMTAB1FEB27.html")





svy.d1 <- data1 %>% 
  select(
    PARENT_COL_RACE,  W1_INTERVIEW_AGE_PHI, D_EDUC, MARITAL_STATUS_D1, W1_D_GENDER,
    MHC_HYP) %>% 
  svydesign(~ 1, data = .)

chis_combined <- tbl_svysummary(
  data = svy.d1, 
  by = PARENT_COL_RACE, 
  statistic = list( W1_INTERVIEW_AGE_PHI ~ "{mean} ({sd})", 
                   W1_D_GENDER ~ "{n_unweighted} ({p})",
                   D_EDUC ~ "{n_unweighted} ({p})",
                   MARITAL_STATUS_D1 ~ "{n_unweighted} ({p})",
                   MHC_HYP ~ "{n_unweighted} ({p})"
                   
  ), 
  missing_text = "Missing", 
  digits = list(W1_INTERVIEW_AGE_PHI ~ 0, W1_D_GENDER ~ 0, D_EDUC ~ 0, MARITAL_STATUS_D1~0, MHC_HYP~0)
) %>% add_overall()



gtsave(chis_combined %>% as_gt, file=".../TAMSUPTAB1FEB27.html")
