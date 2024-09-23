
BWCONFINT <- read.csv(".../TAMobservedCONFINTFeb6BW.csv")
BWCONFINT$X<-NULL
BWCONFINT$CAT<-"Total"
BWCONFINTLOWEDUC<-BWCONFINT %>% mutate(., Educ="Low", OBS_lb=obsLowEduc_lb, OBS_ub=obsLowEduc_ub, obsLowEduc_lb=NULL, obsLowEduc_ub=NULL, obsHiEduc_lb=NULL, obsHiEduc_ub=NULL, cfLowEduc_lb=NULL, cfLowEduc_ub=NULL)
BWCONFINTHIEDUC<-BWCONFINT %>% mutate(., Educ="High", OBS_lb=obsHiEduc_lb, OBS_ub=obsHiEduc_ub, obsLowEduc_lb=NULL, obsLowEduc_ub=NULL, obsHiEduc_lb=NULL, obsHiEduc_ub=NULL, cfLowEduc_lb=NULL, cfLowEduc_ub=NULL)

BCONFINT <- read.csv(".../TAMobservedCONFINTFeb6B.csv")
BCONFINT$X<-NULL
BCONFINT$CAT<-"Black"
BCONFINTLOWEDUC<-BCONFINT %>% mutate(., Educ="Low", OBS_lb=obsLowEduc_lb, OBS_ub=obsLowEduc_ub, obsLowEduc_lb=NULL, obsLowEduc_ub=NULL, obsHiEduc_lb=NULL, obsHiEduc_ub=NULL, cfLowEduc_lb=NULL, cfLowEduc_ub=NULL)
BCONFINTHIEDUC<-BCONFINT %>% mutate(., Educ="High", OBS_lb=obsHiEduc_lb, OBS_ub=obsHiEduc_ub, obsLowEduc_lb=NULL, obsLowEduc_ub=NULL, obsHiEduc_lb=NULL, obsHiEduc_ub=NULL, cfLowEduc_lb=NULL, cfLowEduc_ub=NULL)

BOBS <- read.csv(".../TAMObsEFJan29B.csv")
BOBS$X<-NULL
BOBS$CAT<-"Black"
BOBS<-filter(BOBS, Outcome=="W1_SENAS_EXEC_Z" | Outcome=="W1_SENAS_SEM_Z" | Outcome=="W1_SENAS_VRMEM_Z")
BOBSLOWEDUC<-BOBS %>% mutate(., Educ="Low", OBS=obsLowEduc,  obsLowEduc=NULL, obsHiEduc=NULL, cfLowEduc=NULL)
BOBSHIEDUC<-BOBS %>% mutate(., Educ="High", OBS=obsHiEduc, obsLowEduc=NULL, obsHiEduc=NULL, cfLowEduc=NULL)


WCONFINT <- read.csv(".../TAMobservedCONFINTJan29W.csv")
WCONFINT$X<-NULL
WCONFINT$CAT<-"White"
WCONFINTLOWEDUC<-WCONFINT %>% mutate(., Educ="Low", OBS_lb=obsLowEduc_lb, OBS_ub=obsLowEduc_ub, obsLowEduc_lb=NULL, obsLowEduc_ub=NULL, obsHiEduc_lb=NULL, obsHiEduc_ub=NULL, cfLowEduc_lb=NULL, cfLowEduc_ub=NULL)
WCONFINTHIEDUC<-WCONFINT %>% mutate(., Educ="High", OBS_lb=obsHiEduc_lb, OBS_ub=obsHiEduc_ub, obsLowEduc_lb=NULL, obsLowEduc_ub=NULL, obsHiEduc_lb=NULL, obsHiEduc_ub=NULL, cfLowEduc_lb=NULL, cfLowEduc_ub=NULL)

WOBS <- read.csv(".../TAMObsEFJAN29W.csv")
WOBS$X<-NULL
WOBS$CAT<-"White"
WOBS<-filter(WOBS, Outcome=="W1_SENAS_EXEC_Z" | Outcome=="W1_SENAS_SEM_Z" | Outcome=="W1_SENAS_VRMEM_Z")
WOBSLOWEDUC<-WOBS %>% mutate(., Educ="Low", OBS=obsLowEduc,  obsLowEduc=NULL, obsHiEduc=NULL, cfLowEduc=NULL)
WOBSHIEDUC<-WOBS %>% mutate(., Educ="High", OBS=obsHiEduc, obsLowEduc=NULL, obsHiEduc=NULL, cfLowEduc=NULL)


BWOBS <- read.csv(".../TAMObsEFJan30BW.csv")
BWOBS$X<-NULL
BWOBS$CAT="Total"
BWOBS<-filter(BWOBS, Outcome=="W1_SENAS_EXEC_Z" | Outcome=="W1_SENAS_SEM_Z" | Outcome=="W1_SENAS_VRMEM_Z")
BWOBSLOWEDUC<-BWOBS %>% mutate(., Educ="Low", OBS=obsLowEduc,  obsLowEduc=NULL, obsHiEduc=NULL, cfLowEduc=NULL)
BWOBSHIEDUC<-BWOBS %>% mutate(., Educ="High", OBS=obsHiEduc, obsLowEduc=NULL, obsHiEduc=NULL, cfLowEduc=NULL)


BlackLOWEDUC<-left_join(BOBSLOWEDUC, BCONFINTLOWEDUC, by = join_by(Outcome == Outcome, CAT==CAT, Educ==Educ))
BlackHIEDUC<-left_join(BOBSHIEDUC, BCONFINTHIEDUC, by = join_by(Outcome == Outcome, CAT==CAT, Educ==Educ))
WhiteLOWEDUC<-left_join(WOBSLOWEDUC, WCONFINTLOWEDUC, by = join_by(Outcome == Outcome, CAT==CAT, Educ==Educ))
WhiteHIEDUC<-left_join(WOBSHIEDUC, WCONFINTHIEDUC, by = join_by(Outcome == Outcome, CAT==CAT, Educ==Educ))
TotalLOWEDUC<-left_join(BWOBSLOWEDUC, BWCONFINTLOWEDUC, by = join_by(Outcome == Outcome, CAT==CAT, Educ==Educ))
TotalHIEDUC<-left_join(BWOBSHIEDUC, BWCONFINTHIEDUC, by = join_by(Outcome == Outcome, CAT==CAT, Educ==Educ))

TOTAL<-rbind(BlackLOWEDUC, BlackHIEDUC, WhiteLOWEDUC, WhiteHIEDUC, TotalLOWEDUC, TotalHIEDUC)
TOTAL$Domain <- ifelse (TOTAL$Outcome=="W1_SENAS_EXEC_Z", "Executive function",
                      ifelse (TOTAL$Outcome=="W1_SENAS_SEM_Z", "Semantic memory",
                              ifelse (TOTAL$Outcome=="W1_SENAS_VRMEM_Z", "Verbal memory", NA)))
TOTAL$CAT<-ifelse(TOTAL$CAT=="Total", "Overall", TOTAL$CAT)
TOTAL$CAT<-factor(TOTAL$CAT, levels=c("Overall", "Black", "White"))
TOTAL$Domain <- factor(TOTAL$Domain, levels=c('Semantic memory', 'Executive function', 'Verbal memory'))
TOTAL$Educ<-factor(TOTAL$Educ, levels=c("Low", "High"))
TOTAL
test<-ggplot(data=TOTAL)+
  geom_pointrange(aes(x=Domain, y=OBS, ymin=OBS_lb, ymax=OBS_ub, color=Educ), 
                  position = position_dodge(width=.5)) + 
  geom_hline(yintercept=0, color="black") +

  scale_color_manual(
                     values=c("lightblue", "navy"))+
  facet_grid( cols = vars(CAT), scales="fixed") + 
  ggtitle("") +xlab("Cognitive Domain") + ylab("Cognitive Domain Z Score")+
  guides(color=guide_legend(title="Childhood SES"))+theme_bw()+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
test