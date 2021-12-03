

plate23<-myInputData("023")%>%
  filter(VISITNUM==0)%>%
  mutate(TreatStartDate=as.Date(VISITDAT,format="%d/%B/%Y"))%>%
  select(SUBJID,TreatStartDate)


dta<-myInputData("040")%>%
  select(SUBJID,VISITNUM,VISITDAT,EXDose_Final)%>%
  inner_join(INREPORT, by="SUBJID")%>%
  left_join(plate23,by="SUBJID")%>%
  mutate(VISITDAT=as.Date(VISITDAT,format="%d/%B/%Y"))%>%
  filter(!is.na(EXDose_Final) & EXDose_Final!="00" & !EXDose_Final %in% c(".NA",".ND",".UNK")
         & EXDose_Final!="")%>%
  select(-Arm)%>%
  filter(SUBJID!=15112)  ### 15112 doesn't have baseline plate23 submitted. 



###################
#
#    Week4
###################

HaveWK4<-unique(dta$SUBJID[dta$VISITNUM==40])

Week4<-dta%>%
  filter(VISITNUM<=40 & SUBJID %in% HaveWK4)%>%
  mutate(Denominator=as.numeric(VISITDAT-TreatStartDate)+1)%>%
  group_by(SUBJID)%>%
  summarise(Numerator=sum(as.numeric(EXDose_Final)),Denominator=last(Denominator),BASE_YN=last(BASE_YN),WEEK4_YN=last(WEEK4_YN),
            CCORRES_Week16MinDose=last(CCORRES_Week16MinDose),CCORRES_Week16=last(CCORRES_Week16), RNDMCODE=last(RNDMCODE))%>%
  mutate(Arm=ifelse(!is.na(RNDMCODE)&RNDMCODE!="","B/C",
                    ifelse((!is.na(BASE_YN)&BASE_YN=="No"&is.na(WEEK4_YN)) | (!is.na(WEEK4_YN)&WEEK4_YN=="No"&is.na(CCORRES_Week16MinDose)) | 
                             (!is.na(CCORRES_Week16MinDose)&CCORRES_Week16MinDose=="No" | !is.na(CCORRES_Week16)&CCORRES_Week16=="No"),"A","Pending")),
         PCT=Numerator/Denominator,
         PCT_group=ifelse(is.na(PCT),NA,
                          ifelse(PCT<0.9,"<90%",
                                 ifelse(PCT>=0.9&PCT<0.95,"90% - <95%",
                                        ifelse(PCT>=0.95&PCT<1,"95% - <100%","100%")))))%>%
  mutate(Arm=ifelse(!is.na(Arm)& Arm=="B/C" & RNDMCODE=="B   Continue treatment until week 24","Arm B",
                    ifelse(!is.na(Arm)& Arm=="B/C" & RNDMCODE=="C   Treatment stop at week 16","Arm C",Arm)),
         Arm=ifelse(SUBJID %in% c(14067, 14130, 15075,13039), "A", Arm))



###################
#
#    Week8
###################

HaveWK8<-unique(dta$SUBJID[dta$VISITNUM==80])

Week8<-dta%>%
  filter(VISITNUM<=80 & SUBJID %in% HaveWK8)%>%
  mutate(Denominator=as.numeric(VISITDAT-TreatStartDate)+1)%>%
  group_by(SUBJID)%>%
  summarise(Numerator=sum(as.numeric(EXDose_Final)),Denominator=last(Denominator),BASE_YN=last(BASE_YN),WEEK4_YN=last(WEEK4_YN),
            CCORRES_Week16MinDose=last(CCORRES_Week16MinDose),CCORRES_Week16=last(CCORRES_Week16), RNDMCODE=last(RNDMCODE))%>%
  mutate(Arm=ifelse(!is.na(RNDMCODE)&RNDMCODE!="","B/C",
                    ifelse((!is.na(BASE_YN)&BASE_YN=="No"&is.na(WEEK4_YN)) | (!is.na(WEEK4_YN)&WEEK4_YN=="No"&is.na(CCORRES_Week16MinDose)) | 
                             (!is.na(CCORRES_Week16MinDose)&CCORRES_Week16MinDose=="No" | !is.na(CCORRES_Week16)&CCORRES_Week16=="No"),"A","Pending")),
         PCT=Numerator/Denominator,
         PCT_group=ifelse(is.na(PCT),NA,
                          ifelse(PCT<0.9,"<90%",
                                 ifelse(PCT>=0.9&PCT<0.95,"90% - <95%",
                                        ifelse(PCT>=0.95&PCT<1,"95% - <100%","100%")))))%>%
  mutate(Arm=ifelse(!is.na(Arm)& Arm=="B/C" & RNDMCODE=="B   Continue treatment until week 24","Arm B",
                    ifelse(!is.na(Arm)& Arm=="B/C" & RNDMCODE=="C   Treatment stop at week 16","Arm C",Arm)),
         Arm=ifelse(SUBJID %in% c(14067,14130, 15075,13039), "A", Arm))



###################
#
#    Week12
###################

HaveWK12<-unique(dta$SUBJID[dta$VISITNUM==120])

Week12<-dta%>%
  filter(VISITNUM<=120 & SUBJID %in% HaveWK12)%>%
  mutate(Denominator=as.numeric(VISITDAT-TreatStartDate)+1)%>%
  group_by(SUBJID)%>%
  summarise(Numerator=sum(as.numeric(EXDose_Final)),Denominator=last(Denominator),BASE_YN=last(BASE_YN),WEEK4_YN=last(WEEK4_YN),
            CCORRES_Week16MinDose=last(CCORRES_Week16MinDose),CCORRES_Week16=last(CCORRES_Week16), RNDMCODE=last(RNDMCODE))%>%
  mutate(Arm=ifelse(!is.na(RNDMCODE)&RNDMCODE!="","B/C",
                    ifelse((!is.na(BASE_YN)&BASE_YN=="No"&is.na(WEEK4_YN)) | (!is.na(WEEK4_YN)&WEEK4_YN=="No"&is.na(CCORRES_Week16MinDose)) | 
                             (!is.na(CCORRES_Week16MinDose)&CCORRES_Week16MinDose=="No" | !is.na(CCORRES_Week16)&CCORRES_Week16=="No"),"A","Pending")),
         PCT=Numerator/Denominator,
         PCT_group=ifelse(is.na(PCT),NA,
                          ifelse(PCT<0.9,"<90%",
                                 ifelse(PCT>=0.9&PCT<0.95,"90% - <95%",
                                        ifelse(PCT>=0.95&PCT<1,"95% - <100%","100%")))))%>%
  mutate(Arm=ifelse(!is.na(Arm)& Arm=="B/C" & RNDMCODE=="B   Continue treatment until week 24","Arm B",
                    ifelse(!is.na(Arm)& Arm=="B/C" & RNDMCODE=="C   Treatment stop at week 16","Arm C",Arm)),
         Arm=ifelse(SUBJID %in% c(14067,14130, 15075,13039), "A", Arm))

###################
#
#    Week16
###################

HaveWK16<-unique(dta$SUBJID[dta$VISITNUM==160]) 

Week16<-dta%>%
  filter(VISITNUM<=160 & SUBJID %in% HaveWK16)%>%
  mutate(Denominator=as.numeric(VISITDAT-TreatStartDate)+1)%>%
  group_by(SUBJID)%>%
  summarise(Numerator=sum(as.numeric(EXDose_Final)),Denominator=last(Denominator),BASE_YN=last(BASE_YN),WEEK4_YN=last(WEEK4_YN),
            CCORRES_Week16MinDose=last(CCORRES_Week16MinDose),CCORRES_Week16=last(CCORRES_Week16),RNDMCODE=last(RNDMCODE))%>%
  mutate(Arm=ifelse(!is.na(BASE_YN)&BASE_YN=="Yes"&!is.na(WEEK4_YN)&WEEK4_YN=="Yes"&
                      (!is.na(CCORRES_Week16MinDose)&CCORRES_Week16MinDose=="Yes" | is.na(CCORRES_Week16MinDose))&
                      (!is.na(CCORRES_Week16)&CCORRES_Week16=="Yes" | is.na(CCORRES_Week16)),"B/C","A"),
         PCT=Numerator/Denominator,
         PCT_group=ifelse(is.na(PCT),NA,
                          ifelse(PCT<0.9,"<90%",
                                 ifelse(PCT>=0.9&PCT<0.95,"90% - <95%",
                                        ifelse(PCT>=0.95&PCT<1,"95% - <100%","100%")))))%>%
  mutate(Arm=ifelse(!is.na(Arm)& Arm=="B/C" & RNDMCODE=="B   Continue treatment until week 24","Arm B",
                    ifelse(!is.na(Arm)& Arm=="B/C" & RNDMCODE=="C   Treatment stop at week 16","Arm C",Arm)),
         Arm=ifelse(SUBJID %in% c(14067, 14130, 15075,13039), "A", Arm))%>%
  filter(!is.na(Arm))


###################
#
#    Week24
###################

HaveWK24<-unique(dta$SUBJID[dta$VISITNUM==240])

Week24<-dta%>%
  filter(VISITNUM<=240 & SUBJID %in% HaveWK24)%>%
  mutate(Denominator=as.numeric(VISITDAT-TreatStartDate)+1)%>%
  group_by(SUBJID)%>%
  summarise(Numerator=sum(as.numeric(EXDose_Final)),Denominator=last(Denominator),BASE_YN=last(BASE_YN),WEEK4_YN=last(WEEK4_YN),
            CCORRES_Week16MinDose=last(CCORRES_Week16MinDose),CCORRES_Week16=last(CCORRES_Week16),RNDMCODE=last(RNDMCODE))%>%
  mutate(Arm=ifelse(!is.na(BASE_YN)&BASE_YN=="Yes"&!is.na(WEEK4_YN)&WEEK4_YN=="Yes"&
                      !is.na(CCORRES_Week16MinDose)&CCORRES_Week16MinDose=="Yes"&
                      !is.na(CCORRES_Week16)&CCORRES_Week16=="Yes","B/C","A"),
         PCT=Numerator/Denominator,
         PCT_group=ifelse(is.na(PCT),NA,
                          ifelse(PCT<0.9,"<90%",
                                 ifelse(PCT>=0.9&PCT<0.95,"90% - <95%",
                                        ifelse(PCT>=0.95&PCT<1,"95% - <100%","100%")))))%>%
  filter(!is.na(PCT_group))%>%
  filter(is.na(RNDMCODE) | RNDMCODE==" " | (!is.na(RNDMCODE) & RNDMCODE!="C   Treatment stop at week 16"))%>%
  mutate(Arm=ifelse(!is.na(Arm)& Arm=="B/C" & RNDMCODE=="B   Continue treatment until week 24","Arm B",
                    ifelse(!is.na(Arm)& Arm=="B/C" & RNDMCODE=="C   Treatment stop at week 16","Arm C",Arm)),
         Arm=ifelse(SUBJID %in% c(14067, 14130, 15075,13039), "A", Arm))




#####################
#
#   Table8
#####################

ArmA<-rbind(paste0(sum(Week4$Arm=="A" & Week4$PCT_group=="<90%")," (", formatC(round(sum(Week4$Arm=="A" & Week4$PCT_group=="<90%")*100/(sum(Week4$Arm=="A")),1),digits=2,format='f'),"%)"),
            paste0(sum(Week4$Arm=="A" & Week4$PCT_group=="90% - <95%")," (", formatC(round(sum(Week4$Arm=="A" & Week4$PCT_group=="90% - <95%")*100/(sum(Week4$Arm=="A")),1),digits=2,format='f'),"%)"),
            paste0(sum(Week4$Arm=="A" & Week4$PCT_group=="95% - <100%")," (", formatC(round(sum(Week4$Arm=="A" & Week4$PCT_group=="95% - <100%")*100/(sum(Week4$Arm=="A")),1),digits=2,format='f'),"%)"),
            paste0(sum(Week4$Arm=="A" & Week4$PCT_group=="100%")," (", formatC(round(sum(Week4$Arm=="A" & Week4$PCT_group=="100%")*100/(sum(Week4$Arm=="A")),1),digits=2,format='f'),"%)"),
            paste0(sum(Week8$Arm=="A" & Week8$PCT_group=="<90%")," (", formatC(round(sum(Week8$Arm=="A" & Week8$PCT_group=="<90%")*100/(sum(Week8$Arm=="A")),1),digits=2,format='f'),"%)"),
            paste0(sum(Week8$Arm=="A" & Week8$PCT_group=="90% - <95%")," (", formatC(round(sum(Week8$Arm=="A" & Week8$PCT_group=="90% - <95%")*100/(sum(Week8$Arm=="A")),1),digits=2,format='f'),"%)"),
            paste0(sum(Week8$Arm=="A" & Week8$PCT_group=="95% - <100%")," (", formatC(round(sum(Week8$Arm=="A" & Week8$PCT_group=="95% - <100%")*100/(sum(Week8$Arm=="A")),1),digits=2,format='f'),"%)"),
            paste0(sum(Week8$Arm=="A" & Week8$PCT_group=="100%")," (", formatC(round(sum(Week8$Arm=="A" & Week8$PCT_group=="100%")*100/(sum(Week8$Arm=="A")),1),digits=2,format='f'),"%)"),
            paste0(sum(Week12$Arm=="A" & Week12$PCT_group=="<90%")," (", formatC(round(sum(Week12$Arm=="A" & Week12$PCT_group=="<90%")*100/(sum(Week12$Arm=="A")),1),digits=2,format='f'),"%)"),
            paste0(sum(Week12$Arm=="A" & Week12$PCT_group=="90% - <95%")," (", formatC(round(sum(Week12$Arm=="A" & Week12$PCT_group=="90% - <95%")*100/(sum(Week12$Arm=="A")),1),digits=2,format='f'),"%)"),
            paste0(sum(Week12$Arm=="A" & Week12$PCT_group=="95% - <100%")," (", formatC(round(sum(Week12$Arm=="A" & Week12$PCT_group=="95% - <100%")*100/(sum(Week12$Arm=="A")),1),digits=2,format='f'),"%)"),
            paste0(sum(Week12$Arm=="A" & Week12$PCT_group=="100%")," (", formatC(round(sum(Week12$Arm=="A" & Week12$PCT_group=="100%")*100/(sum(Week12$Arm=="A")),1),digits=2,format='f'),"%)"),
            paste0(sum(Week16$Arm=="A" & Week16$PCT_group=="<90%")," (", formatC(round(sum(Week16$Arm=="A" & Week16$PCT_group=="<90%")*100/(sum(Week16$Arm=="A")),1),digits=2,format='f'),"%)"),
            paste0(sum(Week16$Arm=="A" & Week16$PCT_group=="90% - <95%")," (", formatC(round(sum(Week16$Arm=="A" & Week16$PCT_group=="90% - <95%")*100/(sum(Week16$Arm=="A")),1),digits=2,format='f'),"%)"),
            paste0(sum(Week16$Arm=="A" & Week16$PCT_group=="95% - <100%")," (", formatC(round(sum(Week16$Arm=="A" & Week16$PCT_group=="95% - <100%")*100/(sum(Week16$Arm=="A")),1),digits=2,format='f'),"%)"),
            paste0(sum(Week16$Arm=="A" & Week16$PCT_group=="100%")," (", formatC(round(sum(Week16$Arm=="A" & Week16$PCT_group=="100%")*100/(sum(Week16$Arm=="A")),1),digits=2,format='f'),"%)"),
            paste0(sum(Week24$Arm=="A" & Week24$PCT_group=="<90%")," (", formatC(round(sum(Week24$Arm=="A" & Week24$PCT_group=="<90%")*100/(sum(Week24$Arm=="A")),1),digits=2,format='f'),"%)"),
            paste0(sum(Week24$Arm=="A" & Week24$PCT_group=="90% - <95%")," (", formatC(round(sum(Week24$Arm=="A" & Week24$PCT_group=="90% - <95%")*100/(sum(Week24$Arm=="A")),1),digits=2,format='f'),"%)"),
            paste0(sum(Week24$Arm=="A" & Week24$PCT_group=="95% - <100%")," (", formatC(round(sum(Week24$Arm=="A" & Week24$PCT_group=="95% - <100%")*100/(sum(Week24$Arm=="A")),1),digits=2,format='f'),"%)"),
            paste0(sum(Week24$Arm=="A" & Week24$PCT_group=="100%")," (", formatC(round(sum(Week24$Arm=="A" & Week24$PCT_group=="100%")*100/(sum(Week24$Arm=="A")),1),digits=2,format='f'),"%)"))




ArmBC<-rbind(paste0(sum(Week4$Arm%in% c("Arm B", "Arm C") & Week4$PCT_group=="<90%")," (", formatC(round(sum(Week4$Arm%in% c("Arm B", "Arm C") & Week4$PCT_group=="<90%")*100/(sum(Week4$Arm%in% c("Arm B", "Arm C"))),1),digits=2,format='f'),"%)"),
             paste0(sum(Week4$Arm%in% c("Arm B", "Arm C") & Week4$PCT_group=="90% - <95%")," (", formatC(round(sum(Week4$Arm%in% c("Arm B", "Arm C") & Week4$PCT_group=="90% - <95%")*100/(sum(Week4$Arm%in% c("Arm B", "Arm C"))),1),digits=2,format='f'),"%)"),
             paste0(sum(Week4$Arm%in% c("Arm B", "Arm C") & Week4$PCT_group=="95% - <100%")," (", formatC(round(sum(Week4$Arm%in% c("Arm B", "Arm C") & Week4$PCT_group=="95% - <100%")*100/(sum(Week4$Arm%in% c("Arm B", "Arm C"))),1),digits=2,format='f'),"%)"),
             paste0(sum(Week4$Arm%in% c("Arm B", "Arm C") & Week4$PCT_group=="100%")," (", formatC(round(sum(Week4$Arm%in% c("Arm B", "Arm C") & Week4$PCT_group=="100%")*100/(sum(Week4$Arm%in% c("Arm B", "Arm C"))),1),digits=2,format='f'),"%)"),
             paste0(sum(Week8$Arm%in% c("Arm B", "Arm C") & Week8$PCT_group=="<90%")," (", formatC(round(sum(Week8$Arm%in% c("Arm B", "Arm C") & Week8$PCT_group=="<90%")*100/(sum(Week8$Arm%in% c("Arm B", "Arm C"))),1),digits=2,format='f'),"%)"),
             paste0(sum(Week8$Arm%in% c("Arm B", "Arm C") & Week8$PCT_group=="90% - <95%")," (", formatC(round(sum(Week8$Arm%in% c("Arm B", "Arm C") & Week8$PCT_group=="90% - <95%")*100/(sum(Week8$Arm%in% c("Arm B", "Arm C"))),1),digits=2,format='f'),"%)"),
             paste0(sum(Week8$Arm%in% c("Arm B", "Arm C") & Week8$PCT_group=="95% - <100%")," (", formatC(round(sum(Week8$Arm%in% c("Arm B", "Arm C") & Week8$PCT_group=="95% - <100%")*100/(sum(Week8$Arm%in% c("Arm B", "Arm C"))),1),digits=2,format='f'),"%)"),
             paste0(sum(Week8$Arm%in% c("Arm B", "Arm C") & Week8$PCT_group=="100%")," (", formatC(round(sum(Week8$Arm%in% c("Arm B", "Arm C") & Week8$PCT_group=="100%")*100/(sum(Week8$Arm%in% c("Arm B", "Arm C"))),1),digits=2,format='f'),"%)"),
             paste0(sum(Week12$Arm%in% c("Arm B", "Arm C") & Week12$PCT_group=="<90%")," (", formatC(round(sum(Week12$Arm%in% c("Arm B", "Arm C") & Week12$PCT_group=="<90%")*100/(sum(Week12$Arm%in% c("Arm B", "Arm C"))),1),digits=2,format='f'),"%)"),
             paste0(sum(Week12$Arm%in% c("Arm B", "Arm C") & Week12$PCT_group=="90% - <95%")," (", formatC(round(sum(Week12$Arm%in% c("Arm B", "Arm C") & Week12$PCT_group=="90% - <95%")*100/(sum(Week12$Arm%in% c("Arm B", "Arm C"))),1),digits=2,format='f'),"%)"),
             paste0(sum(Week12$Arm%in% c("Arm B", "Arm C") & Week12$PCT_group=="95% - <100%")," (", formatC(round(sum(Week12$Arm%in% c("Arm B", "Arm C") & Week12$PCT_group=="95% - <100%")*100/(sum(Week12$Arm%in% c("Arm B", "Arm C"))),1),digits=2,format='f'),"%)"),
             paste0(sum(Week12$Arm%in% c("Arm B", "Arm C") & Week12$PCT_group=="100%")," (", formatC(round(sum(Week12$Arm%in% c("Arm B", "Arm C") & Week12$PCT_group=="100%")*100/(sum(Week12$Arm%in% c("Arm B", "Arm C"))),1),digits=2,format='f'),"%)"),
             paste0(sum(Week16$Arm%in% c("Arm B", "Arm C") & Week16$PCT_group=="<90%")," (", formatC(round(sum(Week16$Arm%in% c("Arm B", "Arm C") & Week16$PCT_group=="<90%")*100/(sum(Week16$Arm%in% c("Arm B", "Arm C"))),1),digits=2,format='f'),"%)"),
             paste0(sum(Week16$Arm%in% c("Arm B", "Arm C") & Week16$PCT_group=="90% - <95%")," (", formatC(round(sum(Week16$Arm%in% c("Arm B", "Arm C") & Week16$PCT_group=="90% - <95%")*100/(sum(Week16$Arm%in% c("Arm B", "Arm C"))),1),digits=2,format='f'),"%)"),
             paste0(sum(Week16$Arm%in% c("Arm B", "Arm C") & Week16$PCT_group=="95% - <100%")," (", formatC(round(sum(Week16$Arm%in% c("Arm B", "Arm C") & Week16$PCT_group=="95% - <100%")*100/(sum(Week16$Arm%in% c("Arm B", "Arm C"))),1),digits=2,format='f'),"%)"),
             paste0(sum(Week16$Arm%in% c("Arm B", "Arm C") & Week16$PCT_group=="100%")," (", formatC(round(sum(Week16$Arm%in% c("Arm B", "Arm C") & Week16$PCT_group=="100%")*100/(sum(Week16$Arm%in% c("Arm B", "Arm C"))),1),digits=2,format='f'),"%)"),
             paste0(sum(Week24$Arm%in% c("Arm B", "Arm C") & Week24$PCT_group=="<90%")," (", formatC(round(sum(Week24$Arm%in% c("Arm B", "Arm C") & Week24$PCT_group=="<90%")*100/(sum(Week24$Arm%in% c("Arm B", "Arm C"))),1),digits=2,format='f'),"%)"),
             paste0(sum(Week24$Arm%in% c("Arm B", "Arm C") & Week24$PCT_group=="90% - <95%")," (", formatC(round(sum(Week24$Arm%in% c("Arm B", "Arm C") & Week24$PCT_group=="90% - <95%")*100/(sum(Week24$Arm%in% c("Arm B", "Arm C"))),1),digits=2,format='f'),"%)"),
             paste0(sum(Week24$Arm%in% c("Arm B", "Arm C") & Week24$PCT_group=="95% - <100%")," (", formatC(round(sum(Week24$Arm%in% c("Arm B", "Arm C") & Week24$PCT_group=="95% - <100%")*100/(sum(Week24$Arm%in% c("Arm B", "Arm C"))),1),digits=2,format='f'),"%)"),
             paste0(sum(Week24$Arm%in% c("Arm B", "Arm C") & Week24$PCT_group=="100%")," (", formatC(round(sum(Week24$Arm%in% c("Arm B", "Arm C") & Week24$PCT_group=="100%")*100/(sum(Week24$Arm%in% c("Arm B", "Arm C"))),1),digits=2,format='f'),"%)"))




Pending<-rbind(paste0(sum(Week4$Arm=="Pending" & Week4$PCT_group=="<90%")," (", formatC(round(sum(Week4$Arm=="Pending" & Week4$PCT_group=="<90%")*100/(sum(Week4$Arm=="Pending")),1),digits=2,format='f'),"%)"),
               paste0(sum(Week4$Arm=="Pending" & Week4$PCT_group=="90% - <95%")," (", formatC(round(sum(Week4$Arm=="Pending" & Week4$PCT_group=="90% - <95%")*100/(sum(Week4$Arm=="Pending")),1),digits=2,format='f'),"%)"),
               paste0(sum(Week4$Arm=="Pending" & Week4$PCT_group=="95% - <100%")," (", formatC(round(sum(Week4$Arm=="Pending" & Week4$PCT_group=="95% - <100%")*100/(sum(Week4$Arm=="Pending")),1),digits=2,format='f'),"%)"),
               paste0(sum(Week4$Arm=="Pending" & Week4$PCT_group=="100%")," (", formatC(round(sum(Week4$Arm=="Pending" & Week4$PCT_group=="100%")*100/(sum(Week4$Arm=="Pending")),1),digits=2,format='f'),"%)"),
               paste0(sum(Week8$Arm=="Pending" & Week8$PCT_group=="<90%")," (", formatC(round(sum(Week8$Arm=="Pending" & Week8$PCT_group=="<90%")*100/(sum(Week8$Arm=="Pending")),1),digits=2,format='f'),"%)"),
               paste0(sum(Week8$Arm=="Pending" & Week8$PCT_group=="90% - <95%")," (", formatC(round(sum(Week8$Arm=="Pending" & Week8$PCT_group=="90% - <95%")*100/(sum(Week8$Arm=="Pending")),1),digits=2,format='f'),"%)"),
               paste0(sum(Week8$Arm=="Pending" & Week8$PCT_group=="95% - <100%")," (", formatC(round(sum(Week8$Arm=="Pending" & Week8$PCT_group=="95% - <100%")*100/(sum(Week8$Arm=="Pending")),1),digits=2,format='f'),"%)"),
               paste0(sum(Week8$Arm=="Pending" & Week8$PCT_group=="100%")," (", formatC(round(sum(Week8$Arm=="Pending" & Week8$PCT_group=="100%")*100/(sum(Week8$Arm=="Pending")),1),digits=2,format='f'),"%)"),
               paste0(sum(Week12$Arm=="Pending" & Week12$PCT_group=="<90%")," (", formatC(round(sum(Week12$Arm=="Pending" & Week12$PCT_group=="<90%")*100/(sum(Week12$Arm=="Pending")),1),digits=2,format='f'),"%)"),
               paste0(sum(Week12$Arm=="Pending" & Week12$PCT_group=="90% - <95%")," (", formatC(round(sum(Week12$Arm=="Pending" & Week12$PCT_group=="90% - <95%")*100/(sum(Week12$Arm=="Pending")),1),digits=2,format='f'),"%)"),
               paste0(sum(Week12$Arm=="Pending" & Week12$PCT_group=="95% - <100%")," (", formatC(round(sum(Week12$Arm=="Pending" & Week12$PCT_group=="95% - <100%")*100/(sum(Week12$Arm=="Pending")),1),digits=2,format='f'),"%)"),
               paste0(sum(Week12$Arm=="Pending" & Week12$PCT_group=="100%")," (", formatC(round(sum(Week12$Arm=="Pending" & Week12$PCT_group=="100%")*100/(sum(Week12$Arm=="Pending")),1),digits=2,format='f'),"%)"),
               paste0(sum(Week16$Arm=="Pending" & Week16$PCT_group=="<90%")," (", formatC(round(sum(Week16$Arm=="Pending" & Week16$PCT_group=="<90%")*100/(sum(Week16$Arm=="Pending")),1),digits=2,format='f'),"%)"),
               paste0(sum(Week16$Arm=="Pending" & Week16$PCT_group=="90% - <95%")," (", formatC(round(sum(Week16$Arm=="Pending" & Week16$PCT_group=="90% - <95%")*100/(sum(Week16$Arm=="Pending")),1),digits=2,format='f'),"%)"),
               paste0(sum(Week16$Arm=="Pending" & Week16$PCT_group=="95% - <100%")," (", formatC(round(sum(Week16$Arm=="Pending" & Week16$PCT_group=="95% - <100%")*100/(sum(Week16$Arm=="Pending")),1),digits=2,format='f'),"%)"),
               paste0(sum(Week16$Arm=="Pending" & Week16$PCT_group=="100%")," (", formatC(round(sum(Week16$Arm=="Pending" & Week16$PCT_group=="100%")*100/(sum(Week16$Arm=="Pending")),1),digits=2,format='f'),"%)"),
               paste0(sum(Week24$Arm=="Pending" & Week24$PCT_group=="<90%")," (", formatC(round(sum(Week24$Arm=="Pending" & Week24$PCT_group=="<90%")*100/(sum(Week24$Arm=="Pending")),1),digits=2,format='f'),"%)"),
               paste0(sum(Week24$Arm=="Pending" & Week24$PCT_group=="90% - <95%")," (", formatC(round(sum(Week24$Arm=="Pending" & Week24$PCT_group=="90% - <95%")*100/(sum(Week24$Arm=="Pending")),1),digits=2,format='f'),"%)"),
               paste0(sum(Week24$Arm=="Pending" & Week24$PCT_group=="95% - <100%")," (", formatC(round(sum(Week24$Arm=="Pending" & Week24$PCT_group=="95% - <100%")*100/(sum(Week24$Arm=="Pending")),1),digits=2,format='f'),"%)"),
               paste0(sum(Week24$Arm=="Pending" & Week24$PCT_group=="100%")," (", formatC(round(sum(Week24$Arm=="Pending" & Week24$PCT_group=="100%")*100/(sum(Week24$Arm=="Pending")),1),digits=2,format='f'),"%)"))


Table8<-as.data.frame(rbind(cbind(rep(c("Week 4","Week 8","Week 12","Week 16","Week 24"),each=4),
                                  rep(c("<90%","90% - <95%","95% - <100%","100%"),5),
                                  ArmA,
                                  ArmBC, Pending)))%>%
  mutate(V5=as.character(V5),V5=ifelse(V5=="0 (NaN%)", "NA",V5),
         V5 = ifelse(V1 %in% c("Week 4","Week 8", "Week 12"), "0 (0.00%)", V5))


PID_table8 <- Week4%>%select(SUBJID, Arm, Numerator, Denominator, PCT, PCT_group)%>%mutate(Week=4)%>%
  bind_rows(Week8%>%select(SUBJID, Arm, Numerator, Denominator, PCT, PCT_group)%>%mutate(Week=8))%>%
  bind_rows(Week8%>%select(SUBJID, Arm, Numerator, Denominator, PCT, PCT_group)%>%mutate(Week=12))%>%
  bind_rows(Week8%>%select(SUBJID, Arm, Numerator, Denominator, PCT, PCT_group)%>%mutate(Week=16))%>%
  bind_rows(Week8%>%select(SUBJID, Arm, Numerator, Denominator, PCT, PCT_group)%>%mutate(Week=24))


createSheet(wb, name = "Table8_PID")
writeWorksheet(wb, PID_table8, sheet = "Table8_PID")


myft_table8<-flextable(Table8)%>%
  theme_box()%>%
  bg(bg="grey70", part="header")%>%
  font(fontname = "Arial", part="all")%>%
  fontsize(size=10, part="all")%>%
  
  width(j=1:5,width=c(1.5,2.5,1.5,1.5,1.5))%>%
  align(j=3:5,align="center", part="all")%>%
  align(j=1:2,align="left", part="all")%>%
  merge_at(i=1:4, j=1)%>%
  merge_at(i=5:8, j=1)%>%
  merge_at(i=9:12, j=1)%>%
  merge_at(i=13:16, j=1)%>%
  merge_at(i=17:20, j=1)%>%
  set_header_labels(values=list(V1 = "Visit",
                                V2 = "Percentage of dose taken",
                                V3 = "Arm A\nN(%)",
                                V4 = "Arm B/C*\nN(%)",
                                V5 = "Pending Assignment\nN(%)"))%>%
  add_footer_lines(values = "*Dosing at week24 is only continued in Arm B. Arm C dosing is completed at week16")

