
plate13<-myInputData("013")%>%
  mutate(MBORRES_ProbeA=ifelse(!is.na(MBORRES_ProbeA) & MBORRES_ProbeA==0, 38, MBORRES_ProbeA),
         MBORRES_ProbeB=ifelse(!is.na(MBORRES_ProbeB) & MBORRES_ProbeB==0, 38, MBORRES_ProbeB),
         MBORRES_ProbeC=ifelse(!is.na(MBORRES_ProbeC) & MBORRES_ProbeC==0, 38, MBORRES_ProbeC),
         MBORRES_ProbeD=ifelse(!is.na(MBORRES_ProbeD) & MBORRES_ProbeD==0, 38, MBORRES_ProbeD),
         MBORRES_ProbeE=ifelse(!is.na(MBORRES_ProbeE) & MBORRES_ProbeE==0, 38, MBORRES_ProbeE)
  )%>%
  select(SUBJID,VISITNUM,MBORRES_ProbeA,MBORRES_ProbeB,MBORRES_ProbeC,MBORRES_ProbeD,MBORRES_ProbeE)

min_zero<-function(x){
  if(any(is.na(x))){
    x<-NA
  }else{
    x<-min(x,na.rm=TRUE)
  }
  return(x)
}

plate13$CT<-apply(cbind(plate13$MBORRES_ProbeA,plate13$MBORRES_ProbeB,plate13$MBORRES_ProbeC,plate13$MBORRES_ProbeD,
                        plate13$MBORRES_ProbeE),1,min_zero)


dta<-INREPORT%>%
  left_join(plate13, by="SUBJID")%>%
  filter(VISITNUM==160)%>%
  mutate(ARM=ifelse(!is.na(BASE_YN) & BASE_YN=="Yes" & !is.na(WEEK4_YN) & WEEK4_YN=="Yes" & !is.na(CCORRES_Week16MinDose) & CCORRES_Week16MinDose=="Yes" & !is.na(CCORRES_Week16) & CCORRES_Week16=="Yes","LowRisk",
                    ifelse(!is.na(BASE_YN) & BASE_YN=="Yes" & !is.na(WEEK4_YN) & WEEK4_YN=="Yes" & !is.na(CCORRES_Week16MinDose) & CCORRES_Week16MinDose=="Yes" & !is.na(CCORRES_Week16) & CCORRES_Week16=="No","HighRisk1",
                           ifelse(((!is.na(BASE_YN) & BASE_YN=="No") | (!is.na(WEEK4_YN) & WEEK4_YN=="No") | (!is.na(CCORRES_Week16MinDose) & CCORRES_Week16MinDose=="No")) & !is.na(CCORRES_Week16) & CCORRES_Week16=="Yes","HighRisk2",NA))))%>%
  mutate(ARM=ifelse(SUBJID %in% c(14067, 14130, 15075,13039), "HighRisk2", ARM))



forReport<-dta%>%
  filter(!is.na(ARM))%>%
  group_by(ARM)%>%
  summarise(Mean=round(mean(CT,na.rm=T),1),
            SD=round(sd(CT,na.rm=T),1))%>%
  mutate(stat=paste(Mean,"(",SD,")"))
forReportAll<-dta%>%
  filter(!is.na(ARM))%>%
  summarise(Mean=round(mean(CT,na.rm=T),1),
            SD=round(sd(CT,na.rm=T),1),
            stat=paste(Mean,"(",SD,")"))

Table9a<-as.data.frame(cbind("Ct value",forReport$stat[forReport$ARM=="LowRisk"],
                             forReport$stat[forReport$ARM=="HighRisk1"],forReportAll$stat))

createSheet(wb, name = "Table9a_PID")
writeWorksheet(wb, dta%>%select(SUBJID, ARM, CT), sheet = "Table9a_PID")


myft_table9a<-flextable(Table9a)%>%
  theme_box()%>%
  bg(bg="grey70", part="header")%>%
  font(fontname = "Arial", part="all")%>%
  fontsize(size=10, part="all")%>%
  
  width(j=1:4,width=c(1.5,2.5,2.5,2.5))%>%
  align(j=2:4,align="center", part="all")%>%
  align(j=1,align="left", part="all")%>%
  set_header_labels(values=list(V1 = "  ",
                                V2 = "Low Risk: Arms B/C\nMean (SD)",
                                V3 = "High Risk: Move to Arm A because of\nXpert alone (Ct<28)\nMean (SD)",
                                V4 = "Overall\nMean (SD)"))%>%
  add_footer_lines(values="Fifteen participants were randomized at the beginning of the study, before the May 2018 protocol amendment changed the baseline and week 4 radiology criteria, and the week 16 Xpert criterion from <30 to <28. Of these, 3 moved to arm A with Xpert value >28 but <30 (Ct=28.4, 28.4, 28.5).")



##################
## Table 9b
#####################
plate11<-myInputData("011")%>%
  select(SUBJID,VISITNUM,MBORRES_LJ,MBORRES_LJ2)%>%
  mutate(MBORRES_LJ=ifelse(MBORRES_LJ2!="Blank",MBORRES_LJ2,MBORRES_LJ))%>%
  select(-MBORRES_LJ2)


plate9_BASE<-myInputData("009")%>%
  select(SUBJID,VISITNUM,MBDTC_Received,MBORRES_CK_Micro )%>%
  filter((MBDTC_Received!="" & MBORRES_CK_Micro=="Microbiology" &  !is.na(MBORRES_CK_Micro) & MBORRES_CK_Micro!="Blank"))%>%
  arrange(SUBJID,VISITNUM)%>%
  mutate(spuReceiveDate=as.Date(MBDTC_Received,format="%d/%B/%Y"))%>%
  mutate(VISITNUM=ifelse(VISITNUM%%10 %in% c(1,2,3),floor(VISITNUM/10)*10,VISITNUM))%>%
  group_by(SUBJID,VISITNUM)%>%
  summarise(MBDTC_Received=first(MBDTC_Received),MBORRES_CK_Micro=first(MBORRES_CK_Micro),spuReceiveDate=first(spuReceiveDate))%>%
  left_join(plate11,by=c("SUBJID","VISITNUM"))%>%
  filter(VISITNUM<10 | (VISITNUM>=90 & VISITNUM<120))%>%
  select(SUBJID,VISITNUM,MBORRES_LJ)%>%
  mutate(MBORRES_LJ=ifelse(!is.na(MBORRES_LJ) & MBORRES_LJ=="Blank",NA,MBORRES_LJ))%>%
  spread(VISITNUM,MBORRES_LJ)%>%
  rowwise()%>%
  mutate(num_0=ifelse(!is.na(`0`)&`0`=="TB+",1,
                      ifelse(!is.na(`0`)&`0`=="TB ",2,
                             ifelse(!is.na(`0`)&`0`=="Contaminated",3,
                                    ifelse(!is.na(`0`)&`0`=="Alarm",4,
                                           ifelse(!is.na(`0`)&`0`=="Not Done",9,NA))))),
         num_8=ifelse(!is.na(`8`)&`8`=="TB+",1,
                      ifelse(!is.na(`8`)&`8`=="TB ",2,
                             ifelse(!is.na(`8`)&`8`=="Contaminated",3,
                                    ifelse(!is.na(`8`)&`8`=="Alarm",4,
                                           ifelse(!is.na(`8`)&`8`=="Not Done",9,NA))))),
         num_90=ifelse(!is.na(`90`)&`90`=="TB+",1,
                       ifelse(!is.na(`90`)&`90`=="TB ",2,
                              ifelse(!is.na(`90`)&`90`=="Contaminated",3,
                                     ifelse(!is.na(`90`)&`90`=="Alarm",4,
                                            ifelse(!is.na(`90`)&`90`=="Not Done",9,NA))))),
         num_98=ifelse(!is.na(`98`)&`98`=="TB+",1,
                       ifelse(!is.na(`98`)&`98`=="TB ",2,
                              ifelse(!is.na(`98`)&`98`=="Contaminated",3,
                                     ifelse(!is.na(`98`)&`98`=="Alarm",4,
                                            ifelse(!is.na(`98`)&`98`=="Not Done",9,NA))))))%>%
  mutate(BASE=pmin(num_0,num_8,na.rm=T),SCREEN=pmin(num_90,num_98,na.rm=TRUE),MIN=pmin(BASE,SCREEN,na.rm=T))%>%
  mutate(VISITNUM=0,MBORRES_LJ=ifelse(BASE==1 | SCREEN==1,1,
                                      ifelse(BASE==2 | SCREEN==2,2,
                                             ifelse(!BASE %in% c(1,2),MIN,NA))),
         MBORRES_LJ=ifelse(is.infinite(MBORRES_LJ),NA,MBORRES_LJ),
         LJ=ifelse(!is.na(MBORRES_LJ)&MBORRES_LJ==1,"TB+",
                   ifelse(!is.na(MBORRES_LJ)&MBORRES_LJ==2,"TB ",
                          ifelse(!is.na(MBORRES_LJ)&MBORRES_LJ==3,"Contaminated",
                                 ifelse(!is.na(MBORRES_LJ)&MBORRES_LJ==4,"Alarm",
                                        ifelse(!is.na(MBORRES_LJ)&MBORRES_LJ==9,"Not Done",NA))))))


plate9_BASE<-plate9_BASE%>%
  select(SUBJID,VISITNUM,LJ)%>%
  rename(MBORRES_LJ=LJ)


plate9_FOLLOW<-myInputData("009")%>%
  select(SUBJID,VISITNUM,MBDTC_Received,MBORRES_CK_Micro )%>%
  filter((MBDTC_Received!="" & MBORRES_CK_Micro=="Microbiology" & !is.na(MBORRES_CK_Micro) & MBORRES_CK_Micro!="Blank"))%>%
  arrange(SUBJID,VISITNUM)%>%
  mutate(spuReceiveDate=as.Date(MBDTC_Received,format="%d/%B/%Y"))%>%
  mutate(VISITNUM=ifelse(VISITNUM%%10 %in% c(1,2,3),floor(VISITNUM/10)*10,VISITNUM))%>%
  group_by(SUBJID,VISITNUM)%>%
  summarise(MBDTC_Received=first(MBDTC_Received),MBORRES_CK_Micro=first(MBORRES_CK_Micro),spuReceiveDate=first(spuReceiveDate))%>%
  left_join(plate11,by=c("SUBJID","VISITNUM"))%>%
  filter((VISITNUM>=10 & VISITNUM<90) |  (VISITNUM>=100 & VISITNUM<800))%>%
  filter(VISITNUM%%10==0 | VISITNUM%%10==8 | VISITNUM%%10==9)%>%
  select(SUBJID,VISITNUM,MBORRES_LJ)%>%
  mutate(MBORRES_LJ=ifelse(!is.na(MBORRES_LJ) & MBORRES_LJ=="Blank",NA,MBORRES_LJ))

plate9_FOLLOW_8<-plate9_FOLLOW%>%
  filter(VISITNUM%%10==8)%>%
  filter(!is.na(MBORRES_LJ))%>%
  mutate(VISITNUM=floor(VISITNUM/10)*10)%>%
  rename(MBORRES_LJ_8=MBORRES_LJ)

plate9_FOLLOW_9<-plate9_FOLLOW%>%
  filter(VISITNUM%%10==9)%>%
  filter(!is.na(MBORRES_LJ))%>%
  mutate(VISITNUM=floor(VISITNUM/10)*10)%>%
  rename(MBORRES_LJ_9=MBORRES_LJ)

plate9_FOLLOW_0<-plate9_FOLLOW%>%
  filter(VISITNUM%%10==0)%>%
  full_join(plate9_FOLLOW_8,by=c("SUBJID","VISITNUM"))%>%
  full_join(plate9_FOLLOW_9,by=c("SUBJID","VISITNUM"))%>%
  mutate(LJ0=ifelse(!is.na(MBORRES_LJ)&MBORRES_LJ=="TB+",1,
                    ifelse(!is.na(MBORRES_LJ)&MBORRES_LJ=="TB ",2,
                           ifelse(!is.na(MBORRES_LJ)&MBORRES_LJ=="Contaminated",3,
                                  ifelse(!is.na(MBORRES_LJ)&MBORRES_LJ=="Alarm",4,
                                         ifelse(!is.na(MBORRES_LJ)&MBORRES_LJ=="Not Done",9,NA))))),
         LJ8=ifelse(!is.na(MBORRES_LJ_8)&MBORRES_LJ_8=="TB+",1,
                    ifelse(!is.na(MBORRES_LJ_8)&MBORRES_LJ_8=="TB ",2,
                           ifelse(!is.na(MBORRES_LJ_8)&MBORRES_LJ_8=="Contaminated",3,
                                  ifelse(!is.na(MBORRES_LJ_8)&MBORRES_LJ_8=="Alarm",4,
                                         ifelse(!is.na(MBORRES_LJ_8)&MBORRES_LJ_8=="Not Done",9,NA))))),
         LJ9=ifelse(!is.na(MBORRES_LJ_9)&MBORRES_LJ_9=="TB+",1,
                    ifelse(!is.na(MBORRES_LJ_9)&MBORRES_LJ_9=="TB ",2,
                           ifelse(!is.na(MBORRES_LJ_9)&MBORRES_LJ_9=="Contaminated",3,
                                  ifelse(!is.na(MBORRES_LJ_9)&MBORRES_LJ_9=="Alarm",4,
                                         ifelse(!is.na(MBORRES_LJ_9)&MBORRES_LJ_9=="Not Done",9,NA))))))%>%
  mutate(MBORRES_LJ_final=pmin(LJ0, LJ8, LJ9,na.rm=TRUE))

plate9_Combine<-plate9_FOLLOW_0%>%
  select(SUBJID, VISITNUM, MBORRES_LJ=MBORRES_LJ_final)%>%
  mutate(MBORRES_LJ=ifelse(!is.na(MBORRES_LJ)&MBORRES_LJ==1,"TB+",
                           ifelse(!is.na(MBORRES_LJ)&MBORRES_LJ==2,"TB ",
                                  ifelse(!is.na(MBORRES_LJ)&MBORRES_LJ==3,"Contaminated",
                                         ifelse(!is.na(MBORRES_LJ)&MBORRES_LJ==4,"Alarm",
                                                ifelse(!is.na(MBORRES_LJ)&MBORRES_LJ==9,"Not Done",NA))))))%>%
  bind_rows(plate9_BASE)%>%
  mutate(MBORRES_LJ=ifelse(MBORRES_LJ=="Alarm" | is.na(MBORRES_LJ),"Blank",MBORRES_LJ))%>%
  right_join(INREPORT, by="SUBJID")%>%
  filter(VISITNUM>=160 & VISITNUM<200)


dta2<-dta%>%
  select(SUBJID,CT)%>%
  inner_join(plate9_Combine,by="SUBJID")%>%
  filter(!is.na(CT) & !is.na(MBORRES_LJ) & MBORRES_LJ!="Blank" & MBORRES_LJ!="Contaminated" & MBORRES_LJ!="Not Done")%>%
  mutate(CTcut=ifelse(CT<28,"CT<28","CT>=28"))

CTLJ_check<-dta2%>%
  filter(MBORRES_LJ=="TB+")


forReport<-as.data.frame.matrix(table(dta2$MBORRES_LJ,dta2$CTcut))

Table9b<-rbind(cbind(" ","CT<28","CT>=28"),
               cbind("LJ Positive",as.character(forReport[2,1]),as.character(forReport[2,2])),
               cbind("LJ Negative",as.character(forReport[1,1]),as.character(forReport[1,2])))

createSheet(wb, name = "Table9b_PID")
writeWorksheet(wb, dta2%>%select(SUBJID, MBORRES_LJ, CTcut), sheet = "Table9b_PID")


##################
## Table 9c
#####################

plate12<-myInputData("012")%>%
  select(SUBJID,VISITNUM,MBORRES_MGIT,MBORRES_MGIT2)%>%
  mutate(MBORRES_MGIT=ifelse(MBORRES_MGIT2!="Blank",MBORRES_MGIT2,MBORRES_MGIT))%>%
  select(-MBORRES_MGIT2)


plate9_BASE<-myInputData("009")%>%
  select(SUBJID,VISITNUM,MBDTC_Received,MBORRES_CK_Micro )%>%
  filter((MBDTC_Received!="" & MBORRES_CK_Micro=="Microbiology" &  !is.na(MBORRES_CK_Micro) & MBORRES_CK_Micro!="Blank"))%>%
  arrange(SUBJID,VISITNUM)%>%
  mutate(spuReceiveDate=as.Date(MBDTC_Received,format="%d/%B/%Y"))%>%
  mutate(VISITNUM=ifelse(VISITNUM%%10 %in% c(1,2,3),floor(VISITNUM/10)*10,VISITNUM))%>%
  group_by(SUBJID,VISITNUM)%>%
  summarise(MBDTC_Received=first(MBDTC_Received),MBORRES_CK_Micro=first(MBORRES_CK_Micro),spuReceiveDate=first(spuReceiveDate))%>%
  left_join(plate12,by=c("SUBJID","VISITNUM"))%>%
  filter(VISITNUM<10 | (VISITNUM>=90 & VISITNUM<120))%>%
  select(SUBJID,VISITNUM,MBORRES_MGIT)%>%
  mutate(MBORRES_MGIT=ifelse(!is.na(MBORRES_MGIT) & MBORRES_MGIT=="Blank",NA,MBORRES_MGIT))%>%
  spread(VISITNUM,MBORRES_MGIT)%>%
  rowwise()%>%
  mutate(num_0=ifelse(!is.na(`0`)&`0`=="TB+",1,
                      ifelse(!is.na(`0`)&`0`=="TB ",2,
                             ifelse(!is.na(`0`)&`0`=="Contaminated",3,
                                    ifelse(!is.na(`0`)&`0`=="Alarm",4,
                                           ifelse(!is.na(`0`)&`0`=="Not Done",9,NA))))),
         num_8=ifelse(!is.na(`8`)&`8`=="TB+",1,
                      ifelse(!is.na(`8`)&`8`=="TB ",2,
                             ifelse(!is.na(`8`)&`8`=="Contaminated",3,
                                    ifelse(!is.na(`8`)&`8`=="Alarm",4,
                                           ifelse(!is.na(`8`)&`8`=="Not Done",9,NA))))),
         num_90=ifelse(!is.na(`90`)&`90`=="TB+",1,
                       ifelse(!is.na(`90`)&`90`=="TB ",2,
                              ifelse(!is.na(`90`)&`90`=="Contaminated",3,
                                     ifelse(!is.na(`90`)&`90`=="Alarm",4,
                                            ifelse(!is.na(`90`)&`90`=="Not Done",9,NA))))),
         num_98=ifelse(!is.na(`98`)&`98`=="TB+",1,
                       ifelse(!is.na(`98`)&`98`=="TB ",2,
                              ifelse(!is.na(`98`)&`98`=="Contaminated",3,
                                     ifelse(!is.na(`98`)&`98`=="Alarm",4,
                                            ifelse(!is.na(`98`)&`98`=="Not Done",9,NA))))))%>%
  mutate(BASE=pmin(num_0,num_8,na.rm=T),SCREEN=pmin(num_90,num_98,na.rm=TRUE),MIN=pmin(BASE,SCREEN,na.rm=T))%>%
  mutate(VISITNUM=0,MBORRES_MGIT=ifelse(BASE==1 | SCREEN==1,1,
                                        ifelse(BASE==2 | SCREEN==2,2,
                                               ifelse(!BASE %in% c(1,2),MIN,NA))),
         MBORRES_MGIT=ifelse(is.infinite(MBORRES_MGIT),NA,MBORRES_MGIT),
         MGIT=ifelse(!is.na(MBORRES_MGIT)&MBORRES_MGIT==1,"TB+",
                     ifelse(!is.na(MBORRES_MGIT)&MBORRES_MGIT==2,"TB ",
                            ifelse(!is.na(MBORRES_MGIT)&MBORRES_MGIT==3,"Contaminated",
                                   ifelse(!is.na(MBORRES_MGIT)&MBORRES_MGIT==4,"Alarm",
                                          ifelse(!is.na(MBORRES_MGIT)&MBORRES_MGIT==9,"Not Done",NA))))))



plate9_BASE<-plate9_BASE%>%
  select(SUBJID,VISITNUM,MGIT)%>%
  rename(MBORRES_MGIT=MGIT)


plate9_FOLLOW<-myInputData("009")%>%
  select(SUBJID,VISITNUM,MBDTC_Received,MBORRES_CK_Micro )%>%
  filter((MBDTC_Received!="" & MBORRES_CK_Micro=="Microbiology" & !is.na(MBORRES_CK_Micro) & MBORRES_CK_Micro!="Blank"))%>%
  arrange(SUBJID,VISITNUM)%>%
  mutate(spuReceiveDate=as.Date(MBDTC_Received,format="%d/%B/%Y"))%>%
  mutate(VISITNUM=ifelse(VISITNUM%%10 %in% c(1,2,3),floor(VISITNUM/10)*10,VISITNUM))%>%
  group_by(SUBJID,VISITNUM)%>%
  summarise(MBDTC_Received=first(MBDTC_Received),MBORRES_CK_Micro=first(MBORRES_CK_Micro),spuReceiveDate=first(spuReceiveDate))%>%
  left_join(plate12,by=c("SUBJID","VISITNUM"))%>%
  filter((VISITNUM>=10 & VISITNUM<90) |  (VISITNUM>=100 & VISITNUM<800))%>%
  filter(VISITNUM%%10==0 | VISITNUM%%10==8 | VISITNUM%%10==9)%>%
  select(SUBJID,VISITNUM,MBORRES_MGIT)%>%
  mutate(MBORRES_MGIT=ifelse(!is.na(MBORRES_MGIT) & MBORRES_MGIT=="Blank",NA,MBORRES_MGIT))

plate9_FOLLOW_8<-plate9_FOLLOW%>%
  filter(VISITNUM%%10==8)%>%
  filter(!is.na(MBORRES_MGIT))%>%
  mutate(VISITNUM=floor(VISITNUM/10)*10)%>%
  rename(MBORRES_MGIT_8=MBORRES_MGIT)

plate9_FOLLOW_9<-plate9_FOLLOW%>%
  filter(VISITNUM%%10==9)%>%
  filter(!is.na(MBORRES_MGIT))%>%
  mutate(VISITNUM=floor(VISITNUM/10)*10)%>%
  rename(MBORRES_MGIT_9=MBORRES_MGIT)

plate9_FOLLOW_0<-plate9_FOLLOW%>%
  filter(VISITNUM%%10==0)%>%
  full_join(plate9_FOLLOW_8,by=c("SUBJID","VISITNUM"))%>%
  full_join(plate9_FOLLOW_9,by=c("SUBJID","VISITNUM"))%>%
  mutate(MGIT0=ifelse(!is.na(MBORRES_MGIT)&MBORRES_MGIT=="TB+",1,
                      ifelse(!is.na(MBORRES_MGIT)&MBORRES_MGIT=="TB ",2,
                             ifelse(!is.na(MBORRES_MGIT)&MBORRES_MGIT=="Contaminated",3,
                                    ifelse(!is.na(MBORRES_MGIT)&MBORRES_MGIT=="Alarm",4,
                                           ifelse(!is.na(MBORRES_MGIT)&MBORRES_MGIT=="Not Done",9,NA))))),
         MGIT8=ifelse(!is.na(MBORRES_MGIT_8)&MBORRES_MGIT_8=="TB+",1,
                      ifelse(!is.na(MBORRES_MGIT_8)&MBORRES_MGIT_8=="TB ",2,
                             ifelse(!is.na(MBORRES_MGIT_8)&MBORRES_MGIT_8=="Contaminated",3,
                                    ifelse(!is.na(MBORRES_MGIT_8)&MBORRES_MGIT_8=="Alarm",4,
                                           ifelse(!is.na(MBORRES_MGIT_8)&MBORRES_MGIT_8=="Not Done",9,NA))))),
         MGIT9=ifelse(!is.na(MBORRES_MGIT_9)&MBORRES_MGIT_9=="TB+",1,
                      ifelse(!is.na(MBORRES_MGIT_9)&MBORRES_MGIT_9=="TB ",2,
                             ifelse(!is.na(MBORRES_MGIT_9)&MBORRES_MGIT_9=="Contaminated",3,
                                    ifelse(!is.na(MBORRES_MGIT_9)&MBORRES_MGIT_9=="Alarm",4,
                                           ifelse(!is.na(MBORRES_MGIT_9)&MBORRES_MGIT_9=="Not Done",9,NA))))))%>%
  mutate(MBORRES_MGIT_final=pmin(MGIT0, MGIT8, MGIT9,na.rm=TRUE))%>%
  mutate(MBORRES_MGIT_final=ifelse(SUBJID==11035 & VISITNUM==360,4,MBORRES_MGIT_final))  ### Hard code, confirmed with Dereck


plate9_Combine<-plate9_FOLLOW_0%>%
  select(SUBJID, VISITNUM, MBORRES_MGIT=MBORRES_MGIT_final)%>%
  mutate(MBORRES_MGIT=ifelse(!is.na(MBORRES_MGIT)&MBORRES_MGIT==1,"TB+",
                             ifelse(!is.na(MBORRES_MGIT)&MBORRES_MGIT==2,"TB ",
                                    ifelse(!is.na(MBORRES_MGIT)&MBORRES_MGIT==3,"Contaminated",
                                           ifelse(!is.na(MBORRES_MGIT)&MBORRES_MGIT==4,"Alarm",
                                                  ifelse(!is.na(MBORRES_MGIT)&MBORRES_MGIT==9,"Not Done",NA))))))%>%
  bind_rows(plate9_BASE)%>%
  mutate(MBORRES_MGIT=ifelse(MBORRES_MGIT=="Underterminated","Alarm",MBORRES_MGIT))%>%
  mutate(MBORRES_MGIT=ifelse(is.na(MBORRES_MGIT),"Blank",MBORRES_MGIT))%>%
  right_join(INREPORT, by="SUBJID")%>%
  filter(VISITNUM>=160 & VISITNUM<200)



dta2<-dta%>%
  select(SUBJID,CT)%>%
  inner_join(plate9_Combine,by="SUBJID")%>%
  filter(!is.na(CT) & !is.na(MBORRES_MGIT) & MBORRES_MGIT!="Blank" & MBORRES_MGIT!="Contaminated" & 
           MBORRES_MGIT!="Not Done" & MBORRES_MGIT!="Alarm")%>%
  mutate(CTcut=ifelse(CT<28,"CT<28","CT>=28"))

CTMGIT_check<-dta2%>%
  filter(MBORRES_MGIT=="TB+")


forReport<-as.data.frame.matrix(table(dta2$MBORRES_MGIT,dta2$CTcut))

Table9c<-rbind(cbind(" ","CT<28","CT>=28"),
               cbind("MGIT Positive",as.character(forReport[2,1]),as.character(forReport[2,2])),
               cbind("MGIT Negative",as.character(forReport[1,1]),as.character(forReport[1,2])))

createSheet(wb, name = "Table9c_PID")
writeWorksheet(wb, dta2%>%select(SUBJID, MBORRES_MGIT, CTcut), sheet = "Table9c_PID")

####################
##
## Generate output
##
####################
Table9b_out<-as.data.frame(Table9b[-1,])

myft_table9b<-flextable(Table9b_out)%>%
  theme_box()%>%
  bg(bg="grey70", part="header")%>%
  font(fontname = "Arial", part="all")%>%
  fontsize(size=10, part="all")%>%
  
  width(j=1:3,width=c(1.5,2.5,2.5))%>%
  align(j=2:3,align="center", part="all")%>%
  align(j=1,align="left", part="all")%>%
  set_header_labels(values=list(V1 = "  ",
                                V2 = "CT<28",
                                V3 = "CT>=28"))%>%
  add_footer_lines(values="*differences in counts from 10a are due to pending results from LJ.")


Table9c_out<-as.data.frame(Table9c[-1,])

myft_table9c<-flextable(Table9c_out)%>%
  theme_box()%>%
  bg(bg="grey70", part="header")%>%
  font(fontname = "Arial", part="all")%>%
  fontsize(size=10, part="all")%>%
  
  width(j=1:3,width=c(1.5,2.5,2.5))%>%
  align(j=2:3,align="center", part="all")%>%
  align(j=1,align="left", part="all")%>%
  set_header_labels(values=list(V1 = "  ",
                                V2 = "CT<28",
                                V3 = "CT>=28"))%>%
  add_footer_lines(values="*differences in counts from 11a are due to pending results from MGIT.")
