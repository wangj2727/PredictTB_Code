
plate11<-myInputData("011")%>%
  select(SUBJID,VISITNUM,MBORRES_LJ,MBORRES_LJ2)%>%
  mutate(MBORRES_LJ=ifelse(MBORRES_LJ2!="Blank",MBORRES_LJ2,MBORRES_LJ))%>%
  select(-MBORRES_LJ2)


plate11_check<-myInputData("011")%>%
  select(SUBJID,VISITNUM,MBORRES_LJ,MBORRES_LJ2)%>%
  filter(MBORRES_LJ2!="Blank")



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

check_BASE<-plate9_BASE%>%
  filter(num_0!=num_8 | num_90!=num_98)


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
  filter((VISITNUM>=10 & VISITNUM<90) |  (VISITNUM>=100 & VISITNUM<750))%>%
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

check_FOLLOW<-plate9_FOLLOW_0%>%
  filter(MBORRES_LJ!=MBORRES_LJ_8 | MBORRES_LJ!=MBORRES_LJ_9)%>%
  filter(!MBORRES_LJ_8 %in% c("TB+","TB "))


plate9_Combine<-plate9_FOLLOW_0%>%
  select(SUBJID, VISITNUM, MBORRES_LJ=MBORRES_LJ_final)%>%
  mutate(MBORRES_LJ=ifelse(!is.na(MBORRES_LJ)&MBORRES_LJ==1,"TB+",
                           ifelse(!is.na(MBORRES_LJ)&MBORRES_LJ==2,"TB ",
                                  ifelse(!is.na(MBORRES_LJ)&MBORRES_LJ==3,"Contaminated",
                                         ifelse(!is.na(MBORRES_LJ)&MBORRES_LJ==4,"Alarm",
                                                ifelse(!is.na(MBORRES_LJ)&MBORRES_LJ==9,"Not Done",NA))))))%>%
  bind_rows(plate9_BASE)%>%
  mutate(MBORRES_LJ=ifelse(MBORRES_LJ=="Alarm" | is.na(MBORRES_LJ),"Blank",MBORRES_LJ))%>%
  right_join(INREPORT, by="SUBJID")

plate9_Combine<-plate9_Combine%>%mutate(VISITNUM = ifelse(VISITNUM==260, 360, VISITNUM))
## 13115 has visitnum of 260, but should be 360

#########################
##
##  Table 10a: Total
##
#########################
dta<-plate9_Combine%>%
  select(SUBJID, VISITNUM, MBORRES_LJ)%>%
  bind_rows(data.frame(SUBJID=15002, VISITNUM=200, MBORRES_LJ="Not Done"))

count<-as.data.frame.matrix(table(dta$VISITNUM,dta$MBORRES_LJ))
pct<-round(as.data.frame.matrix(prop.table(table(dta$VISITNUM,dta$MBORRES_LJ),1))*100,1)



Visit<-c("Screening/Baseline Visit - Day 0","Week 1 Visit - Day 7","Week 2 Visit - Day 14",
         "Week 4 Visit - Day 28","Week 8 Visit - Day 56","Week 12 Visit - Day 84","Week 16 Visit - Day 112",
         "Week 20 Visit - Day 140","Week 24 Visit - Day 168","Week 36 Visit - Day 252",
         "Week 48 Visit - Day 336","Week 72 Visit - Day 504"
)

Table10a<-data.frame(Visit=Visit,TB_pos=paste0(count$`TB+`," (",pct$`TB+`,"%)"),
                     TB_neg=paste0(count$`TB `," (",pct$`TB `,"%)"),
                     TB_cont=paste0(count$Contaminated," (",pct$Contaminated,"%)"),
                     TB_pend=paste0(count$Blank," (",pct$Blank,"%)"),
                     TB_not=paste0(count$`Not Done`," (",pct$`Not Done`,"%)"))


createSheet(wb, name = "Table10a_PID")
writeWorksheet(wb, dta, sheet = "Table10a_PID")


#########################
##
##  Table 10b: SA
##
#########################

dta<-plate9_Combine%>%
  select(SUBJID, VISITNUM, MBORRES_LJ)%>%
  bind_rows(data.frame(SUBJID=15002, VISITNUM=200, MBORRES_LJ="Not Done"))%>%
  filter(substr(SUBJID,1,1)=="1")

count<-as.data.frame.matrix(table(dta$VISITNUM,dta$MBORRES_LJ))
pct<-round(as.data.frame.matrix(prop.table(table(dta$VISITNUM,dta$MBORRES_LJ),1))*100,1)

if(!"720" %in% rownames(count)){
  count<-rbind(count,rep(0,ncol(count)))
  row.names(count)[nrow(count)]<-"720"
  pct<-rbind(pct,rep(0.0,ncol(count)))
  row.names(pct)[nrow(count)]<-"720"
}

Visit<-c("Screening/Baseline Visit - Day 0","Week 1 Visit - Day 7","Week 2 Visit - Day 14",
         "Week 4 Visit - Day 28","Week 8 Visit - Day 56","Week 12 Visit - Day 84","Week 16 Visit - Day 112",
         "Week 20 Visit - Day 140","Week 24 Visit - Day 168","Week 36 Visit - Day 252",
         "Week 48 Visit - Day 336","Week 72 Visit - Day 504"
)

Table10b<-data.frame(Visit=Visit,TB_pos=paste0(count$`TB+`," (",pct$`TB+`,"%)"),
                     TB_neg=paste0(count$`TB `," (",pct$`TB `,"%)"),
                     TB_cont=paste0(count$Contaminated," (",pct$Contaminated,"%)"),
                     TB_pend=paste0(count$Blank," (",pct$Blank,"%)"),
                     TB_not=paste0(count$`Not Done`," (",pct$`Not Done`,"%)"))



#########################
##
##  Table 10c: China
##
#########################

dta<-plate9_Combine%>%
  select(SUBJID, VISITNUM, MBORRES_LJ)%>%
  bind_rows(data.frame(SUBJID=15002, VISITNUM=200, MBORRES_LJ="Not Done"))%>%
  filter(substr(SUBJID,1,1)=="2")

count<-as.data.frame.matrix(table(dta$VISITNUM,dta$MBORRES_LJ))
pct<-round(as.data.frame.matrix(prop.table(table(dta$VISITNUM,dta$MBORRES_LJ),1))*100,1)

if(!"720" %in% rownames(count)){
  count<-rbind(count,rep(0,ncol(count)))
  row.names(count)[nrow(count)]<-"720"
  pct<-rbind(pct,rep(0.0,ncol(count)))
  row.names(pct)[nrow(count)]<-"720"
}

Visit<-c("Screening/Baseline Visit - Day 0","Week 1 Visit - Day 7","Week 2 Visit - Day 14",
         "Week 4 Visit - Day 28","Week 8 Visit - Day 56","Week 12 Visit - Day 84","Week 16 Visit - Day 112",
         "Week 20 Visit - Day 140","Week 24 Visit - Day 168","Week 36 Visit - Day 252",
         "Week 48 Visit - Day 336","Week 72 Visit - Day 504"
)

Table10c<-data.frame(Visit=Visit,TB_pos=paste0(count$`TB+`," (",pct$`TB+`,"%)"),
                     TB_neg=paste0(count$`TB `," (",pct$`TB `,"%)"),
                     TB_cont=paste0(count$Contaminated," (",pct$Contaminated,"%)"),
                     TB_pend=paste0(count$Blank," (",pct$Blank,"%)"),
                     TB_not=paste0(count$`Not Done`," (",pct$`Not Done`,"%)"))%>%
  mutate(TB_not=as.character(TB_not),
         TB_not="0 (0%)")     ### Need to check next time



############################
##
## Generate outputs
#############################

myft_table10a<-flextable(Table10a)%>%
  theme_box()%>%
  bg(bg="grey70", part="header")%>%
  font(fontname = "Arial", part="all")%>%
  fontsize(size=10, part="all")%>%
  width(j=1:6,width=c(2.5,1.2,1.2,1.2,1.2,1.2))%>%
  align(j=1,align="left", part="all")%>%
  align(j=2:6,align="center", part="all")%>%
  set_header_labels(values=list(Visit="Visit",
                                TB_pos= "TB positive\nN (%)",
                                TB_neg= "TB negative\nN (%)",
                                TB_cont= "Culture Contaminated\nN (%)",
                                TB_pend="Pending\nN (%)",
                                TB_not="Not Done\nN (%)"))%>%
  add_footer_lines(values="*Denominator of the percentage is total number of participants who had sputum sample collected for a study visit.")



myft_table10b<-flextable(Table10b)%>%
  theme_box()%>%
  bg(bg="grey70", part="header")%>%
  font(fontname = "Arial", part="all")%>%
  fontsize(size=10, part="all")%>%
  width(j=1:6,width=c(2.5,1.2,1.2,1.2,1.2,1.2))%>%
  align(j=1,align="left", part="all")%>%
  align(j=2:6,align="center", part="all")%>%
  set_header_labels(values=list(Visit="Visit",
                                TB_pos= "TB positive\nN (%)",
                                TB_neg= "TB negative\nN (%)",
                                TB_cont= "Culture Contaminated\nN (%)",
                                TB_pend="Pending\nN (%)",
                                TB_not="Not Done\nN (%)"))%>%
  add_footer_lines(values="*Denominator of the percentage is total number of participants who had sputum sample collected for a study visit.")


myft_table10c<-flextable(Table10c)%>%
  theme_box()%>%
  bg(bg="grey70", part="header")%>%
  font(fontname = "Arial", part="all")%>%
  fontsize(size=10, part="all")%>%
  width(j=1:6,width=c(2.5,1.2,1.2,1.2,1.2,1.2))%>%
  align(j=1,align="left", part="all")%>%
  align(j=2:6,align="center", part="all")%>%
  set_header_labels(values=list(Visit="Visit",
                                TB_pos= "TB positive\nN (%)",
                                TB_neg= "TB negative\nN (%)",
                                TB_cont= "Culture Contaminated\nN (%)",
                                TB_pend="Pending\nN (%)",
                                TB_not="Not Done\nN (%)"))%>%
  add_footer_lines(values="*Denominator of the percentage is total number of participants who had sputum sample collected for a study visit.")





