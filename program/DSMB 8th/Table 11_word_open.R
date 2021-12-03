
plate12<-myInputData("012")%>%
  select(SUBJID,VISITNUM,MBORRES_MGIT,MBORRES_MGIT2)%>%
  mutate(MBORRES_MGIT=ifelse(MBORRES_MGIT2!="Blank",MBORRES_MGIT2,MBORRES_MGIT))%>%
  select(-MBORRES_MGIT2)


plate12_check<-myInputData("012")%>%
  select(SUBJID,VISITNUM,MBORRES_MGIT,MBORRES_MGIT2)%>%
  filter(MBORRES_MGIT2!="Blank")


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

check_BASE<-plate9_BASE%>%
  filter(num_0!=num_8 | num_90!=num_98)


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
  filter((VISITNUM>=10 & VISITNUM<90) |  (VISITNUM>=100 & VISITNUM<750))%>%
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
  mutate(MBORRES_MGIT_final=pmin(MGIT0, MGIT8, MGIT9,na.rm=TRUE))
#mutate(MBORRES_MGIT_final=ifelse(SUBJID==11035 & VISITNUM==360,4,MBORRES_MGIT_final))  ### Hard code, confirmed with Dereck

check_FOLLOW<-plate9_FOLLOW_0%>%
  filter(MBORRES_MGIT!=MBORRES_MGIT_8 | MBORRES_MGIT!=MBORRES_MGIT_9)%>%
  filter(!MBORRES_MGIT_8 %in% c("TB+","TB "))


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
  right_join(INREPORT, by="SUBJID")

plate9_Combine<- plate9_Combine%>%mutate(VISITNUM= ifelse(VISITNUM==260, 360, VISITNUM))

#########################
##
##  Table 11a: Total
##
#########################

dta<-plate9_Combine%>%
  select(SUBJID, VISITNUM, MBORRES_MGIT)%>%
  bind_rows(data.frame(SUBJID=15002, VISITNUM=200, MBORRES_MGIT="Not Done"))

count<-as.data.frame.matrix(table(dta$VISITNUM,dta$MBORRES_MGIT))
pct<-round(as.data.frame.matrix(prop.table(table(dta$VISITNUM,dta$MBORRES_MGIT),1))*100,1)

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

Table11a<-data.frame(Visit=Visit,TB_pos=paste0(count$`TB+`," (",pct$`TB+`,"%)"),
                     TB_neg=paste0(count$`TB `," (",pct$`TB `,"%)"),
                     TB_cont=paste0(count$Contaminated," (",pct$Contaminated,"%)"),
                     TB_pend=paste0(count$Blank," (",pct$Blank,"%)"),
                     TB_error=paste0(count$Alarm," (",pct$Alarm,"%)"),
                     TB_not=paste0(count$`Not Done`," (",pct$`Not Done`,"%)"))


createSheet(wb, name = "Table11a_PID")
writeWorksheet(wb, dta, sheet = "Table11a_PID")


#########################
##
##  Table 11b: SA
##
#########################

dta<-plate9_Combine%>%
  select(SUBJID, VISITNUM, MBORRES_MGIT)%>%
  bind_rows(data.frame(SUBJID=15002, VISITNUM=200, MBORRES_MGIT="Not Done"))%>%
  filter(substr(SUBJID,1,1)=="1")

count<-as.data.frame.matrix(table(dta$VISITNUM,dta$MBORRES_MGIT))
pct<-round(as.data.frame.matrix(prop.table(table(dta$VISITNUM,dta$MBORRES_MGIT),1))*100,1)

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

Table11b<-data.frame(Visit=Visit,TB_pos=paste0(count$`TB+`," (",pct$`TB+`,"%)"),
                     TB_neg=paste0(count$`TB `," (",pct$`TB `,"%)"),
                     TB_cont=paste0(count$Contaminated," (",pct$Contaminated,"%)"),
                     TB_pend=paste0(count$Blank," (",pct$Blank,"%)"),
                     TB_error=paste0(count$Alarm," (",pct$Alarm,"%)"),
                     TB_not=paste0(count$`Not Done`," (",pct$`Not Done`,"%)"))



#########################
##
##  Table 11c: China
##
#########################

dta<-plate9_Combine%>%
  select(SUBJID, VISITNUM, MBORRES_MGIT)%>%
  bind_rows(data.frame(SUBJID=15002, VISITNUM=200, MBORRES_MGIT="Not Done"))%>%
  filter(substr(SUBJID,1,1)=="2")

count<-as.data.frame.matrix(table(dta$VISITNUM,dta$MBORRES_MGIT))
pct<-round(as.data.frame.matrix(prop.table(table(dta$VISITNUM,dta$MBORRES_MGIT),1))*100,1)

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

Table11c<-data.frame(Visit=Visit,TB_pos=paste0(count$`TB+`," (",pct$`TB+`,"%)"),
                     TB_neg=paste0(count$`TB `," (",pct$`TB `,"%)"),
                     TB_cont=paste0(count$Contaminated," (",pct$Contaminated,"%)"),
                     TB_pend=paste0(count$Blank," (",pct$Blank,"%)"),
                     TB_error=paste0(count$Alarm," (",pct$Alarm,"%)"),
                     TB_not=paste0(count$`Not Done`," (",pct$`Not Done`,"%)"))%>%
  mutate(TB_not=as.character(TB_not),
         TB_not="0 (0%)",
         TB_error=as.character(TB_error),
         TB_error="0 (0%)")     ### Need to check next time



############################
##
## Generate outputs
#############################

myft_table11a<-flextable(Table11a)%>%
  theme_box()%>%
  bg(bg="grey70", part="header")%>%
  font(fontname = "Arial", part="all")%>%
  fontsize(size=10, part="all")%>%
  width(j=1:7,width=c(2.5,1.2,1.2,1.2,1.2,1.2,1.2))%>%
  align(j=1,align="left", part="all")%>%
  align(j=2:7,align="center", part="all")%>%
  set_header_labels(values=list(Visit="Visit",
                                TB_pos= "TB positive\nN (%)",
                                TB_neg= "TB negative\nN (%)",
                                TB_cont= "Culture Contaminated\nN (%)",
                                TB_pend="Pending\nN (%)",
                                TB_error="No result/error\nN (%)",
                                TB_not="Not Done\nN (%)"))%>%
  add_footer_lines(values="*Denominator of the percentage is total number of participants who had sputum sample collected for a study visit.")



myft_table11b<-flextable(Table11b)%>%
  theme_box()%>%
  bg(bg="grey70", part="header")%>%
  font(fontname = "Arial", part="all")%>%
  fontsize(size=10, part="all")%>%
  width(j=1:7,width=c(2.5,1.2,1.2,1.2,1.2,1.2,1.2))%>%
  align(j=1,align="left", part="all")%>%
  align(j=2:7,align="center", part="all")%>%
  set_header_labels(values=list(Visit="Visit",
                                TB_pos= "TB positive\nN (%)",
                                TB_neg= "TB negative\nN (%)",
                                TB_cont= "Culture Contaminated\nN (%)",
                                TB_pend="Pending\nN (%)",
                                TB_error="No result/error\nN (%)",
                                TB_not="Not Done\nN (%)"))%>%
  add_footer_lines(values="*Denominator of the percentage is total number of participants who had sputum sample collected for a study visit.")


myft_table11c<-flextable(Table11c)%>%
  theme_box()%>%
  bg(bg="grey70", part="header")%>%
  font(fontname = "Arial", part="all")%>%
  fontsize(size=10, part="all")%>%
  width(j=1:7,width=c(2.5,1.2,1.2,1.2,1.2,1.2,1.2))%>%
  align(j=1,align="left", part="all")%>%
  align(j=2:7,align="center", part="all")%>%
  set_header_labels(values=list(Visit="Visit",
                                TB_pos= "TB positive\nN (%)",
                                TB_neg= "TB negative\nN (%)",
                                TB_cont= "Culture Contaminated\nN (%)",
                                TB_pend="Pending\nN (%)",
                                TB_error="No result/error\nN (%)",
                                TB_not="Not Done\nN (%)"))%>%
  add_footer_lines(values="*Denominator of the percentage is total number of participants who had sputum sample collected for a study visit.")




