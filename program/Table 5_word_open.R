

Window<-data.frame(VISITNUM=c(0,10,20,40,80,120,160,200,240,360,480,600,720),
                   left=c(8,15,22,36,64,92,120,148,176,260,344,428,512),
                   right=c(11,18,25,43,71,99,127,155,183,290,374,458,542))


plate23<-myInputData("023")%>%
  right_join(INREPORT, by="SUBJID")%>%
  select(SUBJID,VISITNUM,VISITDAT)%>%
  filter(VISITNUM!=360)%>%
  filter(VISITNUM<=720)%>%
  mutate(VISITDAT=as.Date(VISITDAT,format="%d/%B/%Y"))

TreatStart<-plate23%>%
  right_join(INREPORT, by="SUBJID")%>%
  filter(VISITNUM==0)%>%
  select(SUBJID,TreatStartDate=VISITDAT)

plate7<-myInputData("007")%>%
  right_join(INREPORT, by="SUBJID")%>%
  filter(VISITNUM %in% c(360,480,720))%>%
  select(SUBJID,VISITNUM,VISITDAT)%>%
  mutate(VISITDAT=as.Date(VISITDAT,format="%d/%B/%Y"))

plate27<-myInputData("027")%>%
  right_join(INREPORT, by="SUBJID")%>%
  select(SUBJID,VISITNUM,VISITDAT)%>%
  filter(VISITNUM<=720)%>%
  mutate(VISITDAT=as.Date(VISITDAT,format="%d/%B/%Y"),
         VISITNUM=ifelse(VISITNUM==368, 360, VISITNUM)) 
### 12084, VISITNUM 368 should be 360 
#it was a misread by the DF (Shawn confirmed on 12Mar2021)

combine<-plate23%>%
  right_join(INREPORT, by="SUBJID")%>%
  bind_rows(plate7)%>%
  bind_rows(plate27)%>%
  arrange(VISITNUM,SUBJID)%>%
  distinct(SUBJID, VISITNUM, .keep_all = TRUE)



################### Data Checking ###################
#table(combine$VISITNUM)
# combine2 <- combine%>%
#   distinct(SUBJID, VISITNUM, .keep_all = TRUE)
# check <- combine%>%anti_join(combine2, by=c("SUBJID","VISITNUM","VISITDAT"))

Visit<-c("# Attended Baseline Visit - Day 0","# Attended Week 1 Visit - Day 7","# Attended Week 2 Visit - Day 14",
         "# Attended Week 4 Visit - Day 28","# Attended Week 8 Visit - Day 56","# Attended Week 12 Visit - Day 84","# Attended Week 16 Visit - Day 112",
         "# Attended Week 20 Visit - Day 140","# Attended Week 24 Visit - Day 168","# Attended Week 36 Visit - Day 252",
         "# Attended Week 48 Visit - Day 336","# Attended Week 60 Visit - Day 420","# Attended Week 72 Visit - Day 504","# Completed the Study","# Currently Active"
)

TreatStart<-TreatStart%>%
  mutate(Interval=as.numeric(as.Date("2021-11-05") - TreatStartDate)) ### Data cutoff date - treatment start date

Window$expect<-NA

for(i in 1:nrow(Window)){
  
  Window$expect[i]<-sum(TreatStart$Interval>=Window$left[i])
}
Window$numerator<-as.numeric(table(combine$VISITNUM))

Window<-Window%>%
  mutate(PCT=round(numerator*100/expect,1),
         PCT=ifelse(PCT>100,100,PCT),
         Count=paste0(numerator," (",PCT,"%)"))%>%
  select(Count)

tab5a<-data.frame(Visit=Visit, All=c(Window$Count,nrow(COMPLETESTUDY_3LTFU),nrow(ACTIVE)))
tab5a$All[1]<-"548 (100%)"   ### hard code add 15112 to baseline row


##########################
##
## SA
##
##########################

combine<-plate23%>%
  right_join(INREPORT, by="SUBJID")%>%
  bind_rows(plate7)%>%
  bind_rows(plate27)%>%
  arrange(VISITNUM,SUBJID)%>%
  filter(SUBJID<20000)%>%
  distinct(SUBJID, VISITNUM, .keep_all = TRUE)


TreatStart<-plate23%>%
  right_join(INREPORT, by="SUBJID")%>%
  filter(VISITNUM==0)%>%
  select(SUBJID,TreatStartDate=VISITDAT)%>%
  filter(SUBJID<20000)


TreatStart<-TreatStart%>%
  mutate(Interval=as.numeric(as.Date("2021-11-05") - TreatStartDate))


Window<-data.frame(VISITNUM=c(0,10,20,40,80,120,160,200,240,360,480,600,720),
                   left=c(8,15,22,36,64,92,120,148,176,260,344,428,512),
                   right=c(11,18,25,43,71,99,127,155,183,290,374,458,542))

Window$expect<-NA

for(i in 1:nrow(Window)){
  
  Window$expect[i]<-sum(TreatStart$Interval>=Window$left[i])
}
Window$numerator<-as.numeric(table(combine$VISITNUM))

Window<-Window%>%
  mutate(PCT=round(numerator*100/expect,1),
         PCT=ifelse(PCT>100,100,PCT),
         Count=paste0(numerator," (",PCT,"%)"))%>%
  select(Count)

tab5b<-data.frame(Visit=Visit, All=c(Window$Count,nrow(COMPLETESTUDY_3LTFU[COMPLETESTUDY_3LTFU$SUBJID<20000,]),
                                     nrow(ACTIVE[ACTIVE$SUBJID<20000,])))

tab5b$All[1]<- "366 (100%)"   ### add 15112 to baseline row


##########################
##
## China
##
##########################

combine<-plate23%>%
  right_join(INREPORT, by="SUBJID")%>%
  bind_rows(plate7)%>%
  bind_rows(plate27)%>%
  arrange(VISITNUM,SUBJID)%>%
  filter(SUBJID>=20000)%>%
  distinct(SUBJID, VISITNUM, .keep_all = TRUE)

TreatStart<-plate23%>%
  right_join(INREPORT, by="SUBJID")%>%
  filter(VISITNUM==0)%>%
  select(SUBJID,TreatStartDate=VISITDAT)%>%
  filter(SUBJID>=20000)

TreatStart<-TreatStart%>%
  mutate(Interval=as.numeric(as.Date("2021-11-05") - TreatStartDate))


Window<-data.frame(VISITNUM=c(0,10,20,40,80,120,160,200,240,360,480,600,720),
                   left=c(8,15,22,36,64,92,120,148,176,260,344,428,512),
                   right=c(11,18,25,43,71,99,127,155,183,290,374,458,542))

Window$expect<-NA

for(i in 1:nrow(Window)){
  
  Window$expect[i]<-sum(TreatStart$Interval>=Window$left[i])
}
Window$numerator<-c(as.numeric(table(combine$VISITNUM)))

Window<-Window%>%
  mutate(PCT=round(numerator*100/expect,1),
         PCT=ifelse(PCT>100,100,PCT),
         Count=paste0(numerator," (",PCT,"%)"))%>%
  select(Count)

tab5c<-data.frame(Visit=Visit, All=c(Window$Count,nrow(COMPLETESTUDY_3LTFU[COMPLETESTUDY_3LTFU$SUBJID>=20000,]),
                                     nrow(ACTIVE[ACTIVE$SUBJID>=20000,])))


##########################
##
## Generate output
##
#########################

myft_table5a<-flextable(tab5a)%>%
  theme_box()%>%
  bg(bg="grey70", part="header")%>%
  font(fontname = "Arial", part="all")%>%
  fontsize(size=10, part="all")%>%
  
  width(j=1:2,width=c(3,2))%>%
  align(j=1,align="left", part="all")%>%
  align(j=2,align="center", part="all")%>%
  set_header_labels(values=list(Visit = " ",
                                All = "All\nN(%)*"))%>%
  add_footer_lines(values="* Denominator of the percentage is no. of participants who were expected to attend the visit within the visit window.\nNA= Not applicable")



myft_table5b<-flextable(tab5b)%>%
  theme_box()%>%
  bg(bg="grey70", part="header")%>%
  font(fontname = "Arial", part="all")%>%
  fontsize(size=10, part="all")%>%
  
  width(j=1:2,width=c(3,2))%>%
  align(j=1,align="left", part="all")%>%
  align(j=2,align="center", part="all")%>%
  set_header_labels(values=list(Visit = " ",
                                All = "All\nN(%)*"))%>%
  add_footer_lines(values="* Denominator of the percentage is no. of participants who were expected to attend the visit within the visit window.\nNA= Not applicable")



myft_table5c<-flextable(tab5c)%>%
  theme_box()%>%
  bg(bg="grey70", part="header")%>%
  font(fontname = "Arial", part="all")%>%
  fontsize(size=10, part="all")%>%
  
  width(j=1:2,width=c(3,2))%>%
  align(j=1,align="left", part="all")%>%
  align(j=2,align="center", part="all")%>%
  set_header_labels(values=list(Visit = " ",
                                All = "All\nN(%)*"))%>%
  add_footer_lines(values="* Denominator of the percentage is no. of participants who were expected to attend the visit within the visit window.\nNA= Not applicable")

