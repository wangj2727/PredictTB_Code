
############################
###
### Overall
###
#############################

# Week4 early completion criteria
plate3<-read.table(paste0(indir_Datafax,"plate","003",".dat"),sep="|",header=T,stringsAsFactors = F)
names(plate3)<-gsub(paste0("[_]?",'_003'),"",names(plate3))
plate3<-plate3%>%
  filter(VISITNUM==40)%>%
  mutate(YN=ifelse(WEEK4_YN=="Yes",1,
                   ifelse(WEEK4_YN=="No",2,NA)))%>%
  select(SUBJID,YN)%>%
  rename(WEEK4_YN=YN)


# Week16 early completion criteria
plate4<-read.table(paste0(indir_Datafax,"plate","004",".dat"),sep="|",header=T,stringsAsFactors = F)
names(plate4)<-gsub(paste0("[_]?",'_004'),"",names(plate4))
plate4<-plate4%>%
  mutate(CCORRES_Week16 = ifelse(SUBJID==15002, "No",CCORRES_Week16))%>%
  filter(VISITNUM==160)%>%
  mutate(Week16_xpert=ifelse(CCORRES_Week16=="Yes",1,
                             ifelse(CCORRES_Week16=="No",2,NA)),
         Week16_Adhere=ifelse(CCORRES_Week16MinDose=="Yes",1,
                              ifelse(CCORRES_Week16MinDose=="No",2,NA)))%>%
  select(SUBJID,Week16_xpert,Week16_Adhere, RNDMCODE)


# Day0 early completion criteria (Base_YN: 1=low,2=High)
plate2<-read.table(paste0(indir_Datafax,"plate","002",".dat"),sep="|",header=T,stringsAsFactors = F)
names(plate2)<-gsub(paste0("[_]?",'_002'),"",names(plate2))
plate2<-plate2%>%
  filter(VISITNUM==0)%>%
  mutate(YN=ifelse(BASE_YN=="Yes",1,
                   ifelse(BASE_YN=="No",2,NA)))%>%
  select(SUBJID,YN,VISITNUM)%>%
  rename(BASE_YN=YN)%>%
  left_join(plate3,by="SUBJID")%>%
  left_join(plate4,by="SUBJID")

plate2<-plate2%>%
  filter(!is.na(BASE_YN))%>%
  mutate(AfterWeek4_YN=ifelse(BASE_YN==2,2,
                              ifelse(BASE_YN==1 & !is.na(WEEK4_YN) & WEEK4_YN==2,2,
                                     ifelse(BASE_YN==1 & is.na(WEEK4_YN),0,1))),
         AfterWeek16_YN=ifelse(BASE_YN==1 & !is.na(WEEK4_YN) & WEEK4_YN==2,2.5,
                               ifelse(AfterWeek4_YN==2,2,
                                      ifelse(AfterWeek4_YN==0,0,
                                             ifelse(AfterWeek4_YN==1 & !is.na(Week16_xpert) & Week16_xpert==2 & !is.na(Week16_Adhere) & Week16_Adhere==1,2.1,
                                                    ifelse(AfterWeek4_YN==1 & !is.na(Week16_Adhere) & Week16_Adhere==2 & !is.na(Week16_xpert) & Week16_xpert==1,2.2,
                                                           ifelse(AfterWeek4_YN==1 & !is.na(Week16_xpert) & Week16_xpert==2 & !is.na(Week16_Adhere) & Week16_Adhere==2,2.3,
                                                                  ifelse(AfterWeek4_YN==1 & !is.na(Week16_xpert) & Week16_xpert==1 & !is.na(Week16_Adhere) & Week16_Adhere==1,1,0))))))))



plate2<-plate2%>%
  filter(SUBJID %in% INREPORT$SUBJID)%>%
  mutate(Week4=ifelse(AfterWeek4_YN==2,2,1),
         Week4=ifelse(SUBJID %in% c(14130, 15075,13039), 2, Week4), 
         Week16=ifelse(AfterWeek16_YN>=2,2,
                       ifelse(AfterWeek16_YN==1,0,
                              ifelse(AfterWeek16_YN==0,1,AfterWeek16_YN))),
         Week16=ifelse(SUBJID %in% c(14067, 14130, 15075,13039),2,Week16))%>%
  mutate(Week16=ifelse(Week16==0 & (is.na(RNDMCODE) | RNDMCODE==""),1,Week16))





Base<-as.data.frame(table(plate2$BASE_YN))%>%
  bind_cols(as.data.frame(round(prop.table(table(plate2$BASE_YN))*100,1)))%>%
  mutate(Pct=paste0(`Freq...2`,"(",`Freq...4`,"%)"))%>%
  select(Var1=`Var1...1`,Pct)%>%
  mutate(Var1=as.character(Var1))

Week4<-as.data.frame(table(plate2$Week4))%>%
  bind_cols(as.data.frame(round(prop.table(table(plate2$Week4))*100,1)))%>%
  mutate(Pct=paste0(`Freq...2`,"(",`Freq...4`,"%)"))%>%
  select(Var1=`Var1...1`,Pct)%>%
  mutate(Var1=as.character(Var1))

Week16<-as.data.frame(table(plate2$Week16))%>%
  bind_cols(as.data.frame(round(prop.table(table(plate2$Week16))*100,1)))%>%
  mutate(Pct=paste0(`Freq...2`,"(",`Freq...4`,"%)"))%>%
  select(Var1=`Var1...1`,Pct)%>%
  mutate(Var1=as.character(Var1))


########## baseline image not available yet
NotBaseScan<-INREPORT%>%
  select(SUBJID)%>%
  left_join(plate2,by="SUBJID")%>%
  filter(is.na(BASE_YN))




tab6.1<-Base%>%
  full_join(Week4, by="Var1")%>%
  full_join(Week16, by="Var1")%>%
  column_to_rownames(var="Var1")%>%
  t()%>%
  as.data.frame()%>%
  mutate(Group=c("Baseline radiographic criteria",
                 "Week 4 and baseline radiographic criteria combined",
                 "Week 16 Gene Xpert Cycle threshold, adherence, week4 and baseline criteria combined"))%>%
  select(Group, `High Risk: Arm A`=`2`, `Pending Arm Assignment`=`1`, "Low Risk: Arms B/C"=`0`)%>%
  mutate(`Low Risk: Arms B/C`=as.character(`Low Risk: Arms B/C`),
         `Low Risk: Arms B/C`=ifelse(is.na(`Low Risk: Arms B/C`),"NA",`Low Risk: Arms B/C`))


myft_table6.1a<-flextable(tab6.1)%>%
  theme_box()%>%
  bg(bg="grey70", part="header")%>%
  font(fontname = "Arial", part="all")%>%
  fontsize(size=10, part="all")%>%
  
  width(j=1:4,width=c(3.5,2,2,2))%>%
  align(j=2:4,align="center", part="all")%>%
  align(j=1,align="left", part="all")%>%
  set_header_labels(values=list(Group = "Criteria for treatment shortening allocation",
                                `High Risk: Arm A` = "High Risk: Arm A\nN (%)",
                                `Pending Arm Assignment` = "Pending Assignment\n(N)",
                                `Low Risk: Arms B/C` = "Low Risk: Arms B/C\nN (%)"))%>%
  
  add_footer_lines(values=paste0("(1) Randomization at week 16 is predicated on if participant meets the radiographic criteria at baseline and week 4 and then meets the bacterial load and adherence criteria at week 16.\n(2) Baseline radiographic criteria to be considered for treatment shortening: No total lung collapses of a single side; AND No pleural effusion; AND No single cavity air volume on CT scan >30 mL; AND (CT scan hard volume <200mL (> 100 HU density) OR PET total activity <1500 units).\n(3) Week 4 radiographic criteria to be considered for treatment shortening: All individual cavities decrease by >20% (unless cavity <2mL); AND (CT scan hard volume does not increase by >10% unless the increase is <5mL OR PET total activity does not increase by >30% unless the increase is <50 units).\n(4) Week 16 Bacterial load criterion to be considered for treatment shortening: Week 16 Xpert cycle threshold >= 28.\n(5) Week 16 Adherence criterion to be considered for treatment shortening: Minimum of 100 doses received.\n(6)For Week 16 visit:\n    ",
                                 length(plate2$AfterWeek16_YN[!is.na(plate2$AfterWeek16_YN)&plate2$AfterWeek16_YN==2.2]), 
                                 " participant moved to Arm A because of adherence only;\n    ", 
                                 length(plate2$AfterWeek16_YN[!is.na(plate2$AfterWeek16_YN)&plate2$AfterWeek16_YN==2.1]), 
                                 " participants moved to Arm A because of Xpert CT only;\n    ", 
                                 length(plate2$AfterWeek16_YN[!is.na(plate2$AfterWeek16_YN)&plate2$AfterWeek16_YN==2.3]), 
                                 " participants moved to Arm A because of both adherence and Xpert CT.\n    ",
                                 nrow(plate2[plate2$SUBJID==14067,]),
                                 " participant moved to Arm A because of pregnancy;\n(7) Baseline imaging not yet completed for\n    ", 
                                 nrow(NotBaseScan)," participants\n(8) Three participants moved to Arm A due to Week 16 Xpert value<30.\n(9) Three participants moved to Arm A at Week4 due to pregnancy."))


############################
###
### SA
###
#############################
plate2_SA<-plate2%>%
  filter(SUBJID<20000)

Base<-as.data.frame(table(plate2_SA$BASE_YN))%>%
  bind_cols(as.data.frame(round(prop.table(table(plate2_SA$BASE_YN))*100,1)))%>%
  mutate(Pct=paste0(`Freq...2`,"(",`Freq...4`,"%)"))%>%
  select(Var1=`Var1...1`,Pct)%>%
  mutate(Var1=as.character(Var1))

Week4<-as.data.frame(table(plate2_SA$Week4))%>%
  bind_cols(as.data.frame(round(prop.table(table(plate2_SA$Week4))*100,1)))%>%
  mutate(Pct=paste0(`Freq...2`,"(",`Freq...4`,"%)"))%>%
  select(Var1=`Var1...1`,Pct)%>%
  mutate(Var1=as.character(Var1))

Week16<-as.data.frame(table(plate2_SA$Week16))%>%
  bind_cols(as.data.frame(round(prop.table(table(plate2_SA$Week16))*100,1)))%>%
  mutate(Pct=paste0(`Freq...2`,"(",`Freq...4`,"%)"))%>%
  select(Var1=`Var1...1`,Pct)%>%
  mutate(Var1=as.character(Var1))

########## baseline image not available yet
NotBaseScan_SA<-NotBaseScan%>%
  filter(SUBJID<20000)


tab6.2<-Base%>%
  full_join(Week4, by="Var1")%>%
  full_join(Week16, by="Var1")%>%
  column_to_rownames(var="Var1")%>%
  t()%>%
  as.data.frame()%>%
  mutate(Group=c("Baseline radiographic criteria",
                 "Week 4 and baseline radiographic criteria combined",
                 "Week 16 Gene Xpert Cycle threshold, adherence, week4 and baseline criteria combined"))%>%
  select(Group, `High Risk: Arm A`=`2`, `Pending Arm Assignment`=`1`, "Low Risk: Arms B/C"=`0`)%>%
  mutate(`Low Risk: Arms B/C`=as.character(`Low Risk: Arms B/C`),
         `Low Risk: Arms B/C`=ifelse(is.na(`Low Risk: Arms B/C`),"NA",`Low Risk: Arms B/C`))

myft_table6.1b<-flextable(tab6.2)%>%
  theme_box()%>%
  bg(bg="grey70", part="header")%>%
  font(fontname = "Arial", part="all")%>%
  fontsize(size=10, part="all")%>%
  
  width(j=1:4,width=c(3.5,2,2,2))%>%
  align(j=2:4,align="center", part="all")%>%
  align(j=1,align="left", part="all")%>%
  set_header_labels(values=list(Group = "Criteria for treatment shortening allocation",
                                `High Risk: Arm A` = "High Risk: Arm A\nN (%)",
                                `Pending Arm Assignment` = "Pending Assignment\n(N)",
                                `Low Risk: Arms B/C` = "Low Risk: Arms B/C\nN (%)"))%>%
  
  add_footer_lines(values=paste0("(1) Randomization at week 16 is predicated on if participant meets the radiographic criteria at baseline and week 4 and then meets the bacterial load and adherence criteria at week 16.\n(2) Baseline radiographic criteria to be considered for treatment shortening: No total lung collapses of a single side; AND No pleural effusion; AND No single cavity air volume on CT scan >30 mL; AND (CT scan hard volume <200mL (> 100 HU density) OR PET total activity <1500 units).\n(3) Week 4 radiographic criteria to be considered for treatment shortening: All individual cavities decrease by >20% (unless cavity <2mL); AND (CT scan hard volume does not increase by >10% unless the increase is <5mL OR PET total activity does not increase by >30% unless the increase is <50 units).\n(4) Week 16 Bacterial load criterion to be considered for treatment shortening: Week 16 Xpert cycle threshold >= 28.\n(5) Week 16 Adherence criterion to be considered for treatment shortening: Minimum of 100 doses received.\n(6)For Week 16 visit:\n    ",
                                 length(plate2_SA$AfterWeek16_YN[!is.na(plate2_SA$AfterWeek16_YN)&plate2_SA$AfterWeek16_YN==2.2]), 
                                 " participant moved to Arm A because of adherence only;\n    ", 
                                 length(plate2_SA$AfterWeek16_YN[!is.na(plate2_SA$AfterWeek16_YN)&plate2_SA$AfterWeek16_YN==2.1]), 
                                 " participants moved to Arm A because of Xpert CT only;\n    ", 
                                 length(plate2_SA$AfterWeek16_YN[!is.na(plate2_SA$AfterWeek16_YN)&plate2_SA$AfterWeek16_YN==2.3]), 
                                 " participants moved to Arm A because of both adherence and Xpert CT.\n    ",
                                 nrow(plate2_SA[plate2_SA$SUBJID==14067,]),
                                 " participant moved to Arm A because of pregnancy;\n(7) Baseline imaging not yet completed for\n    ", 
                                 nrow(NotBaseScan_SA)," participants\n(8) Three participants moved to Arm A due to Week 16 Xpert value<30.\n(9) Three participants moved to Arm A at Week4 due to pregnancy."))


############################
###
### China
###
#############################
plate2_CHN<-plate2%>%
  filter(SUBJID>20000)


Base<-as.data.frame(table(plate2_CHN$BASE_YN))%>%
  bind_cols(as.data.frame(round(prop.table(table(plate2_CHN$BASE_YN))*100,1)))%>%
  mutate(Pct=paste0(`Freq...2`,"(",`Freq...4`,"%)"))%>%
  select(Var1=`Var1...1`,Pct)%>%
  mutate(Var1=as.character(Var1))

Week4<-as.data.frame(table(plate2_CHN$Week4))%>%
  bind_cols(as.data.frame(round(prop.table(table(plate2_CHN$Week4))*100,1)))%>%
  mutate(Pct=paste0(`Freq...2`,"(",`Freq...4`,"%)"))%>%
  select(Var1=`Var1...1`,Pct)%>%
  mutate(Var1=as.character(Var1))

Week16<-as.data.frame(table(plate2_CHN$Week16))%>%
  bind_cols(as.data.frame(round(prop.table(table(plate2_CHN$Week16))*100,1)))%>%
  mutate(Pct=paste0(`Freq...2`,"(",`Freq...4`,"%)"))%>%
  select(Var1=`Var1...1`,Pct)%>%
  mutate(Var1=as.character(Var1))

########## baseline image not available yet
NotBaseScan_CHN<-NotBaseScan%>%
  filter(SUBJID>20000)


tab6.3<-Base%>%
  full_join(Week4, by="Var1")%>%
  full_join(Week16, by="Var1")%>%
  column_to_rownames(var="Var1")%>%
  t()%>%
  as.data.frame()%>%
  mutate(Group=c("Baseline radiographic criteria",
                 "Week 4 and baseline radiographic criteria combined",
                 "Week 16 Gene Xpert Cycle threshold, adherence, week4 and baseline criteria combined"))%>%
  select(Group, `High Risk: Arm A`=`2`, `Pending Arm Assignment`=`1`, `Low Risk: Arms B/C`=`0`)%>%
  mutate(`Low Risk: Arms B/C`=as.character(`Low Risk: Arms B/C`),
         `Low Risk: Arms B/C`=ifelse(is.na(`Low Risk: Arms B/C`),"NA",`Low Risk: Arms B/C`))


myft_table6.1c<-flextable(tab6.3)%>%
  theme_box()%>%
  bg(bg="grey70", part="header")%>%
  font(fontname = "Arial", part="all")%>%
  fontsize(size=10, part="all")%>%
  
  width(j=1:4,width=c(3.5,2,2,2))%>%
  align(j=2:4,align="center", part="all")%>%
  align(j=1,align="left", part="all")%>%
  set_header_labels(values=list(Group = "Criteria for treatment shortening allocation",
                                `High Risk: Arm A` = "High Risk: Arm A\nN (%)",
                                `Pending Arm Assignment` = "Pending Assignment\n(N)",
                                `Low Risk: Arms B/C` = "Low Risk: Arms B/C\nN (%)"))%>%
  
  add_footer_lines(values=paste0("(1) Randomization at week 16 is predicated on if participant meets the radiographic criteria at baseline and week 4 and then meets the bacterial load and adherence criteria at week 16.\n(2) Baseline radiographic criteria to be considered for treatment shortening: No total lung collapses of a single side; AND No pleural effusion; AND No single cavity air volume on CT scan >30 mL; AND (CT scan hard volume <200mL (> 100 HU density) OR PET total activity <1500 units).\n(3) Week 4 radiographic criteria to be considered for treatment shortening: All individual cavities decrease by >20% (unless cavity <2mL); AND (CT scan hard volume does not increase by >10% unless the increase is <5mL OR PET total activity does not increase by >30% unless the increase is <50 units).\n(4) Week 16 Bacterial load criterion to be considered for treatment shortening: Week 16 Xpert cycle threshold >= 28.\n(5) Week 16 Adherence criterion to be considered for treatment shortening: Minimum of 100 doses received.\n(6)For Week 16 visit:\n    ",
                                 length(plate2_CHN$AfterWeek16_YN[!is.na(plate2_CHN$AfterWeek16_YN)&plate2_CHN$AfterWeek16_YN==2.2]), 
                                 " participant moved to Arm A because of adherence only;\n    ", 
                                 length(plate2_CHN$AfterWeek16_YN[!is.na(plate2_CHN$AfterWeek16_YN)&plate2_CHN$AfterWeek16_YN==2.1]), 
                                 " participants moved to Arm A because of Xpert CT only;\n    ", 
                                 length(plate2_CHN$AfterWeek16_YN[!is.na(plate2_CHN$AfterWeek16_YN)&plate2_CHN$AfterWeek16_YN==2.3]), 
                                 "participants moved to Arm A because of both adherence and Xpert CT;\n(7) Baseline imaging not yet completed for\n    ", 
                                 nrow(NotBaseScan_CHN)," participants."))

