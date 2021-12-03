


###################
### Age and Sex
###################
plate1<-myInputData("001")%>%
  mutate(Consent=as.Date(RFICDTC,format="%d/%B/%Y"),
         Birthday=as.Date(BRTHDAT,format="%d/%B/%Y"),
         Age=as.numeric(Consent-Birthday)/365.25)%>%
  select(SUBJID,Age, SEX, RACE)


###################
### Weight and BMI
###################
plate8<-myInputData("008")%>%
  filter(VISITNUM==90)%>%
  mutate(BMI=as.numeric(VSORRES_WEIGHT)/(as.numeric(VSORRES_HEIGHT)/100)^2,
         Weight=as.numeric(VSORRES_WEIGHT))%>%
  select(SUBJID,Weight,BMI)

### Smoking
plate6<-myInputData("006")%>%
  select(SUBJID,SCORRES_CH_Tobacco,MHOCCUR_TreatedTB,SCORRES_CH_SmokingOngoing,SCORRES_DurationSmoking)%>%
  mutate(Smoke1=ifelse(MHOCCUR_TreatedTB=="Yes","Yes","No"),
         Smoke2=ifelse(SCORRES_CH_SmokingOngoing=="Yes","Yes","No"),
         Smoke3=ifelse(SCORRES_CH_SmokingOngoing=="No" & SCORRES_CH_Tobacco=="Yes","Yes","No"))%>%
  select(SUBJID,Smoke1,Smoke2,Smoke3,SCORRES_DurationSmoking)

# smoke duration has only been recorded for participant ever has smoked tobbaco

###################
### TTD
###################
plate12<-myInputData("012")%>%
  filter(VISITNUM==0 | VISITNUM==90)%>%
  filter(MBORRES_CH_MGIT %in% c("Negative","Positive"))%>%
  select(SUBJID,VISITNUM,MBDTC_DD_MGIT_DD, MBDTC_DD_MGIT_HH)%>%
  mutate(HH=ifelse(!is.na(MBDTC_DD_MGIT_HH) & MBDTC_DD_MGIT_HH<12,0,
                   ifelse(!is.na(MBDTC_DD_MGIT_HH) & MBDTC_DD_MGIT_HH>=12,1,0)),
         MBDTC_DD_MGIT_DD=MBDTC_DD_MGIT_DD + HH)%>%
  select(-HH, -MBDTC_DD_MGIT_HH)%>%
  spread(VISITNUM,MBDTC_DD_MGIT_DD)%>%
  mutate(TTD=ifelse(is.na(`0`),`90`,`0`))%>%
  select(SUBJID,TTD)


###################
### Xpert Week0
###################
plate13<-myInputData("013")%>%
  filter(VISITNUM==0 | VISITNUM==90)%>%
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
plate13<-plate13%>%
  select(SUBJID,VISITNUM,CT)%>%
  spread(VISITNUM,CT)%>%
  mutate(CT=ifelse(is.na(`0`),`90`,`0`))%>%
  select(SUBJID,CT)


###################
### Baseline Cavity Value
###################
plate203<-myInputData("203")%>%
  select(SUBJID,RIRISK_R3=RIRISK,RIORRES_CAVITY1, RIORRES_CAVITY2, RIORRES_CAVITY3, RIORRES_CAVITY4, RIORRES_CAVITY5)%>%
  mutate(Total=rowSums(select(., 'RIORRES_CAVITY1', 'RIORRES_CAVITY2', 'RIORRES_CAVITY3', 'RIORRES_CAVITY4', 'RIORRES_CAVITY5'),na.rm=TRUE),
         MaxN=apply(select(., 'RIORRES_CAVITY1', 'RIORRES_CAVITY2', 'RIORRES_CAVITY3', 'RIORRES_CAVITY4', 'RIORRES_CAVITY5'),1,function(x) sum(!is.na(x))))%>%
  select(SUBJID,RIRISK_R3,Total_3=Total,MaxN_3=MaxN)


plate202<-myInputData("202")%>%
  select(SUBJID,RIRISK_R2=RIRISK,RIORRES_CAVITY1, RIORRES_CAVITY2, RIORRES_CAVITY3, RIORRES_CAVITY4, RIORRES_CAVITY5)%>%
  mutate(Total=rowSums(select(., 'RIORRES_CAVITY1', 'RIORRES_CAVITY2', 'RIORRES_CAVITY3', 'RIORRES_CAVITY4', 'RIORRES_CAVITY5'),na.rm=TRUE),
         MaxN=apply(select(., 'RIORRES_CAVITY1', 'RIORRES_CAVITY2', 'RIORRES_CAVITY3', 'RIORRES_CAVITY4', 'RIORRES_CAVITY5'),1,function(x) sum(!is.na(x))))%>%
  select(SUBJID,RIRISK_R2,Total_2=Total,MaxN_2=MaxN)


plate201<-myInputData("201")%>%
  select(SUBJID,RIRISK_R1=RIRISK,RIORRES_CAVITY1, RIORRES_CAVITY2, RIORRES_CAVITY3, RIORRES_CAVITY4, RIORRES_CAVITY5)%>%
  mutate(Total=rowSums(select(., 'RIORRES_CAVITY1', 'RIORRES_CAVITY2', 'RIORRES_CAVITY3', 'RIORRES_CAVITY4', 'RIORRES_CAVITY5'),na.rm=TRUE),
         MaxN=apply(select(., 'RIORRES_CAVITY1', 'RIORRES_CAVITY2', 'RIORRES_CAVITY3', 'RIORRES_CAVITY4', 'RIORRES_CAVITY5'),1,function(x) sum(!is.na(x))))%>%
  select(SUBJID,RIRISK_R1,Total_1=Total,MaxN_1=MaxN)



Cavity<-plate201%>%
  full_join(plate202,by="SUBJID")%>%
  full_join(plate203,by="SUBJID")%>%
  filter(!is.na(RIRISK_R1) & !is.na(RIRISK_R2))%>%
  filter(!(RIRISK_R1!=RIRISK_R2 & is.na(RIRISK_R3)))%>%
  mutate(TotalVol=ifelse(RIRISK_R1==RIRISK_R2, rowMeans(select(., "Total_1","Total_2"),na.rm=TRUE),
                         ifelse(!is.na(RIRISK_R3) & RIRISK_R1==RIRISK_R3, rowMeans(select(., "Total_1","Total_3"),na.rm=TRUE),
                                ifelse(!is.na(RIRISK_R3) & RIRISK_R2==RIRISK_R3, rowMeans(select(., "Total_2","Total_3"),na.rm=TRUE),NA))))%>%
  mutate(MaxCount=ifelse(RIRISK_R1==RIRISK_R2, apply(select(., "MaxN_1","MaxN_2"),1,function(x) max(x,na.rm=TRUE)),
                         ifelse(!is.na(RIRISK_R3) & RIRISK_R1==RIRISK_R3, apply(select(., "MaxN_1","MaxN_3"),1,function(x) max(x,na.rm=TRUE)),
                                ifelse(!is.na(RIRISK_R3) & RIRISK_R2==RIRISK_R3, apply(select(., "MaxN_2","MaxN_3"),1,function(x) max(x,na.rm=TRUE)),NA))))%>%
  mutate(MaxCount_Char=ifelse(MaxCount==0,"NZ0",
                              ifelse(MaxCount==1,"NZ1",
                                     ifelse(MaxCount==2,"NZ2",
                                            ifelse(MaxCount==3,"NZ3",
                                                   ifelse(MaxCount>=4,"NZ4",NA))))))%>%
  select(SUBJID,TotalVol,MaxCount_Char)



#########################
##
##  analysis-ready data
##
########################

combineData<-INREPORT%>%
  left_join(plate1,by="SUBJID")%>%
  left_join(plate6,by="SUBJID")%>%
  left_join(plate8,by="SUBJID")%>%
  left_join(plate12,by="SUBJID")%>%
  left_join(plate13,by="SUBJID")%>%
  left_join(Cavity,by="SUBJID")%>%
  mutate(SCORRES_DurationSmoking=as.numeric(SCORRES_DurationSmoking))

final<-combineData

createSheet(wb, name = "Table2a_PID")
writeWorksheet(wb, combineData, sheet = "Table2a_PID")



myCategory<-function(Varname,ARM,CatVar){
  
  dta<-data.frame(ARM,CatVar)
  ArmLevel<-unique(ARM)
  
  if(class(CatVar)=="character"){
    CatLevel<-unique(CatVar[!is.na(CatVar)])
  }else if(class(CatVar)=="factor"){
    CatLevel<-levels(CatVar[!is.na(CatVar)])
  }
  N<-length(CatLevel)
  
  outdta<-NULL
  for(i in 1:N){
    
    CatLevelNoquote<-noquote(CatLevel[i])
    
    leveldta<-dta%>%
      group_by(ARM)%>%
      summarise(num=length(CatVar[!is.na(CatVar)&CatVar==CatLevel[i]]),Allnum=length(CatVar[!is.na(CatVar)]),
                CatLevelNoquote=paste0(num," (",formatC(num*100/Allnum, digits=1, format="f", drop0trailing = FALSE),"%)"))%>%
      select(ARM,CatLevelNoquote)%>%
      spread(ARM,CatLevelNoquote)%>%
      bind_cols(Stat=CatLevel[i])%>%
      bind_cols(Overall=paste0(length(dta$CatVar[!is.na(dta$CatVar)&dta$CatVar==CatLevel[i]])," (",
                               formatC(length(dta$CatVar[!is.na(dta$CatVar)&dta$CatVar==CatLevel[i]])*100/length(dta$CatVar[!is.na(dta$CatVar)]),
                                       digits=1, format="f", drop0trailing = FALSE),"%)"))   
    
    outdta<-bind_rows(outdta,leveldta)
  }
  
  LabelLine<-data.frame(Stat=Varname,ArmLevel,Holder="",Overall="",
                        stringsAsFactors = FALSE)%>%
    spread(ArmLevel,Holder)%>%
    bind_rows(outdta)
  return(LabelLine)
}




myContinuous<-function(Varname,ARM,ConVar,StatType="Mean (SD)"){
  
  dta<-data.frame(ARM,ConVar)
  ArmLevel<-unique(ARM)
  
  if(StatType=="Mean (Range)"){
    
    leveldta<-dta%>%
      group_by(ARM)%>%
      summarise(mean=sprintf(mean(ConVar,na.rm=TRUE),fmt="%#.1f"),min=sprintf(as.numeric(min(ConVar,na.rm=TRUE)),fmt="%#.1f"),
                max=sprintf(as.numeric(max(ConVar,na.rm=TRUE)),fmt="%#.1f"),
                ConStat=paste0(mean," (",min,"-",max,")"))%>%
      select(ARM,ConStat)%>%
      spread(ARM,ConStat)%>%
      bind_cols(Stat=StatType)%>%
      bind_cols(Overall=paste0(sprintf(mean(dta$ConVar,na.rm=TRUE),fmt="%#.1f")," (",
                               sprintf(as.numeric(min(dta$ConVar,na.rm=TRUE)),fmt="%#.1f"),"-",
                               sprintf(as.numeric(max(ConVar,na.rm=TRUE)),fmt="%#.1f"),")"))  
  }else if(StatType=="Mean (SD)"){
    
    leveldta<-dta%>%
      group_by(ARM)%>%
      summarise(mean=sprintf(mean(ConVar,na.rm=TRUE),fmt="%#.1f"),sd=sprintf(sd(ConVar,na.rm=TRUE),fmt="%#.1f"),
                ConStat=paste0(mean," (",sd,")"))%>%
      select(ARM,ConStat)%>%
      spread(ARM,ConStat)%>%
      bind_cols(Stat=StatType)%>%
      bind_cols(Overall=paste0(sprintf(mean(dta$ConVar,na.rm=TRUE),fmt="%#.1f")," (",
                               sd=sprintf(sd(dta$ConVar,na.rm=TRUE),fmt="%#.1f"),")"))
  }
  
  LabelLine<-data.frame(Stat=Varname,ArmLevel,Holder="",Overall="",
                        stringsAsFactors = FALSE)%>%
    spread(ArmLevel,Holder)%>%
    bind_rows(leveldta)
  
  return(LabelLine)
}


####################################
###
###  Table 2a overall
###
####################################
Sex<-myCategory("Sex",combineData$Arm,combineData$SEX)%>%filter(Stat=="Male")
Age<-myContinuous("Age",combineData$Arm,combineData$Age,"Mean (Range)")%>%filter(Stat=="Mean (Range)")
Weight<-myContinuous("Weight (kg)",combineData$Arm,combineData$Weight,"Mean (SD)")%>%filter(Stat=="Mean (SD)")
BMI<-myContinuous("BMI",combineData$Arm, combineData$BMI)%>%filter(Stat=="Mean (SD)")
TB_Episodes<-myCategory("TB_Episodes",combineData$Arm, combineData$Smoke1)%>%filter(Stat=="Yes")
Current_Smoker<-myCategory("Current_Smoker",combineData$Arm, combineData$Smoke2)%>%filter(Stat=="Yes")
Previous_Smoker<-myCategory("Previous_Smoker",combineData$Arm, combineData$Smoke3)%>%filter(Stat=="Yes")
Duration<-myContinuous("SmokeDuration",combineData$Arm, combineData$SCORRES_DurationSmoking,"Mean (Range)")%>%filter(Stat=="Mean (Range)")
NumCav<-myCategory("NumCav",combineData$Arm, combineData$MaxCount_Char)%>%arrange(Stat)
TotalVol<-myContinuous("TotalVol",combineData$Arm,combineData$TotalVol)%>%filter(Stat=="Mean (SD)")
CT<-myContinuous("CT",combineData$Arm,combineData$CT)%>%filter(Stat=="Mean (SD)")
TTD<-myContinuous("TTD",combineData$Arm, combineData$TTD)%>%filter(Stat=="Mean (SD)")
Race<-myCategory("Race",combineData$Arm, combineData$RACE)%>%filter(Stat!="Race")%>%
  bind_rows(c(Stat="American Indian or Alaska Native (N (%))",Overall="0 (0.0%)",`Arm A`="0 (0.0%)",`Arm B & Arm C`="0 (0.0%)",`Pending Assignment`="0 (0.0%)"))%>%
  bind_rows(c(Stat="Native Hawaiian or Other Pacific Islander",Overall="0 (0.0%)",`Arm A`="0 (0.0%)",`Arm B & Arm C`="0 (0.0%)",`Pending Assignment`="0 (0.0%)"))%>%
  bind_rows(c(Stat="White",Overall="0 (0.0%)",`Arm A`="0 (0.0%)",`Arm B & Arm C`="0 (0.0%)",`Pending Assignment`="0 (0.0%)"))%>%
  bind_rows(c(Stat="Unknown or Not Reported",Overall="0 (0.0%)",`Arm A`="0 (0.0%)",`Arm B & Arm C`="0 (0.0%)",`Pending Assignment`="0 (0.0%)"))%>%
  bind_rows(c(Stat="Other",Overall="0 (0.0%)",`Arm A`="0 (0.0%)",`Arm B & Arm C`="0 (0.0%)",`Pending Assignment`="0 (0.0%)"))%>%
  .[c(3,2,4,1,5,6,7),]


Report<-c(Stat=" ",Overall=" ",
          `Arm A`=" ",`Arm B & Arm C`=" ",`Pending Assignment`="")%>%
  bind_rows(Sex)%>%
  bind_rows(Age)%>%
  bind_rows(Weight)%>%
  bind_rows(BMI)%>%
  bind_rows(TB_Episodes)%>%
  bind_rows(Current_Smoker)%>%
  bind_rows(Previous_Smoker)%>%
  bind_rows(Duration)%>%
  bind_rows(NumCav)%>%
  bind_rows(TotalVol)%>%
  bind_rows(CT)%>%
  bind_rows(TTD)%>%
  bind_rows(Race)%>%
  mutate(Stat=ifelse(Stat=="Yes" | str_detect(Stat,"NZ"),"N (%)",
                     ifelse(Stat=="Male","Male N (%)", 
                            ifelse(Stat=="NumCav"," ",Stat))))%>%
  bind_cols(data.frame(VarName=c(" ","Sex","Age","Weight (kg)","BMI","# of participants with at least one previous TB episode not within the past 3 years",
                                 "Current smoker","Previous smoker - those who are not current smokers but report a history of smoking",
                                 "Duration of smoking (years)","Number of cavities (>2 mL)","None","1","2","3",">=4","Total cavity volume (>2 mL)",
                                 "Xpert CT - Week 0","Time to positivity on MGIT (days) - Week 0","Race","","","","","","")))%>%
  .[,c(6,1,2,5,3,4)]


myft_table2a<-flextable(Report[-1,])%>%
  theme_box()%>%
  bg(bg="grey70", part="header")%>%
  font(fontname = "Arial", part="all")%>%
  fontsize(size=10, part="all")%>%
  merge_at(i=1,j=1:2, part="header")%>%
  width(j=1:6,width=c(3,1.5,1.5,1.5,1.5,1.5))%>%
  align(j=1:2,align="left", part="body")%>%
  align(j=3:6,align="center", part="all")%>%
  set_header_labels(values=list(VarName = " ",
                                Overall= paste0("All\n(n=",nrow(combineData),")"),
                                `Pending Assignment` = paste0("Pending Assignment to A or B, C\n(n=",nrow(combineData[combineData$Arm=="Pending Assignment",]),")"),
                                `Arm A` = paste0("Arm A\n(n=",nrow(combineData[combineData$Arm=="Arm A",]),")"),
                                `Arm B & Arm C` = paste0("Arm B & Arm C\n(n=",nrow(combineData[combineData$Arm=="Arm B & Arm C",]),")")))


####################################
###
###  Table 2b SA
###
####################################

combineData<-final%>%
  filter(SUBJID<20000)

Sex<-myCategory("Sex",combineData$Arm,combineData$SEX)%>%filter(Stat=="Male")
Age<-myContinuous("Age",combineData$Arm,combineData$Age,"Mean (Range)")%>%filter(Stat=="Mean (Range)")
Weight<-myContinuous("Weight (kg)",combineData$Arm,combineData$Weight,"Mean (SD)")%>%filter(Stat=="Mean (SD)")
BMI<-myContinuous("BMI",combineData$Arm, combineData$BMI)%>%filter(Stat=="Mean (SD)")
TB_Episodes<-myCategory("TB_Episodes",combineData$Arm, combineData$Smoke1)%>%filter(Stat=="Yes")
Current_Smoker<-myCategory("Current_Smoker",combineData$Arm, combineData$Smoke2)%>%filter(Stat=="Yes")
Previous_Smoker<-myCategory("Previous_Smoker",combineData$Arm, combineData$Smoke3)%>%filter(Stat=="Yes")
Duration<-myContinuous("SmokeDuration",combineData$Arm, combineData$SCORRES_DurationSmoking,"Mean (Range)")%>%filter(Stat=="Mean (Range)")
NumCav<-myCategory("NumCav",combineData$Arm, combineData$MaxCount_Char)%>%arrange(Stat)
TotalVol<-myContinuous("TotalVol",combineData$Arm,combineData$TotalVol)%>%filter(Stat=="Mean (SD)")
CT<-myContinuous("CT",combineData$Arm,combineData$CT)%>%filter(Stat=="Mean (SD)")
TTD<-myContinuous("TTD",combineData$Arm, combineData$TTD)%>%filter(Stat=="Mean (SD)")
Race<-myCategory("Race",combineData$Arm, combineData$RACE)%>%filter(Stat!="Race")%>%
  bind_rows(c(Stat="American Indian or Alaska Native (N (%))",Overall="0 (0.0%)",`Arm A`="0 (0.0%)",`Arm B & Arm C`="0 (0.0%)",`Pending Assignment`="0 (0.0%)"))%>%
  bind_rows(c(Stat="Asian",Overall="0 (0.0%)",`Arm A`="0 (0.0%)",`Arm B & Arm C`="0 (0.0%)",`Pending Assignment`="0 (0.0%)"))%>%
  bind_rows(c(Stat="Native Hawaiian or Other Pacific Islander",Overall="0 (0.0%)",`Arm A`="0 (0.0%)",`Arm B & Arm C`="0 (0.0%)",`Pending Assignment`="0 (0.0%)"))%>%
  bind_rows(c(Stat="White",Overall="0 (0.0%)",`Arm A`="0 (0.0%)",`Arm B & Arm C`="0 (0.0%)",`Pending Assignment`="0 (0.0%)"))%>%
  bind_rows(c(Stat="Unknown or Not Reported",Overall="0 (0.0%)",`Arm A`="0 (0.0%)",`Arm B & Arm C`="0 (0.0%)",`Pending Assignment`="0 (0.0%)"))%>%
  bind_rows(c(Stat="Other",Overall="0 (0.0%)",`Arm A`="0 (0.0%)",`Arm B & Arm C`="0 (0.0%)",`Pending Assignment`="0 (0.0%)"))%>%
  .[c(2,3,4,1,5,6,7),]


Report<-c(Stat=" ",Overall=" ",
          `Arm A`=" ",`Arm B & Arm C`=" ",`Pending Assignment`="")%>%
  bind_rows(Sex)%>%
  bind_rows(Age)%>%
  bind_rows(Weight)%>%
  bind_rows(BMI)%>%
  bind_rows(TB_Episodes)%>%
  bind_rows(Current_Smoker)%>%
  bind_rows(Previous_Smoker)%>%
  bind_rows(Duration)%>%
  bind_rows(NumCav)%>%
  bind_rows(TotalVol)%>%
  bind_rows(CT)%>%
  bind_rows(TTD)%>%
  bind_rows(Race)%>%
  mutate(Stat=ifelse(Stat=="Yes" | str_detect(Stat,"NZ"),"N (%)",
                     ifelse(Stat=="Male","Male N (%)", 
                            ifelse(Stat=="NumCav"," ",Stat))))%>%
  bind_cols(data.frame(VarName=c(" ","Sex","Age","Weight (kg)","BMI","# of participants with at least one previous TB episode not within the past 3 years",
                                 "Current smoker","Previous smoker - those who are not current smokers but report a history of smoking",
                                 "Duration of smoking (years)","Number of cavities (>2 mL)","None","1","2","3",">=4","Total cavity volume (>2 mL)",
                                 "Xpert CT - Week 0","Time to positivity on MGIT (days) - Week 0","Race","","","","","","")))%>%
  .[,c(6,1,2,5,3,4)]%>%
  mutate(`Pending Assignment` = ifelse(is.na(`Pending Assignment`) & VarName!="Number of cavities (>2 mL)", "0 (0.0%)", `Pending Assignment`))


myft_table2b<-flextable(Report[-1,])%>%
  theme_box()%>%
  bg(bg="grey70", part="header")%>%
  font(fontname = "Arial", part="all")%>%
  fontsize(size=10, part="all")%>%
  merge_at(i=1,j=1:2, part="header")%>%
  width(j=1:6,width=c(3,1.5,1.5,1.5,1.5,1.5))%>%
  align(j=1:2,align="left", part="body")%>%
  align(j=3:6,align="center", part="all")%>%
  set_header_labels(values=list(VarName = " ",
                                Overall= paste0("All\n(n=",nrow(combineData),")"),
                                `Pending Assignment` = paste0("Pending Assignment to A or B, C\n(n=",nrow(combineData[combineData$Arm=="Pending Assignment",]),")"),
                                `Arm A` = paste0("Arm A\n(n=",nrow(combineData[combineData$Arm=="Arm A",]),")"),
                                `Arm B & Arm C` = paste0("Arm B & Arm C\n(n=",nrow(combineData[combineData$Arm=="Arm B & Arm C",]),")")))





####################################
###
###  Table 2c China
###
####################################

combineData<-final%>%
  filter(SUBJID>=20000)

Sex<-myCategory("Sex",combineData$Arm,combineData$SEX)%>%filter(Stat=="Male")
Age<-myContinuous("Age",combineData$Arm,combineData$Age,"Mean (Range)")%>%filter(Stat=="Mean (Range)")
Weight<-myContinuous("Weight (kg)",combineData$Arm,combineData$Weight,"Mean (SD)")%>%filter(Stat=="Mean (SD)")
BMI<-myContinuous("BMI",combineData$Arm, combineData$BMI)%>%filter(Stat=="Mean (SD)")
TB_Episodes<-myCategory("TB_Episodes",combineData$Arm, combineData$Smoke1)%>%filter(Stat=="Yes")
Current_Smoker<-myCategory("Current_Smoker",combineData$Arm, combineData$Smoke2)%>%filter(Stat=="Yes")
Previous_Smoker<-myCategory("Previous_Smoker",combineData$Arm, combineData$Smoke3)%>%filter(Stat=="Yes")
Duration<-myContinuous("SmokeDuration",combineData$Arm, combineData$SCORRES_DurationSmoking,"Mean (Range)")%>%filter(Stat=="Mean (Range)")
NumCav<-myCategory("NumCav",combineData$Arm, combineData$MaxCount_Char)%>%arrange(Stat)
TotalVol<-myContinuous("TotalVol",combineData$Arm,combineData$TotalVol)%>%filter(Stat=="Mean (SD)")
CT<-myContinuous("CT",combineData$Arm,combineData$CT)%>%filter(Stat=="Mean (SD)")
TTD<-myContinuous("TTD",combineData$Arm, combineData$TTD)%>%filter(Stat=="Mean (SD)")
Race<-myCategory("Race",combineData$Arm, combineData$RACE)%>%filter(Stat!="Race")%>%
  bind_rows(c(Stat="Colored or Black",Overall="0 (0.0%)",`Arm A`="0 (0.0%)",`Arm B & Arm C`="0 (0.0%)",`Pending Assignment`="0 (0.0%)"))%>%
  bind_rows(c(Stat="American Indian or Alaska Native (N (%))",Overall="0 (0.0%)",`Arm A`="0 (0.0%)",`Arm B & Arm C`="0 (0.0%)",`Pending Assignment`="0 (0.0%)"))%>%
  bind_rows(c(Stat="Native Hawaiian or Other Pacific Islander",Overall="0 (0.0%)",`Arm A`="0 (0.0%)",`Arm B & Arm C`="0 (0.0%)",`Pending Assignment`="0 (0.0%)"))%>%
  bind_rows(c(Stat="White",Overall="0 (0.0%)",`Arm A`="0 (0.0%)",`Arm B & Arm C`="0 (0.0%)",`Pending Assignment`="0 (0.0%)"))%>%
  bind_rows(c(Stat="Unknown or Not Reported",Overall="0 (0.0%)",`Arm A`="0 (0.0%)",`Arm B & Arm C`="0 (0.0%)",`Pending Assignment`="0 (0.0%)"))%>%
  bind_rows(c(Stat="Other",Overall="0 (0.0%)",`Arm A`="0 (0.0%)",`Arm B & Arm C`="0 (0.0%)",`Pending Assignment`="0 (0.0%)"))%>%
  .[c(3,1,4,2,5,6,7),]


### need to further edit
NumCav<-NumCav%>%
  #bind_rows(c(Stat="NZ3",Overall="0 (0.0%)",`Arm A`="0 (0.0%)",`Arm B & Arm C`="0 (0.0%)",`Pending Assignment`="0 (0.0%)"))%>%
  bind_rows(c(Stat="NZ4",Overall="0 (0.0%)",`Arm A`="0 (0.0%)",`Arm B & Arm C`="0 (0.0%)",`Pending Assignment`="0 (0.0%)"))%>%
  arrange(Stat)

Report<-c(Stat=" ",Overall=" ",
          `Arm A`=" ",`Arm B & Arm C`=" ",`Pending Assignment`="")%>%
  bind_rows(Sex)%>%
  bind_rows(Age)%>%
  bind_rows(Weight)%>%
  bind_rows(BMI)%>%
  bind_rows(TB_Episodes)%>%
  bind_rows(Current_Smoker)%>%
  bind_rows(Previous_Smoker)%>%
  bind_rows(Duration)%>%
  bind_rows(NumCav)%>%
  bind_rows(TotalVol)%>%
  bind_rows(CT)%>%
  bind_rows(TTD)%>%
  bind_rows(Race)%>%
  mutate(Stat=ifelse(Stat=="Yes" | str_detect(Stat,"NZ"),"N (%)",
                     ifelse(Stat=="Male","Male N (%)", 
                            ifelse(Stat=="NumCav"," ",Stat))))%>%
  bind_cols(data.frame(VarName=c(" ","Sex","Age","Weight (kg)","BMI","# of participants with at least one previous TB episode not within the past 3 years",
                                 "Current smoker","Previous smoker - those who are not current smokers but report a history of smoking",
                                 "Duration of smoking (years)","Number of cavities (>2 mL)","None","1","2","3",">=4","Total cavity volume (>2 mL)",
                                 "Xpert CT - Week 0","Time to positivity on MGIT (days) - Week 0","Race","","","","","","")))%>%
  .[,c(6,1,2,5,3,4)]


myft_table2c<-flextable(Report[-1,])%>%
  theme_box()%>%
  bg(bg="grey70", part="header")%>%
  font(fontname = "Arial", part="all")%>%
  fontsize(size=10, part="all")%>%
  merge_at(i=1,j=1:2, part="header")%>%
  width(j=1:6,width=c(3,1.5,1.5,1.5,1.5,1.5))%>%
  align(j=1:2,align="left", part="body")%>%
  align(j=3:6,align="center", part="all")%>%
  set_header_labels(values=list(VarName = " ",
                                Overall= paste0("All\n(n=",nrow(combineData),")"),
                                `Pending Assignment` = paste0("`Pending Assignment` Assignment to A or B, C\n(n=",nrow(combineData[combineData$Arm=="Pending Assignment",]),")"),
                                `Arm A` = paste0("Arm A\n(n=",nrow(combineData[combineData$Arm=="Arm A",]),")"),
                                `Arm B & Arm C` = paste0("Arm B & Arm C\n(n=",nrow(combineData[combineData$Arm=="Arm B & Arm C",]),")")))





