
########### Notes:
### the following scenarios should be checked everytime this table gets generated
# 1. Same PID, Same AE, Same Date, Same Severity ---- should be remove
# 2. Same PID, Same AE, Same Date, Different Severity ---- only count the one with highest grade
# 3. Same PID, Same AE, Different Date, Same Severity ---- included
# 4. Same PID, Same AE, Different Date, Diferent Severity ---- included



#######################
#
# Create final list of AE tables
########################


plate350<-myInputData("350")%>%
  select(SUBJID,AEDECOD,AELLT, AESTDAT,AEENDTC,AESER,AESEV,AEREL,AEOUT,AE_RxRelated,AE_SAE_Reason)%>%
  filter((AE_RxRelated=="Unchecked" & AEREL %in% c("Possibly related","Probably related","Definitely related")) | 
           (AE_RxRelated=="Checked" & AESEV %in% c("Grade 3  Severe","Grade 4  Life threatening","Grade 5  Death")) | 
           (AESER=="Yes"))%>%
  left_join(INREPORT, by="SUBJID")



plate350<-plate350%>%
  mutate(AEENDTC=ifelse(substr(AEENDTC,1,2)=="00",paste0("30",substr(AEENDTC,3,11)),AEENDTC),
         AESTDAT=ifelse(substr(AESTDAT,1,2)=="00",paste0("01",substr(AESTDAT,3,11)),AESTDAT),
         start=as.Date(AESTDAT,format="%d/%B/%Y"),
         end=as.Date(AEENDTC,format="%d/%B/%Y"),
         AEduration=as.numeric(end - start))%>%
  mutate(site=substr(SUBJID,1,2),
         Country=ifelse(substr(SUBJID,1,1)==1,"South Africa","China"),
         Site=ifelse(site=='11',"Khayelitsha",
                     ifelse(site=='12',"SATVI",
                            ifelse(site=='13',"SUN",
                                   ifelse(site=='14',"TASK",
                                          ifelse(site=='15',"UCT Lung",
                                                 ifelse(site=='21',"Kaifeng",
                                                        ifelse(site=='22',"Xinmi",
                                                               ifelse(site=='23',"Zhongmu",
                                                                      ifelse(site=='24',"HPCH",
                                                                             ifelse(site=='25',"Xinxiang","Unknown")))))))))))%>%
  arrange(Country, site, SUBJID)%>%
  select(Country, Site, ARM=Arm, SUBJID,AEDECOD,AELLT, AESTDAT, AEENDTC,AEduration, AESER, AESEV, AEREL, AEOUT)%>%
  mutate(AEduration=as.character(AEduration),
         AEduration=ifelse(is.na(AEduration)," ",AEduration),
         AEOUT=ifelse(AEOUT=="Blank"," ",AEOUT))


### Hard Code two AE descriptions: 15063 and 21068 (AESTETC=13/Aug/2019); Reason: data came in after data freeze on 06OCT2019
plate350<-plate350%>%
  mutate(AELLT=ifelse(SUBJID==15063 & AESTDAT=="24/SEP/2019","Traumatic pneumothorax",
                      ifelse(SUBJID==21068 & AESTDAT=="13/AUG/2019","Function liver abnormal",AELLT)),
         AEDECOD=ifelse(SUBJID==21068 & AESTDAT=="13/AUG/2019","Hepatic function abnormal",AEDECOD))%>%
  mutate(AEDECOD=ifelse(SUBJID==15063 & AESTDAT=="24/SEP/2019","Traumatic pneumothorax",AEDECOD),
         ARM=ifelse(!ARM %in% c("Arm A","Arm B & Arm C"),"Pending",ARM))%>%
  mutate(AESEV=str_extract(AESEV,"Grade \\d+"))%>%
  select(ARM, SUBJID, AEDECOD, AESEV,AESTDAT)%>%
  mutate(AEDECOD = ifelse(is.na(AEDECOD) | AEDECOD=="", "Head trauma",AEDECOD))






#########################
##
## check duplications
#########################

### if the same AE happened on the same date, with the same severity grade, it should be removed;
checkSameAESameDateSameSev<-plate350%>%
  arrange(SUBJID, AESTDAT,AEDECOD, desc(AESEV))%>%
  filter(duplicated(cbind(SUBJID, AESTDAT, AEDECOD,AESEV)))
nrow(checkSameAESameDateSameSev) ## if>0, then should be removed



### if the same AE happened on the same date, but with different severity grade, then only count the one with higher grade
checkSameAESameDate<-plate350%>%
  arrange(SUBJID, AESTDAT,AEDECOD, desc(AESEV))%>%
  filter(duplicated(cbind(SUBJID, AESTDAT, AEDECOD)))
nrow(checkSameAESameDate)  ## if>0, then should be removed




#### Count number of participants with AE, Exclude: event=same PID/same AE/different SEV
plate350_NoDupSubj<-plate350[!duplicated(cbind(plate350$SUBJID, plate350$AEDECOD)),]
TotalSUBJ<-as.data.frame.matrix(table(plate350_NoDupSubj$AEDECOD, plate350_NoDupSubj$AESEV))%>%
  rownames_to_column(var="AEDescription")%>%
  mutate(TotalSubj=rowSums(select(., contains("Grade"))))%>%
  gather("Grade","CountSubj","Grade 1":"Grade 5")%>%
  arrange(AEDescription, desc(Grade))%>%
  select(-CountSubj)


#### Count number of participants with AE, Include: event=same PID/same AE/different SEV
plate350_NoDupSubjNoDupSev<-plate350[!duplicated(cbind(plate350$SUBJID, plate350$AEDECOD, plate350$AESEV)),]
TotalSUBJ_DiffSev<-as.data.frame.matrix(table(plate350_NoDupSubjNoDupSev$AEDECOD, plate350_NoDupSubjNoDupSev$AESEV))%>%
  rownames_to_column(var="AEDescription")%>%
  gather("Grade","CountSubj2","Grade 1":"Grade 5")%>%
  arrange(AEDescription, desc(Grade))


#### Total AE calculation
TotalAE<-as.data.frame.matrix(table(plate350$AEDECOD, plate350$AESEV))%>%
  rownames_to_column(var="AEDescription")%>%
  mutate(TotalAE=rowSums(select(., contains("Grade"))))%>%
  gather("Grade","CountAE","Grade 1":"Grade 5")%>%
  arrange(AEDescription, desc(Grade))%>%
  full_join(TotalSUBJ, by=c("AEDescription","Grade"))%>%
  full_join(TotalSUBJ_DiffSev, by=c("AEDescription","Grade"))%>%
  mutate(SumAE=nrow(plate350),
         SumSubj=nrow(plate350_NoDupSubj),
         AllCol=paste0(CountAE,"(",CountSubj2,") ",round(CountSubj2*100/nrow(INREPORT),1),"%"),
         TotalCol=paste0(TotalAE,"(",TotalSubj,") ",round(TotalAE*100/SumAE,1),"%"))%>%
  select(AEDescription, Grade, TotalCol, AllCol)



### Arm A
ArmA_NoDupSubj<-plate350%>%
  filter(!is.na(ARM) & ARM=="Arm A")%>%
  filter(!duplicated(cbind(SUBJID, AEDECOD)))

ArmA_NoDupSubjNoDupSev<-plate350%>%
  filter(!is.na(ARM) & ARM=="Arm A")%>%
  filter(!duplicated(cbind(SUBJID, AEDECOD,AESEV)))

ArmA_DiffSev<-as.data.frame.matrix(table(ArmA_NoDupSubjNoDupSev$AEDECOD, ArmA_NoDupSubjNoDupSev$AESEV))%>%
  rownames_to_column(var="AEDescription")%>%
  gather("Grade","CountSubj2","Grade 1":"Grade 5")%>%
  arrange(AEDescription, desc(Grade))

ArmA<-plate350%>%
  filter(!is.na(ARM) & ARM=="Arm A")

ArmA_AE<-as.data.frame.matrix(table(ArmA$AEDECOD, ArmA$AESEV))%>%
  rownames_to_column(var="AEDescription")%>%
  mutate(TotalAE=rowSums(select(., contains("Grade"))))%>%
  gather("Grade","CountAE","Grade 1":"Grade 5")%>%
  arrange(AEDescription, desc(Grade))%>%
  full_join(ArmA_DiffSev, by=c("AEDescription","Grade"))%>%
  mutate(ArmACol=paste0(CountAE,"(",CountSubj2,") ",round(CountSubj2*100/nrow(INREPORT[INREPORT$Arm=="Arm A",]),1),"%"))%>%
  select(AEDescription, Grade, ArmACol)



### Arm B/C
ArmB_NoDupSubj<-plate350%>%
  filter(!is.na(ARM) & ARM %in% c("Arm B & Arm C"))%>%
  filter(!duplicated(cbind(SUBJID, AEDECOD)))

ArmB_NoDupSubjNoDupSev<-plate350%>%
  filter(!is.na(ARM) & ARM %in% c("Arm B & Arm C"))%>%
  filter(!duplicated(cbind(SUBJID, AEDECOD,AESEV)))

ArmB_DiffSev<-as.data.frame.matrix(table(ArmB_NoDupSubjNoDupSev$AEDECOD, ArmB_NoDupSubjNoDupSev$AESEV))%>%
  rownames_to_column(var="AEDescription")%>%
  gather("Grade","CountSubj2","Grade 1":"Grade 5")%>%
  arrange(AEDescription, desc(Grade))

ArmB<-plate350%>%
  filter(!is.na(ARM) & ARM %in% c("Arm B & Arm C"))

ArmBC_AE<-as.data.frame.matrix(table(ArmB$AEDECOD, ArmB$AESEV))%>%
  rownames_to_column(var="AEDescription")%>%
  mutate(TotalAE=rowSums(select(., contains("Grade"))))%>%
  gather("Grade","CountAE","Grade 1":"Grade 5")%>%
  arrange(AEDescription, desc(Grade))%>%
  full_join(ArmB_DiffSev, by=c("AEDescription","Grade"))%>%
  mutate(ArmBCol=paste0(CountAE,"(",CountSubj2,") ",round(CountSubj2*100/nrow(INREPORT[INREPORT$Arm %in% c("Arm B & Arm C"),]),1),"%"))%>%
  select(AEDescription, Grade, ArmBCol)




### Pending column
Pending_NoDupSubj<-plate350%>%
  filter(!is.na(ARM) & ARM=="Pending")%>%
  filter(!duplicated(cbind(SUBJID, AEDECOD)))

Pending_NoDupSubjNoDupSev<-plate350%>%
  filter(!is.na(ARM) & ARM=="Pending")%>%
  filter(!duplicated(cbind(SUBJID, AEDECOD,AESEV)))

Pending_DiffSev<-as.data.frame.matrix(table(Pending_NoDupSubjNoDupSev$AEDECOD, Pending_NoDupSubjNoDupSev$AESEV))%>%
  rownames_to_column(var="AEDescription")%>%
  gather("Grade","CountSubj2",contains("Grade"))%>%
  arrange(AEDescription, desc(Grade))

Pending<-plate350%>%
  filter(!is.na(ARM) & ARM=="Pending")

Pending_AE<-as.data.frame.matrix(table(Pending$AEDECOD, Pending$AESEV))%>%
  rownames_to_column(var="AEDescription")%>%
  mutate(TotalAE=rowSums(select(., contains("Grade"))))%>%
  gather("Grade","CountAE",contains("Grade"))%>%
  arrange(AEDescription, desc(Grade))%>%
  full_join(Pending_DiffSev, by=c("AEDescription","Grade"))%>%
  mutate(PendingCol=paste0(CountAE,"(",CountSubj2,") ",round(CountSubj2*100/nrow(INREPORT[INREPORT$Arm %in% c("Remain eligible for Arm B/C",
                                                                                                              "Pending Assignment"),]),1),"%"))%>%
  select(AEDescription, Grade, PendingCol)


### Combine columns
final<-TotalAE%>%
  full_join(ArmA_AE, by=c("AEDescription","Grade"))%>%
  full_join(ArmBC_AE, by=c("AEDescription","Grade"))%>%
  full_join(Pending_AE, by=c("AEDescription","Grade"))%>%
  mutate(ArmACol=ifelse(is.na(ArmACol),"0(0) 0%",ArmACol),
         ArmBCol=ifelse(is.na(ArmBCol),"0(0) 0%",ArmBCol),
         PendingCol=ifelse(is.na(PendingCol),"0(0) 0%",PendingCol))%>%
  select(AEDescription, TotalCol, everything())



createSheet(wb, name = "Table14_PID")
writeWorksheet(wb, plate350, sheet = "Table14_PID")




#########################
##
## Generate outputs
#########################

merge_custom <- function(ft, x, columns){
  z <- rle(x)
  rows_at <- cumsum(z$lengths) - z$lengths + 1
  
  for(i in seq_along(rows_at)){
    for(j in columns)
      ft <- merge_at(x = ft, i = seq( rows_at[i], rows_at[i] + z$lengths[i] - 1), j = j)
  }
  
  ft
}

myft_table14<-flextable(final)%>%
  theme_box()%>%
  bg(bg="grey70", part="header")%>%
  font(fontname = "Arial", part="all")%>%
  fontsize(size=10, part="all")%>%
  width(j=1:7,width=c(2,1,1,1,1,1,1))%>%
  align(j=1,align="left", part="all")%>%
  align(j=2:7,align="center", part="all")%>%
  merge_custom(x=final$AEDescription, columns=1:2)%>%
  vline_left(border = fp_border(color="black") )%>%
  set_header_labels(values=list(AEDescription="AE Description",
                                TotalCol= paste0("Total AEs*\n(N=",nrow(plate350),")"),
                                Grade = "Severity",
                                AllCol= paste0("All\n(N=",nrow(INREPORT),")"),
                                ArmACol= paste0("Arm A\n(N=",nrow(INREPORT[INREPORT$Arm=="Arm A",]),")"),
                                ArmBCol= paste0("Arm B & Arm C\n(N=",nrow(INREPORT[INREPORT$Arm %in% c("Arm B & Arm C"),]),")"),
                                PendingCol= paste0("Pending\n(N=",nrow(INREPORT[INREPORT$Arm %in% c("Remain eligible for Arm B/C",
                                                                                                    "Pending Assignment"),]),")")))%>%
  add_footer_lines(values="*The number in parenthesis in this column refers to the number of participants who had this event.")






