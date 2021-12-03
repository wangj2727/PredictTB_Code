### Project: PredictTB
### Purpose: create analysis dataset to be used to create DSMB tables
### Date: Data lock date: Nov 5th 2021; Program finalized date: Dec 1st 2021;
### Programmer: Jing Wang


### Note: This script needs to be run first to generate data sets used in later steps
###       part of the hard-coding script that involves PID and arm assignment was deleted 


library(dplyr)
library(tidyr)
library(tibble)
library(stringr)
library(knitr)
library(rmarkdown)
library(flextable)
library(officer)
library(here)


indir_Datafax<-"C:\\Users\\wangj27\\Desktop\\DSMB 8th\\data\\export_64_decoded\\"

myInputData<-function(platenum){
  plate<-gsub("[^[:alnum:][:blank:]+?&/,.;:|_]", " ", readLines(paste0(indir_Datafax,"plate",platenum,".dat")))
  plate<-read.table(textConnection(plate),sep="|",header=T,stringsAsFactors = F)
  names(plate)<-gsub(paste0("[_00]?",platenum),"",names(plate))
  names(plate)<-gsub(paste0(as.numeric(platenum)),"",names(plate))
  return(plate)
}


#
#  Arm Assignment
#
plate2<-myInputData("002")%>%
  select(SUBJID,BASE_YN)

plate3<-myInputData("003")%>%
  select(SUBJID,WEEK4_YN)

plate4<-myInputData("004")%>%
  select(SUBJID, CCORRES_Week16, CCORRES_Week16MinDose,RNDMCODE,RNDMCODE_A)

ARM<-plate2%>%
  full_join(plate3,by="SUBJID")%>%
  full_join(plate4,by="SUBJID")%>%
  mutate(Arm=ifelse((!is.na(BASE_YN) & BASE_YN=="No") | (!is.na(WEEK4_YN) & WEEK4_YN=="No") | (!is.na(CCORRES_Week16) & CCORRES_Week16=="No") |
                    !is.na(CCORRES_Week16MinDose) & CCORRES_Week16MinDose=="No", "Arm A",
                  ifelse((!is.na(BASE_YN) & BASE_YN=="Yes") & (!is.na(WEEK4_YN) & WEEK4_YN=="Yes") & (!is.na(CCORRES_Week16) & CCORRES_Week16=="Yes") &
                           (!is.na(CCORRES_Week16MinDose) & CCORRES_Week16MinDose=="Yes"),"Arm B & Arm C","Pending Assignment")))%>%
  mutate(Arm=ifelse(Arm=="Arm B & Arm C" & (is.na(RNDMCODE) | RNDMCODE==""),"Pending Assignment",Arm))


#
#  Screen & enrollment
#
SCREENED<-myInputData("021")%>%
  select(SUBJID, IEYN_EXCL10)%>%
  with(table(IEYN_EXCL10))

#
# Withdraw
#
plate21<-myInputData("021")

plate499 <- myInputData("499")

WITHDRAW<-plate21%>%
  filter(!is.na(IEDTC) & IEDTC!="")%>%
  select(SUBJID, IEYN_EXCL10)%>%
  inner_join(plate499, by="SUBJID")%>%
  select(SUBJID, IEYN_EXCL10, DSDECOD,DSTERM__FAIL_OTH,DSTERM_INFO2,EOSDate=VISITDAT)%>%
  filter(DSDECOD %in% c("Participant identified to have resistance on molecular or phenotypic DST",
                        "Participant enrolled in study based on a positive GeneXpert but subsequently found to be culture negative at baseline",
                        "Participant did not adhere to treatment as deemed by the investigator",
                        "Participant withdrew consent",
                        "Adverse event severe enough to require study drug discontinuation",
                        "Any reason deemed appropriate by the investigator or attending physician",
                        "Participants with evidence of pleural TB or large pleural effusions on baseline PET/CT will be withdrawn and replaced")|
           str_detect(DSDECOD,"Participants with significant incidental findings"))%>%
  select(SUBJID,DSDECOD,DSTERM__FAIL_OTH,DSTERM_INFO2,EOSDate)%>%
  left_join(ARM,by="SUBJID")%>%
  mutate(Arm=ifelse(is.na(Arm),"Pending Assignment",Arm))%>%
  mutate(Country=ifelse(SUBJID>20000,"China","South Africa"),
         site=substr(SUBJID,1,2))

### use plate23 adherence plate to determine week16 cutoff time
plate23<-myInputData("023")%>%
  filter(VISITNUM==160)%>%
  select(SUBJID,VISITDAT)

WDBeforeWk16<-plate21%>%
  filter(!is.na(IEDTC) & IEDTC!="")%>%
  select(SUBJID, IEYN_EXCL10)%>%
  inner_join(plate499, by="SUBJID")%>%
  select(SUBJID, IEYN_EXCL10, DSDECOD)%>%
  filter(DSDECOD %in% c("Participant identified to have resistance on molecular or phenotypic DST",
                        "Participant enrolled in study based on a positive GeneXpert but subsequently found to be culture negative at baseline",
                        "Participant did not adhere to treatment as deemed by the investigator",
                        "Participant withdrew consent",
                        "Adverse event severe enough to require study drug discontinuation",
                        "Any reason deemed appropriate by the investigator or attending physician",
                        "Participants with evidence of pleural TB or large pleural effusions on baseline PET/CT will be withdrawn and replaced")|
           str_detect(DSDECOD,"Participants with significant incidental findings"))%>%
  select(SUBJID,DSDECOD)%>%
  left_join(plate23,by="SUBJID")

### list of withdraw after week16
WDAfterWk16 <- WITHDRAW%>%
  filter(!SUBJID %in% WDBeforeWk16$SUBJID)



#
# number included in the report
#
### Total enrolled # minus withdraw # prior to week16
INREPORT<-myInputData("021")%>%
  filter(IEYN_EXCL10=="Yes")%>%
  anti_join(WDBeforeWk16, by="SUBJID")%>%
  select(SUBJID)%>%
  mutate(Country=ifelse(SUBJID>20000,"China","South Africa"),
         site=substr(SUBJID,1,2))%>%
  left_join(ARM,by="SUBJID")%>%
  mutate(Arm=ifelse(is.na(Arm),"Pending Assignment",Arm))


#
# Completed & Active
#
plate500<-myInputData("500")%>%
  select(SUBJID, OUT_OTH, OUTDECOD,OUTCOMEYN)


COMPLETESTUDY_3LTFU<-myInputData("499")%>%
  mutate(EOSDate = as.Date(VISITDAT, format="%d/%B/%Y"))%>%
  select(DSDECOD, SUBJID,EOSDate)%>%
  right_join(INREPORT, by="SUBJID")%>%
  filter(DSDECOD%in% c("Death","Suspected or confirmed recurrent TB","Blank",
                       "Participant on Arm A who is still culture positive at month 6",
                       "Participant is lost to follow up") )%>%
  left_join(plate500, by="SUBJID")


INREPORT <- INREPORT%>%
  filter(!SUBJID %in% LTFUBeforeWk16_NoAssign)


COMPLETESTUDY<-myInputData("499")%>%
  mutate(EOSDate = as.Date(VISITDAT, format="%d/%B/%Y"))%>%
  select(DSDECOD, SUBJID,EOSDate)%>%
  right_join(INREPORT, by="SUBJID")%>%
  filter(DSDECOD%in% c("Death","Suspected or confirmed recurrent TB","Blank",
                       "Participant on Arm A who is still culture positive at month 6",
                       "Participant is lost to follow up") )%>%
  left_join(plate500, by="SUBJID")




ACTIVE<-myInputData("001")%>%
  select(SUBJID,RFICDTC)%>%
  right_join(INREPORT, by="SUBJID")%>%
  filter(!SUBJID %in% COMPLETESTUDY_3LTFU$SUBJID)%>%
  filter(!SUBJID %in% WDAfterWk16$SUBJID)



#
# Table4 population
#
#### withdraw before week16 excluding positive baseline pDST
WDBeforeWk16_PDST<-plate21%>%
  filter(!is.na(IEDTC) & IEDTC!="")%>%
  select(SUBJID, IEYN_EXCL10)%>%
  inner_join(plate499, by="SUBJID")%>%
  select(SUBJID, IEYN_EXCL10, DSDECOD)%>%
  filter(DSDECOD %in% c("Participant enrolled in study based on a positive GeneXpert but subsequently found to be culture negative at baseline",
                        "Participant did not adhere to treatment as deemed by the investigator",
                        "Participant withdrew consent",
                        "Adverse event severe enough to require study drug discontinuation",
                        "Any reason deemed appropriate by the investigator or attending physician",
                        "Participants with evidence of pleural TB or large pleural effusions on baseline PET/CT will be withdrawn and replaced")|
           str_detect(DSDECOD,"Participants with significant incidental findings"))%>%
  select(SUBJID,DSDECOD)%>%
  left_join(plate23,by="SUBJID")


INREPORT_table4<-myInputData("021")%>%
  filter(IEYN_EXCL10=="Yes")%>%
  anti_join(WDBeforeWk16_PDST, by="SUBJID")%>%
  select(SUBJID)%>%
  mutate(Country=ifelse(SUBJID>20000,"China","South Africa"),
         site=substr(SUBJID,1,2))%>%
  left_join(ARM,by="SUBJID")%>%
  mutate(Arm=ifelse(is.na(Arm),"Pending Assignment",Arm))






