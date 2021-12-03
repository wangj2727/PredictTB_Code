### 11071 randomized at wk12 to Arm A, but withdrew at wk16 for multiple drug resistance --> late WD
### 13019 did file plate499, but missed plate21 --> SF

### four participants moved to Arm A due to pregnancy (could not be tracked from DF fields except edit check)
# 14067 got pregnant. So moved to Arm A at W16 to avoid treatment shortening and no more PET/CT scans.
# 13039 went to A at W4 due to pregnancy.
# 14130 went to A at W4 due to pregnancy.
# 15075 went to A at W4 due to pregnancy.

### 14144 didn't have plate23 at week16,but have week16 plate4 --> late WD
### 12017 499 success completion, error, reinfection;should be reinfection, plate499 is wrong. Met study endpoint
### 15002 should be ArmA, but was incorrectly recorded 


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
  mutate(CCORRES_Week16 = ifelse(SUBJID==15002, "No",CCORRES_Week16))%>%
  mutate(Arm=ifelse((!is.na(BASE_YN) & BASE_YN=="No") | (!is.na(WEEK4_YN) & WEEK4_YN=="No") | (!is.na(CCORRES_Week16) & CCORRES_Week16=="No") |
                    !is.na(CCORRES_Week16MinDose) & CCORRES_Week16MinDose=="No", "Arm A",
                  ifelse((!is.na(BASE_YN) & BASE_YN=="Yes") & (!is.na(WEEK4_YN) & WEEK4_YN=="Yes") & (!is.na(CCORRES_Week16) & CCORRES_Week16=="Yes") &
                           (!is.na(CCORRES_Week16MinDose) & CCORRES_Week16MinDose=="Yes"),"Arm B & Arm C","Pending Assignment")))%>%
  mutate(Arm=ifelse(Arm=="Arm B & Arm C" & (is.na(RNDMCODE) | RNDMCODE==""),"Pending Assignment",Arm))%>%
  mutate(Arm=ifelse(SUBJID %in% c(14067, 14130, 15075, 13039),"Arm A",Arm))


#
#  Screen & enrollment
#
SCREENED<-myInputData("021")%>%
  select(SUBJID, IEYN_EXCL10)%>%
  bind_rows(data.frame(SUBJID=13109, IEYN_EXCL10="No"))%>% 
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
         site=substr(SUBJID,1,2))%>%
  filter(!SUBJID %in% c(11071))### 11071 is a re-infection

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
  left_join(plate23,by="SUBJID")%>%
  filter(is.na(VISITDAT) | SUBJID %in% c(13018, 12091, 12093))%>%  ### 12091, 12093, no arm assignment, deemed to be early WD (Email with Ray and Shawn, date: 23Nov2021)
  filter(!SUBJID %in% c(14144))  

### list of withdraw after week16
WDAfterWk16 <- WITHDRAW%>%
  filter(!SUBJID %in% WDBeforeWk16$SUBJID)%>%
  filter(!SUBJID %in% c(13018,12091, 12093))

### list of LTFU prior to week16 with no arm assignment
LTFUBeforeWk16_NoAssign <- c(12056, 23045, 25075)


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
                       "Participant is lost to follow up") | SUBJID %in% c(12017, 11071,14058, 23041,25055, 12052, 12082, 14136, 15075, 23056, 23039))%>%
  left_join(plate500, by="SUBJID")


INREPORT <- INREPORT%>%
  filter(!SUBJID %in% LTFUBeforeWk16_NoAssign)


COMPLETESTUDY<-myInputData("499")%>%
  mutate(EOSDate = as.Date(VISITDAT, format="%d/%B/%Y"))%>%
  select(DSDECOD, SUBJID,EOSDate)%>%
  right_join(INREPORT, by="SUBJID")%>%
  filter(DSDECOD%in% c("Death","Suspected or confirmed recurrent TB","Blank",
                       "Participant on Arm A who is still culture positive at month 6",
                       "Participant is lost to follow up") | SUBJID %in% c(12017, 11071,14058, 23041,25055, 12052, 12082, 14136, 15075, 23056, 23039))%>%
  left_join(plate500, by="SUBJID")




ACTIVE<-myInputData("001")%>%
  select(SUBJID,RFICDTC)%>%
  right_join(INREPORT, by="SUBJID")%>%
  filter(!is.na(RFICDTC) | SUBJID==12044)%>%
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
  left_join(plate23,by="SUBJID")%>%
  filter(is.na(VISITDAT) | SUBJID %in% c(13018, 12091, 12093))%>%
  filter(!SUBJID %in% c(14144))


INREPORT_table4<-myInputData("021")%>%
  filter(IEYN_EXCL10=="Yes")%>%
  anti_join(WDBeforeWk16_PDST, by="SUBJID")%>%
  select(SUBJID)%>%
  mutate(Country=ifelse(SUBJID>20000,"China","South Africa"),
         site=substr(SUBJID,1,2))%>%
  left_join(ARM,by="SUBJID")%>%
  mutate(Arm=ifelse(is.na(Arm),"Pending Assignment",Arm))
# N=594





