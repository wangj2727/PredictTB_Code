

### Week 16  ---- Arm C
plate23_wk16<-myInputData("023")%>%
  filter(!is.na(VISITNUM) & VISITNUM %in% c(160))%>%
  filter(!is.na(VISITDAT))%>%
  select(SUBJID)%>%
  mutate(from23="Yes")

plate33_wk16<-myInputData("033")%>%
  select(SUBJID,VISITDAT,VISITNUM)%>%
  filter(!is.na(VISITNUM) & VISITNUM %in% c(160,168))%>%
  filter(!is.na(VISITDAT))%>%
  distinct(SUBJID)%>%
  mutate(from33="Yes")%>%
  full_join(plate23_wk16,by="SUBJID")%>%
  right_join(INREPORT,by="SUBJID")%>%
  filter(!is.na(RNDMCODE) & RNDMCODE=="C   Treatment stop at week 16")


### Week 24 ---- Arm B
plate23_wk24<-myInputData("023")%>%
  filter(!is.na(VISITNUM) & VISITNUM %in% c(240))%>%
  filter(!is.na(VISITDAT))%>%
  select(SUBJID)%>%
  mutate(from23="Yes")

plate33_wk24_ArmB<-myInputData("033")%>%
  select(SUBJID,VISITDAT,VISITNUM)%>%
  filter(!is.na(VISITNUM) & VISITNUM %in% c(240,248))%>%
  filter(!is.na(VISITDAT))%>%
  distinct(SUBJID)%>%
  mutate(from33="Yes")%>%
  full_join(plate23_wk24,by="SUBJID")%>%
  right_join(INREPORT,by="SUBJID")%>%
  filter(!is.na(RNDMCODE) & RNDMCODE=="B   Continue treatment until week 24")%>%
  filter(!is.na(from23) | !is.na(from33))


### Week 24 ---- Arm A
plate29<-myInputData("029")%>%
  select(SUBJID)%>%
  mutate(from29="Yes")%>%
  full_join(INREPORT, by="SUBJID")%>%
  filter((!is.na(from29)& from29=="Yes")  | (!is.na(RNDMCODE_A) & RNDMCODE_A!=""))

plate23_wk24<-myInputData("023")%>%
  filter(!is.na(VISITNUM) & VISITNUM %in% c(240))%>%
  filter(!is.na(VISITDAT))%>%
  select(SUBJID)%>%
  mutate(from23="Yes")

plate33_wk24<-myInputData("033")%>%
  select(SUBJID,VISITDAT,VISITNUM)%>%
  filter(!is.na(VISITNUM) & VISITNUM %in% c(240,248))%>%
  filter(!is.na(VISITDAT))%>%
  distinct(SUBJID)%>%
  mutate(from33="Yes")%>%
  full_join(plate23_wk24,by="SUBJID")%>%
  right_join(plate29,by="SUBJID")%>%
  filter(!is.na(from23) | !is.na(from33))




##################################
### Check
###
#################################
# 
# library(readxl)
# SY<-read_excel("C:\\Users\\wangj27\\Desktop\\table8 PID numbers.xlsx")
# mycompare<-function(V1, V2){
#   V2<-V2[!is.na(V2)]
#   V1<-V1[!is.na(V1)]
#   OnlyInOne<-V1[!V1 %in% V2]
#   OnlyInTwo<-V2[!V2 %in% V1]
#   out<-as.data.frame(cbind(OnlyInOne, OnlyInTwo))
#   return(out)
# }
# 
# # ArmC Week16
# mycompare(plate33_wk16$SUBJID, SY$C16)
# # ArmB Week24
# mycompare(plate33_wk24_ArmB$SUBJID, SY$B24)  ### SY:14088 (no Week24 data for plate23,33 or 9)
# # ArmA Week24
# mycompare(plate33_wk24$SUBJID, SY$A24) ### JW: 14085, 21048 (both of them have week24 data for plate23)
# 
# 
# # ArmC Complete
# mycompare(Endpoint$SUBJID[Endpoint$ARM=="Arm C"], SY$`C complete`) ### SY has 18, and JW has 17;SY: 11021; Comment: No endpoint found
# # ArmB Complete
# mycompare(Endpoint$SUBJID[Endpoint$ARM=="Arm B"], SY$`B complete`) ### SY has 14, and JW has 13;SY count 15013 twice
# # ArmA Complete
# mycompare(Endpoint$SUBJID[Endpoint$ARM=="Arm A"], SY$`A complete`) ### SY has 41, and JW has 43; JW: 14042 (Death), 15026 (recurrent), 15048 (Death); SY: 12019 (no endpoint found)
# 











#########################
###
### Endpoints (several participants completed study before randomization)
###
#########################


### final output
col1<-c("Arm B & Arm C","Arm A")
col2<-c(nrow(plate33_wk16)+nrow(plate33_wk24_ArmB),nrow(plate33_wk24))
col3<-as.data.frame(table(COMPLETESTUDY$Arm[COMPLETESTUDY$Arm %in% c("Arm A","Arm B & Arm C")]))%>%
  arrange(desc(Var1))%>%
  select(Freq)

combine<-as.data.frame(cbind(col1,col2,col3))

### check if there was any participant who had an endpoint before being assigned to any arms
Pending<-INREPORT%>%
  left_join(plate499,by="SUBJID")%>%
  filter(DSDECOD%in% c("Death","Suspected or confirmed recurrent TB","Blank",
                       "Participant on Arm A who is still culture positive at month 6",
                       "Participant is lost to follow up"))%>%
  filter(Arm=="Pending Assignment")
### N=3, 12056, 23045, 25075


#############################
##
## Generate Output
##
#############################
myft_table15<-flextable(combine)%>%
  theme_box()%>%
  bg(bg="grey70", part="header")%>%
  font(fontname = "Arial", part="all")%>%
  fontsize(size=10, part="all")%>%
  width(j=1:3,width=c(2.5,2.5,2.5))%>%
  align(j=1,align="left", part="all")%>%
  align(j=2:3,align="center", part="all")%>%
  set_header_labels(values=list(col1="Completed treatment by arm",
                                col2="Number completed treatment",
                                Freq="Number completed study"))%>%
  add_footer_lines(values = "Completion of treatment is at week16, 24, and 24 for arms A, B, and C, respectively.")
