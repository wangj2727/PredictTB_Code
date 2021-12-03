

combine<-as.data.frame(table(WITHDRAW$DSDECOD))
combine$Var1<-as.character(combine$Var1)
combine<-rbind(combine,data.frame(Var1="Total", Freq=sum(combine$Freq,na.rm=TRUE)))


SA<-as.data.frame(table(WITHDRAW$DSDECOD[WITHDRAW$Country=="South Africa"]))
SA$Var1<-as.character(SA$Var1)
SA<-rbind(SA,data.frame(Var1="Total", Freq=sum(SA$Freq,na.rm=TRUE)))


China<-as.data.frame(table(WITHDRAW$DSDECOD[WITHDRAW$Country=="China"]))
China$Var1<-as.character(China$Var1)
China<-rbind(China,data.frame(Var1="Total", Freq=sum(China$Freq,na.rm=TRUE)))


final<-combine%>%
  full_join(SA,by="Var1")%>%
  full_join(China,by="Var1")%>%
  mutate(Reasons=ifelse(Var1=="Participant identified to have resistance on molecular or phenotypic DST",1,
                        ifelse(Var1=="Participant enrolled in study based on a positive GeneXpert but subsequently found to be culture negative at baseline",2,
                               ifelse(Var1=="Participant did not adhere to treatment as deemed by the investigator",5,
                                      ifelse(Var1=="Participant withdrew consent",6,
                                             ifelse(Var1=="Adverse event severe enough to require study drug discontinuation",8,
                                                    ifelse(Var1=="Any reason deemed appropriate by the investigator or attending physician",9,
                                                           ifelse(Var1=="Participants with evidence of pleural TB or large pleural effusions on baseline PET/CT will be WITHDRAWn and replaced",10,
                                                                  ifelse(str_detect(Var1,"Participants with significant incidental findings"),"Participants with significant incidental findings on PET/CT that require immediate diagnostic procedures or treatment may be WITHDRAWn from the study if in the opinion of the investigator, continuing on the study may not be in the participant's best interests",
                                                                         ifelse(Var1=="Blank",0," "))))))))))%>%
  mutate(Reasons=as.numeric(Reasons))%>%
  arrange(Reasons)%>%
  select(Var1, Total=Freq.x, SA=Freq.y, China=Freq)%>%
  mutate(SA=ifelse(is.na(SA),0,SA),
         China=ifelse(is.na(China),0,China))%>%
  mutate(Var1=ifelse(Var1=="Any reason deemed appropriate by the investigator or attending physician",
                     "*Any reason deemed appropriate by the investigator or attending physician",Var1))


PI_reason<-WITHDRAW%>%
  filter(DSDECOD=="Any reason deemed appropriate by the investigator or attending physician")%>%
  select(SUBJID,DSTERM_INFO2)


createSheet(wb, name = "Table3_PID")
writeWorksheet(wb, WITHDRAW%>%select(SUBJID, DSDECOD, Arm), sheet = "Table3_PID")



myft_table3<-flextable(final)%>%
  theme_box()%>%
  bg(bg="grey70", part="header")%>%
  font(fontname = "Arial", part="all")%>%
  fontsize(size=10, part="all")%>%
  
  colformat_double(j=2:4, digits=0)%>% 
  width(j=1:4,width=c(5,1.5,1.5,1.5))%>%
  align(j=1,align="left", part="all")%>%
  align(j=2:4,align="center", part="all")%>%
  set_header_labels(values=list(Var1 = " ",
                                Total = "Overall\n(N)",
                                SA = "South Africa\n(N)",
                                China = "China\n(N)"))%>%
  add_footer_lines(values =paste0('* Reasons include:\n',str_c(paste0(str_to_sentence(PI_reason$DSTERM_INFO2),"(N=1)"), collapse="\n")))



