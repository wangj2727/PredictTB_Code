



dta<-myInputData("496")%>%
  filter(!substr(SUBJID,3,3)=="9")%>%   ### remove participants with 1X9XX as the PID in SA sites
  select(DVNUM_Name)%>%
  mutate(Reason=ifelse(is.na(DVNUM_Name),NA,
                       ifelse(DVNUM_Name==1,"Missed visit",
                              ifelse(DVNUM_Name==2,"Study visit or tests out of window",
                                     ifelse(DVNUM_Name==3,"PET/CT incorrect settings",
                                            ifelse(DVNUM_Name==4,"Wrong number of doses dispensed",
                                                   ifelse(DVNUM_Name==5,"Conducting procedures not delegated for",
                                                          ifelse(DVNUM_Name==6,"Specimen collection issue",
                                                                 ifelse(DVNUM_Name==7,"Informed consent issue",
                                                                        ifelse(DVNUM_Name==8,"Breach of PII",
                                                                               ifelse(DVNUM_Name==9,"Ineligible for enrollment",
                                                                                      ifelse(DVNUM_Name==10,"Unanticipated procedure/surgery",
                                                                                             ifelse(DVNUM_Name==11,"Change in status (e.g. increased level of care required)",
                                                                                                    ifelse(DVNUM_Name==12,"AE/SAE",
                                                                                                           ifelse(DVNUM_Name==13,"Freezer or sample storage issue",
                                                                                                                  ifelse(DVNUM_Name==14,"Missed or late reporting",
                                                                                                                         ifelse(DVNUM_Name==15,"PET/CT visit out of window",
                                                                                                                                ifelse(DVNUM_Name==16,"Study procedures or tests not performed",
                                                                                                                                       ifelse(DVNUM_Name==17,"Intensive phase extended beyond week 8",
                                                                                                                                              ifelse(DVNUM_Name==18,"Continuation phase extended beyong week 24",
                                                                                                                                                     ifelse(DVNUM_Name==19,"MERM Questionnaire issue",
                                                                                                                                                            ifelse(DVNUM_Name==21,"Other","Unknown"))))))))))))))))))))))


combine<-as.data.frame(table(dta$Reason))
pct<-as.numeric(round(prop.table(table(dta$Reason))*100,1))
combine$Npct<-paste0(combine$Freq," (",pct,"%)")
combine<-combine%>%
  select(Var1,Npct)



### Shawn moved 2 from the "Other" category to "Intensive phase extended beyond week 8", manually on Oct 25th 2019
