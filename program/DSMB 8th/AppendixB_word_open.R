plate350<-myInputData("350")%>%
  select(SUBJID,AEDECOD, AESTDAT,AEENDTC,AESER,AESEV,AEREL,AEOUT,AE_RxRelated,AE_SAE_Reason)%>%
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
  select(Country, Site,  SUBJID,AEDECOD, AESTDAT, AEENDTC,AEduration, AESER, AESEV, AEREL, AEOUT)%>%
  mutate(AEduration=as.character(AEduration),
         AEduration=ifelse(is.na(AEduration)," ",AEduration),
         AEOUT=ifelse(AEOUT=="Blank"," ",AEOUT))


### Hard Code two AE descriptions: 15063 and 21068 (AESTETC=13/Aug/2019); Reason: data came in after data freeze on 06OCT2019
plate350<-plate350%>%
  mutate(SUBJID=as.character(SUBJID),
         AELLT=ifelse(SUBJID==15063 & AESTDAT=="24/SEP/2019","Traumatic pneumothorax",
                      ifelse(SUBJID==21068 & AESTDAT=="13/AUG/2019","Function liver abnormal",AEDECOD)))%>%
  select(-AELLT)%>%
  mutate(AEDECOD = ifelse(is.na(AEDECOD) | AEDECOD=="", "Head trauma",AEDECOD))



myft_AppB<-flextable(plate350)%>%
  theme_box()%>%
  bg(bg="grey70", part="header")%>%
  font(fontname = "Arial", part="all")%>%
  fontsize(size=10, part="all")%>%
  
  width(j=1:11,width=c(0.8,1,0.8,1.5,1,1,0.5,0.5,1.2,1,1.5))%>%
  colformat_double(j=c("AEduration"), digits=0)%>% 
  align(j=c(3,5,6,7,8),align="center", part="all")%>%
  align(j=c(1,2,4,9,10,11),align="left", part="body")%>%
  set_header_labels(values=list(SUBJID = "Participant\nID",
                                AEDECOD = "AE Description",
                                AESTDAT = "AE Start\nDate",
                                AEduration = "AE\nDuration\n(Days)",
                                AESEV = "AE\nSeverity",
                                AEREL = "AE\nRelationship\nto Study",
                                AEOUT = "Outcome*",
                                AEENDTC = "AE End\nDate",
                                AESER = "SAE\n(Y/N)"))%>%
  add_footer_lines(values = "* Blank cells represent ongoing and/or unresolved outcomes.")

