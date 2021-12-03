


### get Age and Sex
plate1<-myInputData("001")%>%
  select(SUBJID,SEX,RFICDTC,BRTHDAT)%>%
  mutate(Consent=as.Date(RFICDTC,format="%d/%B/%Y"),Birthday=as.Date(BRTHDAT,format="%d/%B/%Y"),
         Age=as.numeric(Consent-Birthday)/365.25)%>%
  mutate(Age=floor(Age))



final<-WITHDRAW%>%
  left_join(plate1,by="SUBJID")%>%
  mutate(Country=ifelse(SUBJID>20000,"China","South Africa"),
         site=substr(SUBJID,1,2),
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
  select(Country, Site, SUBJID,SEX, Age, EOSDate, DSDECOD)%>%
  mutate(SUBJID=as.character(SUBJID))



myft_AppA<-flextable(final)%>%
  theme_box()%>%
  bg(bg="grey70", part="header")%>%
  font(fontname = "Arial", part="all")%>%
  fontsize(size=10, part="all")%>%
  
  width(j=1:7,width=c(1,1,1,1,0.5,1.2,3.3))%>%
  colformat_double(j="Age",digits=0)%>%
  align(j=c(1,2,3,4,5,6),align="center", part="all")%>%
  align(j=c(7),align="left", part="all")%>%
  set_header_labels(values=list(Country = "Country",
                                Site = "Site",
                                SUBJID = "Participant ID",
                                SEX= "Sex",
                                Age = "Age (years)**",
                                EOSDate = "Completed Date/\nLast Visit Date",
                                DSDECOD = "Reason Not Completing"))%>%
  add_footer_lines(values="**Age at baseline visits")



