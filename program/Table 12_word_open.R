

plate39<-myInputData("039")%>%
  select(SUBJID,SignConsent_PKSubStudy,RFICDTC_PKSubStudy)

PK<-myInputData("002")%>%
  select(SUBJID,BASE_YN)%>%
  full_join(plate3,by="SUBJID")%>%
  full_join(plate39,by="SUBJID")%>%
  mutate(site=substr(SUBJID,1,2))%>%
  mutate(country=ifelse(as.numeric(site)>20,"China","SA"))


# Eligible<-PK%>%
#   filter(SUBJID %in% INREPORT$SUBJID)%>%
#   mutate(group=ifelse(BASE_YN=="Yes"&WEEK4_YN=="No","ArmA",
#                       ifelse(BASE_YN=="Yes"&WEEK4_YN=="Yes","Control"," ")))%>%
#   filter(group=="ArmA")

Eligible<-PK%>%
  filter(SUBJID %in% INREPORT$SUBJID)%>%
  mutate(group=ifelse(BASE_YN=="Yes"&WEEK4_YN==2,"ArmA",
                      ifelse(BASE_YN=="Yes"&WEEK4_YN==1,"Control"," ")))%>%
  filter(group=="ArmA")


consented<-PK%>%
  filter(SUBJID %in% INREPORT$SUBJID)%>%
  filter(!is.na(RFICDTC_PKSubStudy))%>%
  mutate(group=ifelse(BASE_YN=="Yes"&WEEK4_YN==2,"ArmA",
                      ifelse(BASE_YN=="Yes"&WEEK4_YN==1,"Control"," ")))%>%
  filter(group!=" ")


table12<-data.frame(country=c("All","South Africa","China"),
                    ArmA=c(length(consented$SUBJID[consented$group=="ArmA"]),
                           length(consented$SUBJID[consented$country=="SA"&consented$group=="ArmA"]),
                           length(consented$SUBJID[consented$country=="China"&consented$group=="ArmA"])
                    ),
                    Control=c(length(consented$SUBJID[consented$group=="Control"]),
                              length(consented$SUBJID[consented$country=="SA"&consented$group=="Control"]),
                              length(consented$SUBJID[consented$country=="China"&consented$group=="Control"])
                    ))
names(table12)<-c("Var1","Number of cases","Number of controls")

### ARm A 14004 was included here;


createSheet(wb, name = "Table12_PID")
writeWorksheet(wb, consented%>%select(SUBJID, country, site, group), sheet = "Table12_PID")



myft_table12<-flextable(table12)%>%
  theme_box()%>%
  bg(bg="grey70", part="header")%>%
  font(fontname = "Arial", part="all")%>%
  fontsize(size=10, part="all")%>%
  
  width(j=1:3,width=c(2,2,2))%>%
  align(j=2:3,align="center", part="all")%>%
  align(j=1,align="left", part="all")%>%
  set_header_labels(values=list(Var1 = "  ",
                                `Number of cases` = "Number of cases",
                                `Number of controls` = "Number of controls"))%>%
  add_footer_lines(values=paste0("(1) Number of cases denotes participants moved from Arm B/C at baseline to Arm A at week 4 and subsequently enrolled into the PK sub-study.\n(2) Number of control cases denotes participants who were still eligible for Arm B/C at week 4 and subsequently enrolled into the PK sub-study.\n(3) ",nrow(Eligible)," participants were eligible for the PK sub-study"))




