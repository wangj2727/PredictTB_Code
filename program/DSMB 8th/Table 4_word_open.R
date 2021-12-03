
### 14138 and 14150, PHENOTYPIC DST ATTMEPS FAILED TO YIELD SATISFATORY RESULTS. 
# MOLECULAR RESISTANCE TEST SHOWS INH RESISTANCE
# confirmed with Shawn that we'll consider these two participants to be INH mono-resistant (12AUG2020)


plate14<-myInputData("014")%>%
  right_join(INREPORT_table4, by="SUBJID")%>%
  filter(VISITNUM<300)%>%
  filter((!is.na(MBDTC_PDST) & MBDTC_PDST!="") | SUBJID %in% c(14138, 14150)) # add the two back

### N=591

### 7 Replacement sample
# plate14[plate14$VISITNUM %in% c(98,99,8,9,18,19,28,29),]
#       SUBJID VISITNUM
# 181  14048       98
# 184  14051       98
# 200  14078       98
# 212  14091       98
# 225  14112       98
# 251  15033       98
# 264  15051       98


DSTresult<-function(x){
  if(any(!is.na(x)&x=="Sensitive")){
    x<-"Sensitive"
  }else if(any(!is.na(x)&x=="Resistant")){
    x<-"Resistant"
  }else if(!any(!is.na(x)&x=="Sensitive")&!any(!is.na(x)&x=="Resistant")&any(!is.na(x)&x=="Indeterminate")){
    x<-"Indeterminate/Error"
  }else if(!any(!is.na(x)&x=="Sensitive")&!any(!is.na(x)&x=="Resistant")&any(!is.na(x)&x=="Error")){
    x<-"Indeterminate/Error"
  }else{
    x<-"Missing"
  }
  return(x)
}


INH<-plate14%>%
  filter(VISITNUM<300)%>%
  select(SUBJID,VISITNUM,MBORRES_CH_Inh)%>%
  mutate(MBORRES_CH_Inh=ifelse(is.na(MBORRES_CH_Inh),"Blank",MBORRES_CH_Inh))%>%
  spread(VISITNUM,MBORRES_CH_Inh)
INH$INH<-apply(INH,1,DSTresult)
INH<- INH%>%
  mutate(INH=ifelse(SUBJID %in% c(14138, 14150), "Resistant", INH))

RIF<-plate14%>%
  filter(VISITNUM<300)%>%
  select(SUBJID,VISITNUM,MBORRES_CH_Rif)%>%
  mutate(MBORRES_CH_Rif=ifelse(is.na(MBORRES_CH_Rif),"Blank",MBORRES_CH_Rif))%>%
  spread(VISITNUM,MBORRES_CH_Rif)
RIF$RIF<-apply(RIF,1,DSTresult)
RIF <- RIF%>%
  mutate(RIF=ifelse(SUBJID %in% c(14138, 14150), "Sensitive", RIF))




SA<-INH%>%
  select(SUBJID,INH)%>%
  full_join(RIF,by="SUBJID")%>%
  select(SUBJID,INH,RIF)%>%
  filter(substr(SUBJID,1,1)=="1")%>%
  filter(substr(SUBJID,3,3)!="9")
CHN<-INH%>%
  select(SUBJID,INH)%>%
  full_join(RIF,by="SUBJID")%>%
  select(SUBJID,INH,RIF)%>%
  filter(substr(SUBJID,1,1)=="2")%>%
  filter(substr(SUBJID,3,3)!="9")


combine<-SA%>%
  bind_rows(CHN)%>%
  mutate(Country=ifelse(SUBJID>20000,"China","South Africa"),
         Outcome=ifelse(INH=="Sensitive" & RIF=="Sensitive","Not Resistant",
                        ifelse(INH=="Resistant" & RIF=="Sensitive","INH, Mono-resistant",
                               ifelse(INH=="Sensitive" & RIF=="Resistant","RIF, mono-resistant",
                                      ifelse(INH=="Resistant" & RIF=="Resistant","INH and RIF multi-resistant",
                                             ifelse(INH=="Indeterminate/Error" |RIF=="Indeterminate/Error","Indeterminate/Error",
                                                    ifelse(INH=="Missing" | RIF=="Missing","Missing",NA)))))))

combine$Outcome<-factor(combine$Outcome,levels=c("Not Resistant","INH, Mono-resistant","RIF, mono-resistant","INH and RIF multi-resistant","Indeterminate/Error","Missing"))
combine$Country<-factor(combine$Country,levels=c("South Africa","China"))


### footnote numbers
Dropout_pDST<-filter(combine, INH=="Resistant" | RIF=="Resistant")
YY<-nrow(Dropout_pDST)
XX<-nrow(combine)-YY
ZZ<-nrow(INREPORT_table4)-nrow(combine)

# ZZ_PID <-INREPORT_table4$SUBJID[!INREPORT_table4$SUBJID %in% combine$SUBJID]
# write.csv(ZZ_PID,here("data/Table4_pending_PID.csv"))

### Generate final table
TAB<-as.data.frame.matrix(table(combine$Outcome,combine$Country))
TAB<-rownames_to_column(TAB,var="Country")

PCT<-as.data.frame.matrix(round(prop.table(table(combine$Outcome,combine$Country),2)*100,1))
names(PCT)<-c("SA_PCT","CHN_PCT")

TAB<-cbind(TAB,PCT)%>%
  mutate(SA=paste0(`South Africa`," (",SA_PCT,"%)"),
         CHN=paste0(China," (",CHN_PCT,"%)"))%>%
  select(Country, SA, CHN)


final<-rbind(c("Overall",colSums(table(combine$Outcome,combine$Country))),TAB)

createSheet(wb, name = "Table4_PID")
writeWorksheet(wb, combine, sheet = "Table4_PID")


#####################
##
## Generate final output
####################

myft_table4<-flextable(final)%>%
  theme_box()%>%
  bg(bg="grey70", part="header")%>%
  font(fontname = "Arial", part="all")%>%
  fontsize(size=10, part="all")%>%
  
  width(j=1:3,width=c(2,2,2))%>%
  align(j=1,align="left", part="all")%>%
  align(j=2:3,align="center", part="all")%>%
  set_header_labels(values=list(Country = "Country",
                                SA = "South Africa",
                                CHN = "China"))%>%
  add_footer_lines(values=paste0("(1) Table numbers reflect ",XX," participants who are active and completed the study as well as ",YY," participants who discontinued due to pDST resistance.\n(2) Data are pending for ",ZZ," participants."))




