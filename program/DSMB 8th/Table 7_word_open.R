

### Table 7a baseline
plate201<-myInputData("201")%>%
  select(SUBJID,R1.RISK=RIRISK)%>%
  filter(!is.na(R1.RISK) & R1.RISK!="Blank")

base<-myInputData("202")%>%
  select(SUBJID,R2.RISK=RIRISK)%>%
  filter(!is.na(R2.RISK) & R2.RISK!="Blank")%>%
  full_join(plate201,by="SUBJID")%>%
  filter(SUBJID %in% INREPORT$SUBJID)%>%
  filter(R1.RISK %in% c("Low","High") & R2.RISK %in% c("Low","High"))

out<-as.data.frame.matrix(table(base$R1.RISK,base$R2.RISK))

Table7a<-rbind(cbind(" "," ","Reader 1"),
               cbind("Reader 2","Low-Risk","High-Risk"),
               cbind("Low-Risk",out[2,2],out[1,2]),
               cbind("High-Risk",out[2,1],out[1,1]))


# calculate mismatch at baseline
mismatch_base<-base%>%
  filter(R1.RISK!=R2.RISK)

createSheet(wb, name = "Table7a_PID")
writeWorksheet(wb, base, sheet = "Table7a_PID")


### Table 7b Week4
### Note: only summarize participants who remain eligible for randomization after baseline imaging
plate2<-myInputData("002")%>%
  select(SUBJID,BASE_YN)

plate201<-myInputData("201")%>%
  select(SUBJID,BASE_R1=RIRISK)

plate202<-myInputData("202")%>%
  select(SUBJID,BASE_R2=RIRISK)

plate203<-myInputData("203")%>%
  select(SUBJID,BASE_R3=RIRISK)

plate213<-myInputData("213")%>%
  select(SUBJID,R3.RISK=RIRISK)%>%
  filter(!is.na(R3.RISK) & R3.RISK!="Blank")

plate211<-myInputData("211")%>%
  select(SUBJID,R1.RISK=RIRISK)%>%
  filter(!is.na(R1.RISK) & R1.RISK!="Blank")

plate212<-myInputData("212")%>%
  select(SUBJID,R2.RISK=RIRISK)%>%
  filter(!is.na(R2.RISK) & R2.RISK!="Blank")%>%
  full_join(plate211,by="SUBJID")%>%
  full_join(plate213,by="SUBJID")%>%
  full_join(plate2,by="SUBJID")%>%
  left_join(plate201,by="SUBJID")%>%
  left_join(plate202,by="SUBJID")%>%
  left_join(plate203,by="SUBJID")%>%
  filter(SUBJID %in% INREPORT$SUBJID)%>%
  filter(BASE_YN=="Yes")%>%
  filter((!is.na(R1.RISK) & !is.na(R2.RISK)) | (!is.na(R1.RISK) & !is.na(R3.RISK)) | (!is.na(R2.RISK) & !is.na(R3.RISK)))

out2<-plate212%>%
  mutate(RISK1=ifelse(!is.na(BASE_R1) & !is.na(BASE_R2) & BASE_R1==BASE_R2,R1.RISK,
                      ifelse(!is.na(BASE_R1) & !is.na(BASE_R3) & BASE_R1==BASE_R3,R1.RISK,R3.RISK)),
         RISK2=ifelse(!is.na(BASE_R1) & !is.na(BASE_R2) & BASE_R1==BASE_R2,R2.RISK,
                      ifelse(!is.na(BASE_R2) & !is.na(BASE_R3) & BASE_R2==BASE_R3,R2.RISK,R3.RISK)))%>%
  mutate(RISK2=ifelse(SUBJID==15013,"Low",RISK2))%>%
  filter(!is.na(RISK1) & !is.na(RISK2))

### special case 15013 (R1=R2=R3=Low-Risk at baseline, R1=R3=2, R2=NA at week4)


out2tab<-as.data.frame.matrix(table(out2$RISK1,out2$RISK2))

Table7b<-rbind(cbind(" "," ","Reader 1"),
               cbind("Reader 2","Low-Risk","High-Risk"),
               cbind("Low-Risk",out2tab[2,2],out2tab[1,2]),
               cbind("High-Risk",out2tab[2,1],out2tab[1,1]))

createSheet(wb, name = "Table7b_PID")
writeWorksheet(wb, out2%>%select(SUBJID, RISK1, RISK2), sheet = "Table7b_PID")


# calculate mismatch at week4
mismatch_week4<-out2%>%
  filter(RISK1!=RISK2)

#### compare tables between # of eligible participant to be randomized at 2k4, and
#    # of participants included in table 7b
RemainEli<-myInputData("002")%>%
  filter(SUBJID %in% INREPORT$SUBJID)%>%
  select(SUBJID,BASE_YN)%>%
  filter(BASE_YN=="Yes")  ## N=439, same as the number in table 6
diff <-anti_join(RemainEli, out2, by="SUBJID")
# filter(plate211, SUBJID %in% diff$SUBJID)
# filter(myInputData("212"), SUBJID %in% diff$SUBJID)
# filter(myInputData("213"), SUBJID %in% diff$SUBJID)

# following 5 participants have not had week4 image plates
#   SUBJID BASE_YN
# 1  12056     Yes
# 2  12093     Yes
# 3  13039     Yes
# 4  14130     Yes
# 5  15075     Yes


myft_table7a<-flextable(as.data.frame(Table7a)[-1,])%>%
  theme_box()%>%
  bg(bg="grey70", part="header")%>%
  font(fontname = "Arial", part="all")%>%
  fontsize(size=10, part="all")%>%
  bold(i=1,j=1,part="body")%>%
  merge_at(i=1,j=2:3, part="header")%>%
  width(j=1:3,width=c(2,2,2))%>%
  align(j=1,align="left", part="body")%>%
  align(j=2:3,align="center", part="all")%>%
  set_header_labels(values=list(V1 = " ",
                                V2 ="Reader 1"))%>%
  add_footer_lines(values = paste0(nrow(mismatch_base)," is the number of times a third reader was required."))



myft_table7b<-flextable(as.data.frame(Table7b)[-1,])%>%
  theme_box()%>%
  bg(bg="grey70", part="header")%>%
  font(fontname = "Arial", part="all")%>%
  fontsize(size=10, part="all")%>%
  bold(i=1,j=1,part="body")%>%
  merge_at(i=1,j=2:3, part="header")%>%
  width(j=1:3,width=c(2,2,2))%>%
  align(j=1,align="left", part="body")%>%
  align(j=2:3,align="center", part="all")%>%
  set_header_labels(values=list(V1 = " ",
                                V2 ="Reader 1"))

