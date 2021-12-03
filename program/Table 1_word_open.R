

#
# Generate final table
#
outtab<-as.data.frame(table(INREPORT$Arm))%>%
  full_join(as.data.frame(table(INREPORT$Arm[INREPORT$Country=="China"])),by="Var1")%>%
  full_join(as.data.frame(table(INREPORT$Arm[INREPORT$site=="21"])),by="Var1")%>%
  full_join(as.data.frame(table(INREPORT$Arm[INREPORT$site=="22"])),by="Var1")%>%
  full_join(as.data.frame(table(INREPORT$Arm[INREPORT$site=="23"])),by="Var1")%>%
  full_join(as.data.frame(table(INREPORT$Arm[INREPORT$site=="24"])),by="Var1")%>%
  full_join(as.data.frame(table(INREPORT$Arm[INREPORT$site=="25"])),by="Var1")%>%
  full_join(as.data.frame(table(INREPORT$Arm[INREPORT$Country=="South Africa"])),by="Var1")%>%
  full_join(as.data.frame(table(INREPORT$Arm[INREPORT$site=="11"])),by="Var1")%>%
  full_join(as.data.frame(table(INREPORT$Arm[INREPORT$site=="12"])),by="Var1")%>%
  full_join(as.data.frame(table(INREPORT$Arm[INREPORT$site=="13"])),by="Var1")%>%
  full_join(as.data.frame(table(INREPORT$Arm[INREPORT$site=="14"])),by="Var1")%>%
  full_join(as.data.frame(table(INREPORT$Arm[INREPORT$site=="15"])),by="Var1")%>%
  column_to_rownames(var="Var1")

dta<-as.data.frame(t(rbind(outtab,colSums(outtab,na.rm=TRUE))))%>%
  mutate(Group=c("Total # of Participants in Study","Total # of Participants in China","\tKaifeng","\tXinmi","\tZhongmu","\tHPCH","\tXinxiang",
                 "Total # of Participants in South Africa","\tCIDRI/Khayelitsha Site B","\tSATVI","\tSUN","\tTASK","\tUCT Lung"))%>%
  mutate(`Pending Assignment`=ifelse(is.na(`Pending Assignment`),0,`Pending Assignment`),
         `Arm A`=ifelse(is.na(`Arm A`),0, `Arm A`),
         `Arm B & Arm C`=ifelse(is.na(`Arm B & Arm C`),0, `Arm B & Arm C`))%>%
  select(Group, All=`4`,Pending=`Pending Assignment`,ArmA=`Arm A`,ArmBC=`Arm B & Arm C`)

createSheet(wb, name = "Table1_PID")
writeWorksheet(wb, INREPORT%>%select(SUBJID, Country, site, Arm), sheet = "Table1_PID")


#
# table formatting
#

myft_table1<-flextable(dta)%>%
  theme_box()%>%
  bg(bg="grey70", part="header")%>%
  font(fontname = "Arial", part="all")%>%
  fontsize(size=10, part="all")%>%
  
  colformat_double(j=c("All","Pending","ArmA","ArmBC"), digits=0)%>% 
  width(j=1:5,width=c(2.5,1.5,1.5,1.5,1.5))%>%
  align(j=2:5,align="center", part="all")%>%
  set_header_labels(values=list(Group = " ",
                                All = "All\n(N)",
                                Pending = "Pending Assignment*\nto A or B/C\n(N)",
                                ArmA = "Arm A\n(N)",
                                ArmBC = "Arm B & Arm C\n(N)"))%>%
  
  add_footer_lines(values = "(1) Table numbers reflect participants who are currently active or have completed the study. Withdrawals and participants who were lost to follow up and without arm assignment before the week 16 randomization visit are excluded from this table.\n(2) China Sites: Kaifeng= Kaifeng City Institute of Tuberculosis Prevention and Control; Xinmi= Xinmi City Center for Disease Control and Prevention; Zhongmu= Zhongmu County Station for Disease Control and Prevention; HPCH= Henan Provincial Chest Hospital;Xinxiang= Xinxiang City Institute of Tuberculosis Prevention and Control.\n(3) South Africa Sites: CIDRI=Khayelitsha Site B; SATVI= South African Tuberculosis Vaccine Initiative; SUN= Stellenbosch University; TASK= TASK Applied Science; \nUCT Lung=University of Cape Town Lung Institute.\n*Due to data not received from China")



