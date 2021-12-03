
INREPORT_6.2 <- INREPORT%>%
  filter(Arm !="Pending Assignment")

bycountry<-as.data.frame.matrix(addmargins(table(INREPORT_6.2$Country,INREPORT_6.2$Arm)))
bycountry<-bycountry%>%
  rownames_to_column()%>%
  mutate(armA=paste0(`Arm A`," (",round(100*`Arm A`/Sum,0),"%)"))%>%
  mutate(armBC=paste0(`Arm B & Arm C`," (",round(100*`Arm B & Arm C`/Sum,0),"%)"))%>%
  mutate(rowname=ifelse(rowname=="Sum","Total", rowname))%>%
  select(rowname, Sum, armA, armBC)%>%
  .[c(2,1,3),]



myft_table6.2<-flextable(bycountry)%>%
  theme_box()%>%
  bg(bg="grey70", part="header")%>%
  font(fontname = "Arial", part="all")%>%
  fontsize(size=10, part="all")%>%
  
  width(j=1:4,width=c(2,2,2,2))%>%
  colformat_double(j=c("Sum"), digits=0)%>% 
  align(j=2:4,align="center", part="all")%>%
  align(j=1,align="left", part="all")%>%
  set_header_labels(values=list(rowname = "  ",
                                Sum = "Total\nN",
                                armA = "Arm A\nN(%)",
                                armBC = "Arm B/C\nN (%)"))

