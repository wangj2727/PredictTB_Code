### Project: PredictTB
### Purpose: Create DSMB open tables
### Note: This is the master script, which will invoke all the other scripts to create the final output
### Date: Data lock date: Nov 5th 2021; Program finalized date: Dec 1st 2021;
### Programmer: Jing Wang


library(dplyr)
library(tidyr)
library(tibble)
library(stringr)
library(knitr)
library(rmarkdown)
library(flextable)
library(officer)
library(here)
library(XLConnect)



indir_Datafax<-"C:\\Users\\wangj27\\Desktop\\DSMB 8th\\data\\export_64_decoded\\"
prgdir<-"C:\\Users\\wangj27\\Desktop\\DSMB 8th\\program\\open\\"

myInputData<-function(platenum){
  plate<-gsub("[^[:alnum:][:blank:]+?&/,.;:|_]", " ", readLines(paste0(indir_Datafax,"plate",platenum,".dat")))
  plate<-read.table(textConnection(plate),sep="|",header=T,stringsAsFactors = F)
  names(plate)<-gsub(paste0("[_00]?",platenum),"",names(plate))
  names(plate)<-gsub(paste0(as.numeric(platenum)),"",names(plate))
  return(plate)
}

source(paste0(prgdir,"confirm_total_enroll_withdrawal.R"))


wb <- loadWorkbook(here("output/DSMB8th_PIDList_v2.xlsx"), create = TRUE)

source(paste0(prgdir,"Table 1_word_open.R"))
source(paste0(prgdir,"Table 2_word_open.R"))
source(paste0(prgdir,"Table 3_word_open.R"))
source(paste0(prgdir,"Table 4_word_open.R"))  
source(paste0(prgdir,"Table 5_word_open.R"))
source(paste0(prgdir,"Table 6.1_word_open.R"))
source(paste0(prgdir,"Table 6.2_word_open.R"))
source(paste0(prgdir,"Table 7_word_open.R"))
source(paste0(prgdir,"Table 8_word_open.R"))
source(paste0(prgdir,"Table 9_word_open.R"))
source(paste0(prgdir,"Table 10_word_open.R"))
source(paste0(prgdir,"Table 11_word_open.R"))
source(paste0(prgdir,"Table 12_word_open.R"))
source(paste0(prgdir,"Table 14_word_open.R"))
source(paste0(prgdir,"Table 15_word_open.R"))
source(paste0(prgdir,"AppendixA_word_open.R"))
source(paste0(prgdir,"AppendixB_word_open.R"))
source(paste0(prgdir,"AppendixC_word_open.R"))

saveWorkbook(wb)

my_doc_Alltable<-read_docx()%>%
  body_add_par("Table 1 - Summary of Current Enrollment Status.", style = "heading 2")%>%
  slip_in_text(" Number of participants currently on the study (excludes withdrawals) is shown overall and per arm per site.", style="strong")%>%
  body_add_flextable(myft_table1)%>%
  body_end_section_landscape()%>%
  
  
  body_add_par("Table 2a - Summary of Baseline Characteristics - Overall.", style = "heading 2")%>%
  slip_in_text(" Baseline characteristics of all participants on the study by arm. Participants not yet assigned to an arm due to pending PET/CT scan review are also indicated.", style="strong")%>%
  body_add_flextable(myft_table2a)%>%
  body_end_section_landscape()%>%
  
  body_add_par("Table 2b - Summary of Baseline Characteristics - South Africa.", style = "heading 2")%>%
  slip_in_text(" Baseline characteristics of all participants on the study by arm. Participants not yet assigned to an arm due to pending PET/CT scan review are also indicated.", style="strong")%>%
  body_add_flextable(myft_table2b)%>%
  body_end_section_landscape()%>%
  
  body_add_par("Table 2c - Summary of Baseline Characteristics - China.", style = "heading 2")%>%
  slip_in_text(" Baseline characteristics of all participants on the study by arm. Participants not yet assigned to an arm due to pending PET/CT scan review are also indicated.", style="strong")%>%
  body_add_flextable(myft_table2c)%>%
  body_end_section_landscape()%>%
  
  
  
  body_add_par("Table 3 - Summary of Withdrawals-Reasons for not Completing the Study", style = "heading 2")%>%
  slip_in_text(" Overall withdrawals and withdrawals by country", style="strong")%>%
  body_add_flextable(myft_table3)%>%
  body_end_section_landscape()%>%
  
  
  
  body_add_par("Table 4: Baseline Drug-resistance.", style = "heading 2")%>%
  slip_in_text(" Phenotypic drug sensitivity testing (pDST) is performed on the isolate obtained from the screening or baseline sputum sample. If found resistant, the participant is withdrawn.", style="strong")%>%
  body_add_flextable(myft_table4)%>%
  body_end_section_landscape()%>%
  
  
  
  body_add_par("Table 5a - Attendance at Study Follow-up Visits - Overall.", style = "heading 2")%>%
  slip_in_text(" Breakdown of percent attendance at protocol-defined study visits. Participants may be called for an unscheduled visit to evaluate for TB recurrence after completion of treatment.", style="strong")%>%
  body_add_flextable(myft_table5a)%>%
  body_end_section_landscape()%>%
  
  
  body_add_par("Table 5b - Attendance at Study Follow-up Visits - South Africa.", style = "heading 2")%>%
  slip_in_text(" Breakdown of percent attendance at protocol-defined study visits. Participants may be called for an unscheduled visit to evaluate for TB recurrence after completion of treatment.", style="strong")%>%
  body_add_flextable(myft_table5b)%>%
  body_end_section_landscape()%>%
  
  
  body_add_par("Table 5c - Attendance at Study Follow-up Visits - China.", style = "heading 2")%>%
  slip_in_text(" Breakdown of percent attendance at protocol-defined study visits. Participants may be called for an unscheduled visit to evaluate for TB recurrence after completion of treatment.", style="strong")%>%
  body_add_flextable(myft_table5c)%>%
  body_end_section_landscape()%>%
  
  
  body_add_par("Table 6.1a - Arm Assignment by Criteria- Overall.", style = "heading 2")%>%
  slip_in_text(" The distribution of participants into the study arms is shown.", style="strong")%>%
  body_add_par(" ")%>% 
  body_add_flextable(myft_table6.1a)%>%
  body_end_section_landscape()%>%
  
  body_add_par("Table 6.1b - Arm Assignment by Criteria- South Africa.", style = "heading 2")%>%
  slip_in_text(" The distribution of participants into the study arms is shown.", style="strong")%>%
  body_add_par(" ")%>% 
  body_add_flextable(myft_table6.1b)%>%
  body_end_section_landscape()%>%
  
  body_add_par("Table 6.1c - Arm Assignment by Criteria- China.", style = "heading 2")%>%
  slip_in_text(" The distribution of participants into the study arms is shown.", style="strong")%>%
  body_add_par(" ")%>% 
  body_add_flextable(myft_table6.1c)%>%
  body_end_section_landscape()%>%
  
  
  
  body_add_par("Table 6.2 - Arm Assignment Summary.", style = "heading 2")%>%
  slip_in_text(" The balance between the study arms is shown.", style="strong")%>%
  body_add_par(" ")%>%
  body_add_flextable(myft_table6.2)%>%
  body_end_section_landscape()%>%
  
  
  
  body_add_par("Table 7a - Radiology Progress.", style = "heading 2")%>%
  slip_in_text(" Frequency of concordance and discordance between readers with respect to stratification of participants to Arm A and Arm B/C at baseline is shown.", style="strong")%>%
  body_add_flextable(myft_table7a)%>%
  body_end_section_landscape()%>%
  
  
  body_add_par("Table 7b - Radiology Progress.", style = "heading 2")%>%
  slip_in_text(" Frequency of concordance and discordance between readers with respect to stratification of participants to Arm A and Arm B/C at week 4 is shown.", style="strong")%>%
  body_add_par(" ")%>%
  body_add_flextable(myft_table7b)%>%
  body_end_section_landscape()%>%
  
  
  
  body_add_par("Table 8 - Adherence to Treatment.", style = "heading 2")%>%
  slip_in_text(" Percent doses taken are presented up to 24 weeks on the study.", style="strong")%>%
  body_add_par(" ")%>%
  body_add_flextable(myft_table8)%>%
  body_end_section_landscape()%>%
  
  
  
  body_add_par("Table 9a: Week 16 GeneXpert Ct.", style = "heading 2")%>%
  slip_in_text(" Comparison of average GeneXpert Ct value at week 16 in Arms A and B/C.", style="strong")%>%
  body_add_par(" ")%>% 
  body_add_flextable(myft_table9a)%>%
  body_end_section_landscape()%>%
  
  body_add_par("Table 9b: GeneXpert Ct Threshold vs LJ Culture Result at Week 16.", style = "heading 2")%>%
  body_add_par(" ")%>% 
  body_add_flextable(myft_table9b)%>%
  body_end_section_landscape()%>%
  
  body_add_par("Table 9c: GeneXpert Ct Threshold vs MGIT Culture Result at Week 16.", style = "heading 2")%>%
  body_add_par(" ")%>% 
  body_add_flextable(myft_table9c)%>%
  body_end_section_landscape()%>%
  
  
  
  body_add_par("Table 10a - LJ Culture Data - Overall.", style = "heading 2")%>%
  slip_in_text(" Results of LJ culture at various time points on the protocol are shown.", style="strong")%>%
  body_add_flextable(myft_table10a)%>%
  body_end_section_landscape()%>%
  
  body_add_par("Table 10b - LJ Culture Data - South Africa.", style = "heading 2")%>%
  slip_in_text(" Results of LJ culture at various time points on the protocol are shown.", style="strong")%>%
  body_add_flextable(myft_table10b)%>%
  body_end_section_landscape()%>%
  
  body_add_par("Table 10c - LJ Culture Data - China.", style = "heading 2")%>%
  slip_in_text(" Results of LJ culture at various time points on the protocol are shown.", style="strong")%>%
  body_add_flextable(myft_table10c)%>%
  body_end_section_landscape()%>%
  
  
  
  body_add_par("Table 11a - MGIT Culture Data - Overall.", style = "heading 2")%>%
  slip_in_text(" Results of MGIT culture at various time points on the protocol are shown.", style="strong")%>%
  body_add_flextable(myft_table11a)%>%
  body_end_section_landscape()%>%
  
  body_add_par("Table 11b - MGIT Culture Data - South Africa.", style = "heading 2")%>%
  slip_in_text(" Results of MGIT culture at various time points on the protocol are shown.", style="strong")%>%
  body_add_flextable(myft_table11b)%>%
  body_end_section_landscape()%>%
  
  body_add_par("Table 11c - MGIT Culture Data - China.", style = "heading 2")%>%
  slip_in_text(" Results of MGIT culture at various time points on the protocol are shown.", style="strong")%>%
  body_add_flextable(myft_table11c)%>%
  body_end_section_landscape()%>%
  
  
  
  body_add_par("Table 12 - Enrollment on the PK Sub-study.", style = "heading 2")%>%
  slip_in_text(" Number of cases and controls is shown.", style="strong")%>%
  body_add_par(" ")%>% 
  body_add_flextable(myft_table12)%>%
  body_end_section_landscape()%>%
  
  
  
  body_add_par("Table 14: Adverse Event Summary by Treatment Group.", style = "heading 2")%>%
  slip_in_text(" Adverse events in each treatment group are shown.", style="strong")%>%
  body_add_flextable(myft_table14)%>%
  body_end_section_landscape()%>%
  
  
  
  body_add_par("Table 15: Summary of Completed Visit by Arm.", style = "heading 2")%>%
  slip_in_text(" Number of participants who have completed treatment in each arm and subsequently completed study is shown.", style="strong")%>%
  body_add_flextable(myft_table15)%>%
  body_end_section_landscape()%>%
  

  
  body_add_par("Appendix A: Line Listing of Withdrawals", style = "heading 2")%>%
  slip_in_text("-Reasons for not Completing the Study.", style="strong")%>%
  body_add_par(" ")%>% 
  body_add_flextable(myft_AppA)%>%
  body_end_section_landscape()%>%
  
  
  
  body_add_par("Appendix B: Line Listing of Adverse Events", style = "heading 2")%>%
  body_add_par(" ")%>% 
  body_add_flextable(myft_AppB)%>%
  body_end_section_landscape()%>%
  
  
  
  body_add_par("Appendix C: Line Listing of Serious Adverse Events", style = "heading 2")%>%
  body_add_par(" ")%>% 
  body_add_flextable(myft_AppC)%>%
  body_end_section_landscape()

print(my_doc_Alltable, target = here("output/PredictTB_8thDSMB_tables_OPEN_SharedVersion.docx"))


