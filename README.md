# PredictTB_Code
This repository copntains R code to generate tables for the PredictTB study. 

The main data collected from the study is not available here; therefore the R code is provided only to give a detailed record of how the tables were created, and without the data file that code cannot be run to recreate any tables. 

## Overview

**Program/DSMB 8th** folders contains R scripts used to generate open tables for the 8th DSMB meeting. R scripts were named according to the table names. 
`Combine_all_Open.R` is the master script, which will source and invoke all the other scripts, among which `confirm_total_enroll_withdrawal.R` needs to be sourced first to obtain analysis data used in later steps.

## Update

This repository will keep being updated to include analysis code if applicable. 
