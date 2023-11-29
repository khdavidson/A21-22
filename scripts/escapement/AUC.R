# AUC 
# Nov 2023


# Load libraries -----------------------
library(tidyverse)
library(here)
library(readxl)
library(stringr)


# Helpers -----------------------
analysis_year <- 2023


# Load data -----------------------
a22.surveys <- readxl::read_excel(paste0("//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/WCVI/ESCAPEMENT/Data/", analysis_year, "/SILs/Area 21-22/Nitinat River/A22_Nitinat_River_2023.xlsx")
                                  , sheet="Nitinat River raw SIL entry", trim_ws=T) %>% 
  # Add zero start date
  add_row(date = as.Date("2023-08-15"),
          system = "Nitinat River",
          species = "Chinook", 
          `adults live / habitat seen (# Est Live)` = 0) %>% 
  arrange(date) %>% 
  # Add zero end date (if needed - not needed in 2023 for CN because have a zero end survey)
  # add_row(date = as.Date("2023-11-15"),
  #         system = "Nitinat River",
  #         species = "Chinook", 
  #         `adults live / habitat seen (# Est Live)` = 0) %>% 
  group_by(date) %>%
  mutate(surveyNum = paste0(str_sub(system, 1, 3), sep="-", (cur_group_id()-1))) %>% 
  ungroup() %>%
  mutate_at("date", as.Date) %>%
  print()
  



####################################################################################################################################################################


# AUC function
a22.auc <- data.frame(i = unique(a22.surveys$surveyNum),
                      #t = rep("NULL", length(unique(a22.surveys$surveyNum))),
                      #f = rep("NULL", length(unique(a22.surveys$surveyNum)))
                      )

for (i in unique(a22.surveys[a22.surveys$species=="Chinook",]$surveyNum)) {
  a22.auc$t[i] <- a22.surveys[a22.surveys$species=="Chinook",]$date[i] - a22.surveys[a22.surveys$species=="Chinook",]$date[i]-1
  #a22.auc$f[i] <- a22.surveys$`adults live / habitat seen (# Est Live)`[i] + a22.surveys$`adults live / habitat seen (# Est Live)`
  
  
}


a22.surveys$date[2] - a22.surveys$date[1]










