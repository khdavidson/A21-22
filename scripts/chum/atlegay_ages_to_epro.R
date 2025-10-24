# Chum scales Atlegay --> EPRO 
# Oct 2025 


# Set up ---------------
library(tidyverse)
"%notin%" <- Negate("%in%")


# Find chum ages ---------------
#   May not exactly work year to year depending on how file is saved... 
atlegay <- readxl::read_excel(path=list.files(path="//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/SC_BioData_Management/6-Scale/Age_Results/Atlegay-InStream",
                                              pattern="^A-TlegayChum",
                                              full.names=T),
                              sheet=2) %>%
  janitor::clean_names() %>%
  mutate(r_book_no = paste0(scale_book_no, sep="-", fish_no),
         age_eu = case_when(age_eu %in% c(".03", "0..3", "0.3.", "00.3", "03", "3") ~ "0.3",
                            age_eu %in% c("02") ~ "0.2",
                            age_eu=="?" ~ NA,
                            TRUE ~ age_eu))



# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

#                                                                     Summarize for Nitinat/Hobiton


# Pull out cases where the duplicated age reads conflict (Nitinat only) ---------------
a22.cm.conflicts <- atlegay %>% 
  filter(grepl("Hobiton|Nitinat", sample_location_name, ignore.case=T), !grepl("ud|ds|rg|wd|ns|rd|dm", age_eu, ignore.case=T), !is.na(age_eu)) %>% 
  group_by(r_book_no, age_eu) %>%
  summarize(n=n()) %>%
  pivot_wider(names_from = age_eu, values_from = n) %>%
  ungroup() %>%
  mutate(conflicting_ages = rowSums(!is.na(select(., `0.2`:`0.1`))) > 1) %>% 
  filter(conflicting_ages=="TRUE") %>%
  pull(r_book_no)
  

# Assign an updated age back to the database for these cases ---------------
a22.cm.resolved <- atlegay %>% 
  filter(grepl("Hobiton|Nitinat", sample_location_name, ignore.case=T), !grepl("ud|ds|rg|wd|ns|rd|dm", age_eu, ignore.case=T), !is.na(age_eu)) %>% 
  mutate(resolved_age_eu = case_when(r_book_no %in% a22.cm.conflicts ~ NA,
                                     TRUE ~ age_eu))


# Check that the resolved ages worked: 
View(a22.cm.resolved %>%
       select(r_book_no, age_eu, resolved_age_eu) %>%
       filter(is.na(resolved_age_eu) & !is.na(age_eu)) %>%
       group_by(r_book_no) %>%
       summarize(resolved_age_eu=unique(resolved_age_eu), age_eu=unique(age_eu)))
# ^ these book numbers should be the same as a22.cm.conflicts



# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

#                                                                             Clean for EPRO


# Summarize in a way that is helpful for populating EPRO template ---------------
# First entries with a real age 
a22.cm.ages <- a22.cm.resolved %>%
  filter(!grepl("ud|ds|rg|wd|ns|rd|dm", age_eu, ignore.case=T), !is.na(resolved_age_eu)) %>%
  group_by(r_book_no, scale_book_no, fish_no) %>%
  summarize(resolved_age_eu = unique(resolved_age_eu)) %>%
  print()


# Entries with error codes
a22.cm.noages <- a22.cm.resolved %>%
  filter(grepl("ud|ds|rg|wd|ns|rd|dm", age_eu, ignore.case=T)) %>%
  group_by(r_book_no, scale_book_no, fish_no) %>%
  summarize(resolved_age_eu = unique(resolved_age_eu), age_comments = paste0(age_comments, collapse=" / ")) %>%
  print()

#**** here next day - why isn't this working??? why won't filter work 


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

#                                                                                EXPORT


# To DFO Network drive ---------------
writexl::write_xlsx(x=a22.cm.resolved, path=paste0("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/SC_BioData_Management/6-Scale/Age_Results/Atlegay-InStream/R_OUT - Area22_Chum_from-Atlegay-for-EPRO_",
                                                   Sys.Date(),
                                                   ".xlsx"))


# To github repo ---------------
writexl::write_xlsx(x=a22.cm.resolved, path=paste0(here::here("outputs"), 
                                                   "/R_OUT - Area22_Chum_from-Atlegay-for-EPRO_",
                                                   Sys.Date(),
                                                   ".xlsx"))
