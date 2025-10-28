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
                            TRUE ~ age_eu)) %>%
  mutate(across(c(catch_start, catch_end, catch_date), ~lubridate::ymd(.)))



# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

#                                                                     Deal with double-reads 


# 1. Pull out cases where there are conflicting age results from the multiple reads per fish (Nitinat/Hobiton only) ---------------
a22.cm.conflicts <- atlegay %>% 
  filter(grepl("Hobiton|Nitinat", sample_location_name, ignore.case=T), !grepl("ud|ds|rg|wd|ns|rd|dm", age_eu, ignore.case=T), !is.na(age_eu)) %>% 
  group_by(r_book_no, age_eu) %>%
  summarize(n=n()) %>%
  pivot_wider(names_from = age_eu, values_from = n) %>%
  ungroup() %>%
  mutate(conflicting_ages = rowSums(!is.na(select(., `0.2`:`0.1`))) > 1) %>% 
  filter(conflicting_ages=="TRUE") %>%
  pull(r_book_no)
  

# 2. Assign the samples with conflicting ages NA back in the full database (Nitinat/Hobiton) ---------------
a22.cm.resolved <- atlegay %>% 
  filter(grepl("Hobiton|Nitinat", sample_location_name, ignore.case=T)) %>% 
  mutate(resolved_age_eu = case_when(r_book_no %in% a22.cm.conflicts ~ NA,
                                     TRUE ~ age_eu),
         age_comments = case_when(r_book_no %in% a22.cm.conflicts ~ paste0(age_comments, " / conflicting age reads, result removed"),
                                  TRUE ~ age_comments))


# Check that the resolved ages worked: 
View(a22.cm.resolved %>%
       select(r_book_no, age_eu, resolved_age_eu) %>%
       filter(is.na(resolved_age_eu) & !is.na(age_eu)) %>%
       group_by(r_book_no) %>%
       summarize(resolved_age_eu=unique(resolved_age_eu), age_eu=unique(age_eu)))
# ^ these book numbers should be the same as a22.cm.conflicts (this is duplicated though to show the duplicate reads)



# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

#                                                                             Clean for EPRO


# ================== Summarize in a way that is helpful for populating EPRO template ==================

# First entries with a REAL (non-conflicting) age -------------------- 
a22.cm.ages <- a22.cm.resolved %>%
  filter(!grepl("ud|ds|rg|wd|ns|rd|dm", age_eu, ignore.case=T), !is.na(resolved_age_eu)) %>%
  group_by(r_book_no, scale_book_no, fish_no) %>%
  summarize(project_name = unique(project_name),
            projectyear = unique(projectyear),
            sample_location_name = unique(sample_location_name),
            species = unique(species),
            book_format = unique(book_format),
            scale_no = paste0(scale_no, collapse=" / "),
            # read_no = paste0(scale_no, collapse=" / "),   always read=1 for now
            catch_start = unique(as.Date(catch_start)),
            catch_end = unique(as.Date(catch_end)),
            catch_date = unique(as.Date(catch_date)),
            resolved_age_eu = unique(resolved_age_eu),
            age_comments = paste0(age_comments, collapse=" / "),
            #across(c(project_name:book_format, sample_no, read_no:age_gr, resorbed:resolved_age_eu), ~unique(.x)),
            #growth = paste0(growth, collapse=" / ")
            ) %>%
  mutate(age_comments = gsub(age_comments, pattern="NA", replacement=""),
         age_comments = case_when(age_comments==" / " ~ NA,
                                  TRUE ~ age_comments),
         from = "cm.ages") %>%
  ungroup() %>%
  print()
#2734

# Check: should be 0
a22.cm.ages %>%
  group_by(r_book_no) %>%
  summarize(n=n()) %>%
  filter(n>1)

aged_book_nos <- unique(a22.cm.ages$r_book_no)


# Entries where both reads resulted in error codes -------------------- 
a22.cm.noages <- a22.cm.resolved %>%
  filter(grepl("ud|ds|rg|wd|ns|rd|dm", age_eu, ignore.case=T) | is.na(resolved_age_eu) & !is.na(sample_no)) %>%
  group_by(r_book_no, scale_book_no, fish_no) %>%
  summarize(project_name = unique(project_name),
            projectyear = unique(projectyear),
            sample_location_name = unique(sample_location_name),
            species = unique(species),
            book_format = unique(book_format),
            scale_no = paste0(scale_no, collapse=" / "),
            # read_no = paste0(scale_no, collapse=" / "),   always read=1 for now
            catch_start = unique(as.Date(catch_start)),
            catch_end = unique(as.Date(catch_end)),
            catch_date = unique(as.Date(catch_date)),
            resolved_age_eu = paste0(resolved_age_eu, collapse=" / "),
            age_comments = paste0(age_comments, collapse=" / "),
            #across(c(project_name:book_format, sample_no, read_no:age_gr, resorbed:resolved_age_eu), ~unique(.x)),
            #growth = paste0(growth, collapse=" / ")
  ) %>%
  mutate(age_comments = gsub(age_comments, pattern="NA", replacement=""),
         age_comments = case_when(age_comments==" / " ~ "",
                                  TRUE ~ age_comments),
         from = "cm.noages") %>%
  ungroup() %>%
  print()
# 635

# Check: should be 0
a22.cm.noages %>%
  group_by(r_book_no) %>%
  summarize(n=n()) %>%
  filter(n>1)



# Join together -------------------- 
a22.cm.allrecords <- full_join(a22.cm.ages,
                               a22.cm.noages %>%
                                 filter(r_book_no %notin% aged_book_nos))
# 2911 

# Check: should be 0
View(
  a22.cm.allrecords %>%
  group_by(r_book_no) %>%
  summarize(n=n(), from=unique(from)) %>%
  filter(n>1)
)

# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

#                                                                                EXPORT


# To DFO Network drive ---------------
writexl::write_xlsx(x=a22.cm.allrecords, path=paste0("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/SC_BioData_Management/6-Scale/Age_Results/Atlegay-InStream/R_OUT - Area22_Chum_from-Atlegay-for-EPRO_",
                                                   Sys.Date(),
                                                   ".xlsx"))


# To github repo ---------------
writexl::write_xlsx(x=a22.cm.allrecords, path=paste0(here::here("outputs"), 
                                                   "/R_OUT - Area22_Chum_from-Atlegay-for-EPRO_",
                                                   Sys.Date(),
                                                   ".xlsx"))
