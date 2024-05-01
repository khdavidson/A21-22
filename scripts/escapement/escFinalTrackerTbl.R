# esacpement tracking table 

library(tidyverse)
library(writexl)


analysis_year <- 2023

# Create empty table 
a2122tbl <- rbind(
  data.frame(
    year=analysis_year,
    area="21",
    system=c("Klanawa"),
    species=c("Chinook", "Coho", "Chum", "Sockeye", "Pink")) %>% 
    complete(year, area, system, species) %>%
    mutate(Access_DB = NA,
           NewEscIndex = NA,
           WCVIStreamSummary = NA,
           ReleasedToSDU = NA),
  data.frame(
    year=analysis_year,
    area="22",
    system=c("Hobiton", "Doobah", "Caycuse", "Campus", "Nitinat", "NoName", "Parker", "Worthless", "Jasper", "Little Nitinat"),
    species=c("Chinook", "Coho", "Chum", "Sockeye", "Pink")) %>% 
    complete(year, area, system, species) %>%
    mutate(Access_DB = NA,
           NewEscIndex = NA,
           WCVIStreamSummary = NA,
           ReleasedToSDU = NA)
)



# Export
write_xlsx(a2122tbl, path=paste0("//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/WCVI/ESCAPEMENT/Data/",
                               analysis_year,
                               "/SILs/Area 21-22/Area 21-22 Final escapement tracker ",
                               analysis_year,
                               ".xlsx"
                               ))

# Locked for editing until 2024! 

