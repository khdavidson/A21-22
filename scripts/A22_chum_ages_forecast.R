
# Summarizing Atlegay age file for age composition 
# to update WCVI Chum Forecast file


# Load library and helpers ----------------
library(tidyverse)
"%notin%" <- Negate("%in%")
analysis_year <- 2024



# Read data from network ----------------
# File name changes a lot so may have to tweak "pattern" argument 
chum_ages <- readxl::read_excel(path=list.files(path="//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/SC_BioData_Management/6-Scale/Age_Results/Atlegay/",
                                                pattern="A-TlegayChum_up-to-2024", full.names = T),
                                sheet=2)
                                  
                                  

############################################################################################################################################################

# Age summary - finescale (see if there are major differences b/w Hobiton and Nitinat)

Area22_chum_ages_FINE <- chum_ages %>% 
  filter(PROJECTYEAR==analysis_year, StatArea=="22") %>% 
  pivot_wider(names_from = SCALE_NO, values_from = AGE_EU) %>% 
  filter(!(`1`!=`2`), `1` %notin% c("NS", "UD", "DS", "RG")) %>%
  group_by(ProjectName, LocationName, `1`) %>%
  summarize(n=n()) %>% 
  group_by(ProjectName, LocationName) %>%
  mutate(total=sum(n),
         propn=n/total) %>% 
  rename(EU_Age = `1`) %>%
  print()




############################################################################################################################################################

# Age summary - coarse (all results pooled for forecast file)

Area22_chum_ages_COARSE <- chum_ages %>% 
  filter(PROJECTYEAR==analysis_year, StatArea=="22") %>% 
  pivot_wider(names_from = SCALE_NO, values_from = AGE_EU) %>% 
  filter(!(`1`!=`2`), `1` %notin% c("NS", "UD", "DS", "RG")) %>%
  group_by(`1`) %>%
  summarize(n=n()) %>% 
  ungroup() %>%
  mutate(total=sum(n),
         propn=n/total) %>% 
  rename(EU_Age = `1`) %>%
  print()
    



############################################################################################################


# ================== EXPORT ==================

# Create readme ---------------------------
readme <- data.frame(`1` = c("date rendered:", 
                             "source R code:", 
                             "source data file:"
                             ),
`2` = c(as.character(Sys.Date()), 
        "github...", 
        "SCD_Stad/SC_BioData_Management/6-Scale/Age_Results/Atlegay/A-TlegayChum_up-to-2024_sent5Mar2025.xlsx"
        )
)


# Create empty workbook ---------------------------
R_OUT.Area22ChumAges <- openxlsx::createWorkbook()


# Add empty tabs to the workbook ---------------------------
openxlsx::addWorksheet(R_OUT.Area22ChumAges, "readme")
openxlsx::addWorksheet(R_OUT.Area22ChumAges, "Area 22 finescale ages")
openxlsx::addWorksheet(R_OUT.Area22ChumAges, "Area 22 forecast age roll-up")


# Write data to tabs ---------------------------
openxlsx::writeData(R_OUT.Area22ChumAges, sheet="readme", x=readme)
openxlsx::writeData(R_OUT.Area22ChumAges, sheet="Area 22 finescale ages", x=Area22_chum_ages_FINE)
openxlsx::writeData(R_OUT.Area22ChumAges, sheet="Area 22 forecast age roll-up", x=Area22_chum_ages_COARSE)



# ================== Export ================== 
# To github repo ---------------------------
openxlsx::saveWorkbook(R_OUT.Area22ChumAges, 
                       file=paste0(here::here("outputs"), 
                                   "/R_OUT - Area 22 Chum ages ",
                                   analysis_year,
                                   ".xlsx"),
                       overwrite=T,
                       returnValue=T)

# copy to sharepoint manually afterwards 


# To DFO Network drive ---------------------------
# openxlsx::saveWorkbook(R_OUT_EPRO.NPAFC, 
#                        file=paste0("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/EPROcompile_base-files/2-Export-from-R",
#                                    "/R_OUT - All Adult Biosampling ALL FACILITIES WITH RESULTS ",
#                                    min(wcviCNepro_w_Results$`(R) RETURN YEAR`),
#                                    "-",
#                                    max(wcviCNepro_w_Results$`(R) RETURN YEAR`),
#                                    ".xlsx"),
#                        overwrite=T,
#                        returnValue=T)






