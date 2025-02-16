#####################################################
###       MONTHLY DIFFHIST LAYOUTS FOR DASHBOARD  ###
#####################################################

# written by Annie Kellner 2-16-2025

# This script compares all scenarios from the MonthlySeries 
  # dataframe and produces a .csv file for inputting into the
  # Climate Viewer Dashboard


### BEGIN SCRIPT

monthlySeriesDiffHist <- list()

##  ---  Compare Observed Historical Period (ScenID 3v1)  --- #

base <- MonthlySeries %>%
  filter(ScenID == 3)

Diff <- MonthlySeries %>%
  filter(ScenID == 1)
  
  # separate historical values for use in calculations

monthlySeriesDiffHist[[1]] <- base %>%
  bind_rows(Diff) %>%
  group_by(Month) %>%
  summarise(across(Pctl90_Prcp_in:VWETDAYS, ~diff(.))) %>%
  ungroup()
           

