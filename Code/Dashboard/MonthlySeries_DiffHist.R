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
  

monthlySeriesDiffHist[[1]] <- Diff %>%
  bind_rows(base) %>% 
  group_by(MonthNum) %>%
  summarise(across(Pctl90_Prcp_in:VWETDAYS, ~diff(.))) %>%
  mutate(across(Pctl90_Prcp_in:VWETDAYS, ~round(., digits = 2))) %>%  
  arrange(MonthNum) %>%
  ungroup()
           

##  --- Compare Hist Modeled to Hist Observed (ScenID 4v2) ----    ##

base <- MonthlySeries %>%
  filter(ScenID == 4)

Diff <- MonthlySeries %>%
  filter(ScenID == 2)

monthlySeriesDiffHist[[2]] <- Diff %>%
  bind_rows(base) %>% 
  group_by(MonthNum) %>%
  summarise(across(Pctl90_Prcp_in:VWETDAYS, ~diff(.))) %>%
  mutate(across(Pctl90_Prcp_in:VWETDAYS, ~round(., digits = 2))) %>%  
  arrange(MonthNum) %>%
  ungroup()

##  --  Compare all other Future/Scenario combos to Modeled Historical  -- ##

 # separate historical values for use in calculations

Diff <- MonthlySeries %>%
  filter(ScenID == 4)

# Loop to calculate scenarios 5 - 8

for(i in 5:8){ 
  
  base = MonthlySeries %>%
    filter(ScenID == i)
  
    df = Diff %>% 
    bind_rows(base) %>%
    group_by(MonthNum) %>%
    summarise(across(Pctl90_Prcp_in:VWETDAYS, \(x) diff(x))) %>%
    mutate(across(Pctl90_Prcp_in:VWETDAYS, ~round(., digits = 2))) %>% 
    arrange(MonthNum) %>%
    ungroup()
  
  monthlySeriesDiffHist[[i-2]] = df

  }
  
  
    
  
  diffHist[[i-1]] = diff # add to DiffHist dataframe for future use
  names(diffHist)[[i-1]] = names(monthSumDF[i])
}



base <- MonthlySeries %>%
  filter(ScenID == 5)

Diff <- MonthlySeries %>%
  filter(ScenID == 4)

monthlySeriesDiffHist[[3]] <- Diff %>%
  bind_rows(base) %>% 
  group_by(MonthNum) %>%
  summarise(across(Pctl90_Prcp_in:VWETDAYS, ~diff(.))) %>%
  mutate(across(Pctl90_Prcp_in:VWETDAYS, ~round(., digits = 2))) %>%  
  arrange(MonthNum) %>%
  ungroup()
