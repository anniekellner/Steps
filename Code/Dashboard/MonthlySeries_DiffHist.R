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

sid <- paste0(base$ScenID[1],"v",Diff$ScenID[1]) # the addition of [1] makes it a single value rather than a vector of 12

prd <- paste(base$Period[1], "vs.", Diff$Period[1], sep = " ")

  

monthlySeriesDiffHist[[1]] <- Diff %>%
  bind_rows(base) %>% 
  group_by(SITENAME, Month, MonthNum) %>%
  summarise(across(Pctl90_Prcp_in:VWETDAYS, ~diff(.))) %>%
  arrange(MonthNum) %>%
  ungroup()

monthlySeriesDiffHist[[1]]$ScenID = sid
monthlySeriesDiffHist[[1]]$Period = prd
monthlySeriesDiffHist[[1]]$Scenario <- "Observed Historical Climate Trend"


##  --- Compare Hist Modeled to Hist Observed (ScenID 4v2) ----    ##

base <- MonthlySeries %>%
  filter(ScenID == 4)

Diff <- MonthlySeries %>%
  filter(ScenID == 2)

sid <- paste0(base$ScenID[1],"v",Diff$ScenID[1])

prd <- paste(base$Period[1],"Modeled", "vs.", Diff$Period[1], "Observed", sep = " ")

monthlySeriesDiffHist[[2]] <- Diff %>%
  bind_rows(base) %>% 
  group_by(SITENAME, Month, MonthNum) %>%
  summarise(across(Pctl90_Prcp_in:VWETDAYS, ~diff(.))) %>%
  mutate(across(Pctl90_Prcp_in:VWETDAYS, ~round(., digits = 2))) %>%  
  arrange(MonthNum) %>%
  ungroup()

monthlySeriesDiffHist[[2]]$ScenID = sid
monthlySeriesDiffHist[[2]]$Period = prd
monthlySeriesDiffHist[[2]]$Scenario = prd


##  --  Compare all other Future/Scenario combos to Modeled Historical  -- ##

 # separate historical values for use in calculations

Diff <- MonthlySeries %>%
  filter(ScenID == 4)

# Loop to calculate scenarios 5 - 8

for(i in 5:8){ 
  
  base = MonthlySeries %>%
    filter(ScenID == i)
  
  sid <- paste0(base$ScenID[1],"v",Diff$ScenID[1])
  
  prd <- paste(base$Period[1], "vs.", Diff$Period[1], sep = " ")
  
  scenario <- paste(base$Scenario[1], "Change vs. Historical", sep = " ")
  
    df = Diff %>% 
    bind_rows(base) %>%
    group_by(SITENAME, Month, MonthNum) %>%
    summarise(across(Pctl90_Prcp_in:VWETDAYS, \(x) diff(x))) %>%
    arrange(MonthNum) %>%
    ungroup()
    
    df$Scenario = scenario
    df$ScenID = sid
    df$Period = prd
  
  monthlySeriesDiffHist[[i-2]] = df

  }

# After creating spreadsheet, add these:

monthlySeriesDiffHist_dash <- bind_rows(list(monthlySeriesDiffHist[[1]],
                                monthlySeriesDiffHist[[2]],
                                monthlySeriesDiffHist[[3]],
                                monthlySeriesDiffHist[[4]],
                                monthlySeriesDiffHist[[5]],
                                monthlySeriesDiffHist[[6]]))

monthlySeriesDiffHist_dash$Scenario <- factor(
  monthlySeriesDiffHist_dash$Scenario, 
  levels = c(
    "Observed Historical Climate Trend",
    "1985 to 2014 Modeled vs. 1985-2014 Observed",
    "Moderate Emissions (SSP2-4.5) Change vs. Historical",
    "High Emissions (SSP5-8.5) Change vs. Historical")
)

monthlySeriesDiffHist_dash <- monthlySeriesDiffHist_dash %>%
  mutate(across(Pctl90_Prcp_in:VWETDAYS, ~ round(., digits = 2))) %>% 
  rename_with(~paste0(., "_diff"), .cols = Pctl90_Prcp_in:VWETDAYS)
  
  
# reorder columns

monthlySeriesDiffHist_dash <- monthlySeriesDiffHist_dash %>%
  select(SITENAME,
         Scenario,
         Period,
         ScenID, 
         Month,
         MonthNum, 
         Pctl90_Prcp_in_diff: VWETDAYS_diff)


# ----    CALCULATE 30-YEAR AVERAGES  ------  #

avg30 <- monthlySeriesDiffHist_dash %>%
  select(-c(MonthNum, Scenario, Period)) %>%
  group_by(SITENAME, ScenID) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
  arrange(ScenID) %>%
  ungroup()

avg30 <- avg30 %>%
  mutate(across(where(is.numeric),  ~ round(., digits = 2))) %>%
  mutate(Period = c("1991 to 2020 vs. 1981-2010",
                    "Modeled 1985 to 2014 vs. Observed 1985 - 2014",
                    "2021 to 2050 vs. Modeled Historical",
                    "2051 to 2080 vs. Modeled Historical",
                    "2021 to 2050 vs. Modeled Historical",
                    "2051 to 2080 vs. Modeled Historical")
                    )

avg30 <- select(avg30,
                SITENAME,
                ScenID,
                Period,
                Pctl90_Prcp_in_diff: VWETDAYS_diff)

# Save .csv files

write.csv(monthlySeriesDiffHist_dash, file = paste(dash_dir, "30yr_Avgs_Diffs.csv", sep = "/"))
write.csv(avg30, file = paste(dash_dir, "30yr_Diffs.csv", sep = "/"))


