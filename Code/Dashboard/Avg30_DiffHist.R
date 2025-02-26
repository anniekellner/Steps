#######################################################################
###         30-year Diffs (Overall Averages)                        ###
#######################################################################

# This script compares the overall averages of a suite of climate variables 
  # among 30-year time periods

# written for the Climate Viewer by Annie Kellner (annie.kellner@colostate.edu)

# Inputs:
  # avg30 dataframe

# Outputs:
  # .csv file for Dashboard input

### BEGIN SCRIPT

avg30 <- avg30 %>%
  select(!SCENARIO)

diffs30 <- list()


##  ---  Compare Observed Historical PERIOD (ScenID 3v2)  --- #

base <- avg30 %>%
  filter(ScenID == 3)

Diff <- avg30 %>%
  filter(ScenID == 1)

sid <- paste0(base$ScenID[1],"v",Diff$ScenID[1]) # the addition of [1] makes it a single value rather than a vector of 12

prd <- paste(base$PERIOD[1], "vs.", Diff$PERIOD[1], sep = " ")



diffs30[[1]] <- Diff %>%
  bind_rows(base) %>% 
  group_by(SITENAME) %>%
  summarise(across(Pctl90_Prcp_in:VWETDAYS, ~diff(.))) %>%
  ungroup()

diffs30[[1]]$ScenID = sid
diffs30[[1]]$PERIOD = prd

##  ---   Compare Modeled Hist to Observed Hist   --- ##

base <- avg30 %>%
  filter(ScenID == 4)

Diff <- avg30 %>%
  filter(ScenID == 2)

sid <- paste0(base$ScenID[1],"v",Diff$ScenID[1]) # the addition of [1] makes it a single value rather than a vector of 12

prd <- paste("Modeled", base$PERIOD[1], "vs.", "Observed", Diff$PERIOD[1], sep = " ")


diffs30[[2]] <- Diff %>%
  bind_rows(base) %>% 
  summarise(across(Pctl90_Prcp_in:VWETDAYS, ~diff(.))) %>%
  ungroup()

diffs30[[2]]$ScenID = sid
diffs30[[2]]$PERIOD = prd

##  ---   Futures vs. Modeled Historical  ----- ##

Diff = avg30 %>%
  filter(ScenID == 4)

# Loop to calculate scenarios 5 - 8

for(i in 5:8){ 
  
  base = avg30 %>%
    filter(ScenID == i)
  
  sid <- paste0(base$ScenID[1],"v",Diff$ScenID[1])
  
  prd <- paste(base$PERIOD[1], "vs.", "Modeled Historical", sep = " ")
  
  df = Diff %>% 
    bind_rows(base) %>%
    summarise(across(Pctl90_Prcp_in:VWETDAYS, \(x) diff(x))) %>%
    ungroup()
  
  df$ScenID = sid
  df$PERIOD = prd
  
  diffs30[[i-2]] = df
  
}

# After creating spreadsheet, add these:

diffs30_Dash <- bind_rows(list(diffs30[[1]],
                          diffs30[[2]],
                          diffs30[[3]],
                          diffs30[[4]],
                          diffs30[[5]],
                          diffs30[[6]]))


diffs30_Dash$SITENAME <- official_name

diffs30_Dash <- diffs30_Dash %>%
  mutate(across(Pctl90_Prcp_in:VWETDAYS, ~ round(., digits = 2))) %>% 
  rename_with(~paste0(., "_diff"), .cols = Pctl90_Prcp_in:VWETDAYS)

diffs30_Dash <- diffs30_Dash %>%
  mutate(SCENARIO = case_when(
    ScenID == "3v1" ~ "Observed Historical Climate Trend Annual",
    ScenID == "4v2" ~ "Modeled Historical Period vs. Observed Historical Period",
    ScenID == "5v4" | ScenID == "6v4" ~ "SSP2-4.5 vs. Modeled Historical",
    ScenID == "7v4" | ScenID == "8v4" ~  "SSP5-8.5 vs. Modeled Historical"
  ))
  

# reorder columns

diff30_Dash <- diffs30_Dash %>%
  select(SITENAME,
         SCENARIO,
         ScenID, 
         PERIOD,
         Pctl90_Prcp_in_diff: VWETDAYS_diff)


# Write to csv

dash_dir <- paste(results_folder,"Dashboard",sep = "/")

if (!dir.exists(dash_dir)){
  dir.create(dash_dir)}

write.csv(diffs30_Dash, file = paste(dash_dir, "30yr_Annual_Diffs.csv", sep = "/"))

