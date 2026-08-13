#################################################
###     BIOCLIMATICS V2.0                     ###
#################################################

# 2026-08-05
# This script replaces Ecosystems_Climate_Data.R (written by [ask Trevor name]) with updated calculations from Maria Gaetani (CEMML)
# written by Annie Kellner for CEMML (annie.kellner@colostate.edu)

# Inputs: AllDays (R object)


##  -----------  BEGIN SCRIPT --------------  ##

## Modify AllDays df

conflicts_prefer(month::lubridate) # set conflict preferences


# Create function for adding month (e.g., "Jan") to dataframe

add_month <- function(df){
  df = df %>%
    mutate(month = month(df$date, label = TRUE, abbr = TRUE, locale = Sys.getlocale("LC_TIME")))
}

# loop to add month to dataframes

for(i in 1:length(AllDays)){
  AllDays[[i]] = add_month(AllDays[[i]])
  AllDays[[i]]$month = as.character(AllDays[[i]]$month)
}


##  ----   BioClimatics Variables ----- ##


## Reorganize data

scenarioFuture_names <- names(AllDays) # same for AllDays & monthSum DF's


# Historical

in_days_historical <- AllDays[[which(grepl("baseline", scenarioFuture_names))]]
in_monthsum_historical <- monthSumDF[[which(grepl("baseline", scenarioFuture_names))]]

# Near Moderate (ssp245_2021)

in_days_near_mod <- AllDays[[which(grepl("s1f1", scenarioFuture_names))]]
in_monthsum_near_mod <- monthSumDF[[which(grepl("s1f1", scenarioFuture_names))]]
in_diffhist_near_mod <- diffHist[[which(grepl("s1f1", names(diffHist)))]]

#- Far Moderate (ssp245_2051)

in_days_far_mod <- AllDays[[which(grepl("s1f2", scenarioFuture_names))]]
in_monthsum_far_mod <- monthSumDF[[which(grepl("s1f2", scenarioFuture_names))]]
in_diffhist_far_mod <- diffHist[[which(grepl("s1f2", names(diffHist)))]]

# Near High (ssp585_2021)

in_days_near_high <- AllDays[[which(grepl("s2f1", scenarioFuture_names))]]
in_monthsum_near_high <- monthSumDF[[which(grepl("s2f1", scenarioFuture_names))]]
in_diffhist_near_high <- diffHist[[which(grepl("s2f1", names(diffHist)))]]

# Far High (ssp585_2051)

in_days_far_high <- AllDays[[which(grepl("s2f2", scenarioFuture_names))]]
in_monthsum_far_high <- monthSumDF[[which(grepl("s2f2", scenarioFuture_names))]]
in_diffhist_far_high <- diffHist[[which(grepl("s2f2", names(diffHist)))]]


## MIGHT NOT NEED THIS

name_scenario_order_wmonth <- c("month", "historical", "near_mod", "far_mod", "near_high", "far_high")
name_scenario_order <- c("historical", "near_mod", "far_mod", "near_high", "far_high")

name_scenario_match_asis <- c(
  "historical" = "historical_1985 - 2014",# the spacing of the file names in the folder is different on DiffHist
  "near_mod" = "ssp245_2021 - 2050",
  "near_high" = "ssp585_2021 - 2050",
  "far_mod" =  "ssp245_2051 - 2080",
  "far_high" = "ssp585_2051 - 2080"
)

name_scenario_match_wordy_ordered_time <- c(
  "historical" = "Historical",
  "near_mod" = "Moderate Disruption Near Term",
  "near_high" = "High Disruption Near Term",
  "far_mod" =  "Moderate Disruption Far Term",
  "far_high" = "High Disruption Far Term"
)

name_scenario_match_wordy_ordered_flow <- c(
  "historical" = "Historical",
  "near_mod" = "Moderate Disruption Near Term",
  "far_mod" =  "Moderate Disruption Far Term",
  "near_high" = "High Disruption Near Term",
  "far_high" = "High Disruption Far Term"
)

## Generate tibbles by time increment (daily & monthly) and pivot to long form

# Daily

frankent_daily_00 <- mget(ls(pattern = "^in_days_")) |>
  imap(function(df, name) {
    prefix <- str_remove(name, "^in_days_")
    rename_with(df, \(x) str_c(prefix, "_", x), -date)
  }) |>
  reduce(full_join, by = "date") |> 
  select(date, 
         ends_with("TMeanF"), 
         ends_with("TMaxF"), 
         ends_with("TMinF"), 
         ends_with("PPT_in"))

daily_long <- frankent_daily_00 |> 
  pivot_longer(-c(date),
               names_to = c("scenario", "variable"),
               names_pattern = "(historical|near_mod|near_high|far_mod|far_high)_(.*)",
               values_to = "value"
  )


frankent_daily_wmonth <- frankent_daily_00 |> 
  mutate(month = month(date), 
         month_word = month(date, label = TRUE, abbr = FALSE)) |> 
  relocate(month, month_word, .after = date)

daily_long_wmonth <- frankent_daily_wmonth |> 
  pivot_longer(
    -c(date,month,month_word),
    names_to = c("scenario", "variable"),
    names_pattern = "(historical|near_mod|near_high|far_mod|far_high)_(.*)",
    values_to = "value"
  )


