#################################################
###     BIOCLIMATICS V2.0                     ###
#################################################

# 2026-08-05
# This script replaces Ecosystems_Climate_Data.R (written by [ask Trevor name]) with updated calculations from Maria Gaetani (CEMML)
# written by Annie Kellner for CEMML (annie.kellner@colostate.edu)

# Inputs: AllDays (R object)


##  -----------  BEGIN SCRIPT --------------  ##



conflicts_prefer(month::lubridate) # set conflict preferences

## Modify AllDays df

# Create function for adding month (e.g., "Jan") and year to dataframe

add_month <- function(df){
  df = df %>%
    mutate(month = month(df$date, label = TRUE, abbr = TRUE, locale = Sys.getlocale("LC_TIME")))
}

for(i in 1:length(AllDays)){

df = AllDays[[i]]
df = df %>%
  mutate(month = as.character(month(df$date,label = TRUE, abbr = TRUE, locale = Sys.getlocale("LC_TIME")))) %>%
  mutate(year = year(date)) 
}


# Create function for adding month (e.g., "Jan") and year to dataframe

add_month <- function(df){
  df = df %>%
    mutate(month = month(df$date, label = TRUE, abbr = TRUE, locale = Sys.getlocale("LC_TIME")))
}

# loop to add month and to dataframes

for(i in 1:length(AllDays)){
  AllDays[[i]] = add_month(AllDays[[i]])
  AllDays[[i]]$month = as.character(AllDays[[i]]$month)
}

# Add year (code added 2026-08-22 by Annie Kellner)

for(i in 1:length(AllDays)){
  df = AllDays[[i]]
  df = df %>%
    mutate(year = year(date(df)))
    
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


## Ordering of Things

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

frankent_daily <- mget(ls(pattern = "^in_days_")) |>
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

daily_long <- frankent_daily |> 
  pivot_longer(-c(date),
               names_to = c("scenario", "variable"),
               names_pattern = "(historical|near_mod|near_high|far_mod|far_high)_(.*)",
               values_to = "value"
  )


frankent_daily_w_month <- frankent_daily |> 
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

# Monthly

frankent_monthly <- mget(ls(pattern = "^in_monthsum_")) |>
  imap(function(df, name) {
    prefix <- str_remove(name, "^in_monthsum_")
    rename_with(df, function(x) {
      clean_name <- str_remove(x, "Avg_")
      str_c(prefix, "_", clean_name)
    }, -month)
  }) |>
  reduce(full_join, by = "month") |> 
  select(month,
         ends_with("TMeanF"), 
         ends_with("TMaxF"), 
         ends_with("TMinF"), 
         ends_with("PPT_in")
  ) |> 
  select(!contains("Abs_TminF")) |> 
  slice_head(n = -2) |> 
  mutate(month = as.numeric(month))

monthly_long <- frankent_monthly |> 
  pivot_longer(
    cols = -c(month),
    names_to = c("scenario", "variable"),
    names_pattern = "(historical|near_mod|near_high|far_mod|far_high)_(.*)",
    values_to = "value"
  )

frankent_monthly_w_month <- frankent_monthly |> 
  mutate(month_word = lubridate::month(month, label = TRUE, abbr = FALSE), .after = month)

monthly_long_w_month <- frankent_monthly_w_month |> 
  pivot_longer(
    cols = -c(month, month_word),
    names_to = c("scenario", "variable"),
    names_pattern = "(historical|near_mod|near_high|far_mod|far_high)_(.*)",
    values_to = "value"
  )

monthly_all_temps <- monthly_long_w_month %>%
  filter(variable %in% c("TMeanF", "TMaxF", "TMinF")) |>
  rename(value_f = value) |>
  mutate(
    variable = str_replace(variable, "F$", "")
  ) |>
  mutate(
    value_c = RasterUnitConvert(value_f, "FtoC"),
    value_k = RasterUnitConvert(value_c, "CtoK")
  )


##    ---   INDIVIDUAL VARIABLES    ---   ##

variables <- c(
  "Annual Mean Diurnal Range (\u00B0F)",
  "Isothermality (%)",
  "Temperature Seasonality (standard deviation)",
  "Temperature Seasonality (Coefficient of Variation)",
  "Max Temperature of Warmest Month",
  "Min Temperature of Coldest Month",
  "Annual Temperature Range",
  "Mean Temperature of Wettest Quarter (\u00B0F)",
  "Mean Temperature of Driest Quarter (\u00B0F)",
  "Mean Temperature of Warmest Quarter (\u00B0F)",
  "Mean Temperature of Coldest Quarter (\u00B0F)",
  "(Total??) Precipitation of Wettest Month (in)",
  "Total Precipitation of Driest Month (in)",
  "Precipitation Seasonality (Coefficient of Variation)",
  "Total Precipitation of Wettest Quarter (in)",
  "Total Precipitation of Driest Quarter (in)",
  "Total Precipitation of Coldest Quarter (in)",
  "Total Precipitation of Warmest Quarter (in)"
)

scenario_future_combos <- c(
  "Historical",
  "Near-Term Moderate",
  "Far-Term Moderate",
  "Near Term High",
  "Far Term High"
)


## Max Temp of Warmest Month

maxTemp_warmestMonth <- data.frame(Scenarios = scenario_future_combos,
                                   Temp = double(length = 5L))

for(i in 1:5){
warmestMonth_label = monthSumDF[[i]] %>%
  slice_max(Avg_TMaxF, n = 1, with_ties = FALSE) %>%
  mutate(month = month.abb[month]) %>%
  pull(month)

maxTMaxF = AllDays[[i]] %>%
  filter(month == warmestMonth_label) %>%
  summarise(maxTMaxF = max(TMaxF, na.rm = TRUE)) %>%
  pull(maxTMaxF) %>%
  round(3)

maxTemp_warmestMonth$Temp[i] = maxTMaxF
}


## Min Temp of Coldest Month

minTemp_coldestMonth <- data.frame(Scenarios = scenario_future_combos,
                                   Temp = double(length = 5L))

for(i in 1:5){
  coldestMonth_label = monthSumDF[[i]] %>%
    slice_min(Avg_TMinF, n = 1, with_ties = FALSE) %>%
    mutate(month = month.abb[month]) %>%
    pull(month)
  
  minTMinF = AllDays[[i]] %>%
    filter(month == coldestMonth_label) %>%
    summarise(minTMinF = min(TMinF, na.rm = TRUE)) %>%
    pull(minTMinF) %>%
    round(3)

minTemp_coldestMonth$Temp[i] = minTMinF
}


## Precipitation of Wettest Month

precip_wettestMonth <- data.frame(Scenarios = scenario_future_combos,
                                  Value = double(length = 5L))

for(i in 1:5){

wettestMonth_label = monthSumDF[[i]] %>%
  slice_max(Avg_PPT_in, n = 1, with_ties = FALSE) %>%
  mutate(month = month.abb[month]) %>%
  pull(month)

  sumPPT_in_wettestMonth = AllDays[[i]] %>%
    filter(month == wettestMonth_label) %>%
    group_by()
    summarise(sumPPT_in = sum(PPT_in, na.rm = TRUE)) %>%
    round(3)
  
  precip_wettestMonth$Value[i] = maxPPT_in
}


## Precipitation of Driest Month

precip_driestMonth <- data.frame(Scenarios = scenario_future_combos,
                                 Value = double(length = 5L))

for(i in 1:5){
  driestMonth_label = monthSumDF[[i]] %>%
    slice_min(Avg_PPT_in, n = 1, with_ties = FALSE) %>%
    mutate(month = month.abb[month]) %>%
    pull(month)
  
  max
}




  