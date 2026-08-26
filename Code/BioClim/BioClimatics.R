#################################################
###     BIOCLIMATICS V2.0                     ###
#################################################

# 2026-08-05
# This script replaces Ecosystems_Climate_Data.R (written by [ask Trevor name]) with updated calculations from Maria Gaetani (CEMML)
# written by Annie Kellner for CEMML (annie.kellner@colostate.edu)

# Inputs: AllDays (R object)


##  -----------  BEGIN SCRIPT --------------  ##

### PREP  ###

scenarioFuture_names <- names(AllDays) # same for AllDays & monthSum DF's

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



###  Modify AllDays df  ###

conflicts_prefer(month::lubridate) # set conflict preferences

# Add column for year

# Add year (code added 2026-08-22 by Annie Kellner)

for(i in 1:length(AllDays)){
  df = AllDays[[i]]
  df = df %>%
    mutate(date = date(date)) %>%
    mutate(date = ymd(date)) %>%
    mutate(year = year(date))
  AllDays[[i]] = df
  
}

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


##    ---   INDIVIDUAL VARIABLES    ---   ##


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
  pull(maxTMaxF)

maxTemp_warmestMonth$Temp[i] = maxTMaxF
}


## Precipitation of Wettest Month

# Sum of Precip in Wettest Month

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
    group_by(year) %>%
  summarise(sumPPT_in = sum(PPT_in, na.rm = TRUE)) %>%
    round(3)
  
  precip_wettestMonth$Value[i] = maxPPT_in
}


# Sum of Precip in Driest Month - WAIT FOR MARIA RESPONSE RE: SUM OR AVERAGE


###  ---   QUARTERLY CALCULATIONS  ---   ###

## Rolling 3-month windows from a 12-month climatology
# start 11 = Nov-Dec-Jan, start 12 = Dec-Jan-Feb (same year, not next year)

quarter_months <- function(start_month) {
  month.abb[((start_month - 1 + 0:2) %% 12) + 1]
}


roll_quarter <- function(x, fun = sum) {
  wrapped <- c(as.numeric(x), as.numeric(x)[1:2])
  vapply(1:12, function(j) fun(wrapped[j:(j + 2)]), numeric(1))
}

## Total Precipitation of Wettest Quarter

precip_wettestQuarter <- data.frame(Scenarios = scenario_future_combos,
                                    Value = double(length = 5L))
for(i in 1:5){
  
  ppt = monthSumDF[[i]] %>%
    mutate(month = as.numeric(month)) %>%
    arrange(month) %>%
    pull(Avg_PPT_in)
  
  wettest_start = which.max(roll_quarter(ppt, sum))
  wettest_months = quarter_months(wettest_start)
  
  precip_wettestQuarter$Value[i] <- AllDays[[i]] %>%
    filter(month %in% wettest_months) %>%
    group_by(year) %>%
    summarise(value = sum(PPT_in, na.rm = TRUE)) %>%
    pull(value) %>%
    round(3)
}

## CLARIFY METHODOLOGY. SEE TEAMS MESSAGE 08-24-2026
  