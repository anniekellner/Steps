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
                                   Value = double(length = 5L))

for(i in 1:5){
warmestMonth_label = monthSumDF[[i]] %>%
  slice_max(Avg_TMaxF, n = 1, with_ties = FALSE) %>%
  mutate(month = month.abb[month]) %>%
  pull(month) 

maxTMaxF = AllDays[[i]] %>%
  filter(month == warmestMonth_label) %>%
  summarise(maxTMaxF = max(TMaxF, na.rm = TRUE)) %>%
  pull(maxTMaxF)

maxTemp_warmestMonth$Value[i] = maxTMaxF
}


## Min Temp of Coldest Month

minTemp_coldestMonth <- data.frame(Scenarios = scenario_future_combos,
                                   Value = double(length = 5L))

for(i in 1:5){
  coldestMonth_label = monthSumDF[[i]] %>%
    slice_min(Avg_TMaxF, n = 1, with_ties = FALSE) %>%
    mutate(month = month.abb[month]) %>%
    pull(month) 
  
  minTMinF = AllDays[[i]] %>%
    filter(month == coldestMonth_label) %>%
    summarise(minTMinF = min(TMinF, na.rm = TRUE)) %>%
    pull(minTMinF)
  
  minTemp_coldestMonth$Value[i] = minTMinF
}  


## Precipitation of Wettest Month 
# Defined as mean precip of wettest month over scenario-future

precip_wettestMonth <- data.frame(Scenarios = scenario_future_combos,
                                  Value = double(length = 5L))

for(i in 1:5){
  
  precip_wettestMonth$Value[i] = monthSumDF[[i]] %>%
    slice_max(Avg_PPT_in, n = 1, with_ties = FALSE) %>%
    pull(Avg_PPT_in)
}


## Precipitation of Driest Month

precip_driestMonth <- data.frame(Scenarios = scenario_future_combos,
                                 Value = double(length = 5L))

for(i in 1:5){
  
  precip_driestMonth$Value[i] = monthSumDF[[i]] %>%
    slice_min(Avg_PPT_in, n = 1, with_ties = FALSE) %>%
    pull(Avg_PPT_in)
}


## Annual Temperature Range

annual_temp_range <- data.frame(Scenarios = scenario_future_combos,
                                Value = double(length = 5L))

for(i in 1:5){

TMax = maxTemp_warmestMonth$Value[i]
TMin = minTemp_coldestMonth$Value[i]
  
annual_temp_range$Value[i] = TMax - TMin

}

## Annual Mean Diurnal Range

annual_mean_diurnal_range <- data.frame(Scenarios = scenario_future_combos,
                                        Value = double(length = 5L))

for(i in 1:5){
  
  annual_mean_diurnal_range$Value[i] = monthSumDF[[i]] %>%
    summarise(amdr = mean(Avg_TMaxF - Avg_TMinF, na.rm = TRUE)) %>%
    pull(amdr)
  
}


## Isothermality

isothermality <- data.frame(Scenarios = scenario_future_combos,
                            Value = double(length = 5L))

for(i in 1:5){
  
  diurnal_range = annual_mean_diurnal_range$Value[i]
  temp_range = annual_temp_range$Value[i]
  
  isothermality$Value[i] = (diurnal_range / temp_range) * 100  
  
}


## Temperature Seasonality (standard deviation)
  # Kelvin calculation used for CV calculation

temp_seasonality_sdF <- data.frame(Scenarios = scenario_future_combos, # Fahrenheit
                                  Value = double(length = 5L))

temp_seasonality_sdK <- data.frame(Scenarios = scenario_future_combos, # Kelvin
                                   Value = double(length = 5L))

# Add Kelvin to monthSumDF

for(i in 1:length(monthSumDF)){

  monthSumDF[[i]] <- monthSumDF[[i]] %>%
  arrange(month) %>%
  mutate(Avg_TMeanK = RasterUnitConvert(Avg_TMeanF, "FtoK")) 

}


for(i in 1:5){
  
  temp_seasonality_sdF$Value[i] = monthSumDF[[i]] %>%
    summarise(value = sd(Avg_TMeanF, na.rm = TRUE)) %>%
    pull(value)
  
  temp_seasonality_sdK$Value[i] = monthSumDF[[i]] %>%
    summarise(value = sd(Avg_TMeanK, na.rm = TRUE)) %>%
    pull(value)
  
}


## Temperature Seasonality (coefficient of variation)
# note: calculated in K to negate negative values. Result is a % so units are irrelevant.

temp_seasonality_cv <- data.frame(Scenarios = scenario_future_combos,
                                  Value = double(length = 5L))

for(i in 1:5){
  
  meanTMeanF = monthSumDF[[i]] %>%
    summarise(meanTMeanF = mean(Avg_TMeanF)) %>%
    pull(meanTMeanF)
  
  # Kelvin conversions
  
  TMeanK = RasterUnitConvert(meanTMeanF, "FtoK")
  
  seasonalitySD_F = temp_seasonality_sdF$Value[i]
  seasonalitySD_K = temp_seasonality_sdK$Value[i]
  
  temp_seasonality_cv$Value[i] = (seasonalitySD_K / TMeanK) * 100
  
}


## Precipitation Seasonality (coefficient of variation)

precip_seasonality_cv <- data.frame(Scenarios = scenario_future_combos,
                                    Value = double(length = 5L))

for(i in 1:5){
  
  meanMonthlyPrecip = monthSumDF[[i]] %>%
    summarise(meanPrecip = mean(Avg_PPT_in)) %>%
    pull(meanPrecip)
  
  meanMonthlyPrecip_SD = monthSumDF[[i]] %>%
    summarise(precipSD = sd(Avg_PPT_in)) %>%
    pull(precipSD)
  
  precip_seasonality_cv$Value[i] = ((meanMonthlyPrecip_SD) / (meanMonthlyPrecip + 1)) * 100
  
}


###  ---   QUARTERLY CALCULATIONS  ---   ###

## Rolling 3-month windows from a 12-month climatology
# start 11 = Nov-Dec-Jan, start 12 = Dec-Jan-Feb (same year, not next year)

# Steps for calculating quarterly precip values: 
  # 1. Ascertain 'wettest quarter', etc. from monthSum
  # 2. Calculate total precip (sum) for each month-year combo within a given future-scenario
  # 3. Calculate the mean for each month over the future-scenario
  # 4. Add the resulting three means together 

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

meanPPT = monthSumDF[[i]] %>%
  mutate(month = as.numeric(month)) %>%
  arrange(month) %>%
  pull(Avg_PPT_in)

wettest_start = which.max(roll_quarter(meanPPT, sum))
wettest_quarter = quarter_months(wettest_start)

  
sumPPT_ym_wet = AllDays[[i]] %>%
    filter(month %in% wettest_quarter) %>%
    group_by(year, month) %>%
    summarise(sumPPT_wet = sum(PPT_in, na.rm = TRUE), .groups = "drop_last") %>%
    ungroup()
  
precip_wettestQuarter$Value[i] = sumPPT_ym_wet %>%
    group_by(month) %>%
    summarise(meanPPT_wet = mean(sumPPT_wet, na.rm = TRUE)) %>%
    pull(meanPPT_wet) %>%
    sum() %>%
    round(3)
}


## Precipitation of Driest Quarter

precip_driestQuarter <- data.frame(Scenarios = scenario_future_combos,
                                   Value = double(length = 5L))

for(i in 1:5){
  
  driest_start = which.min(roll_quarter(meanPPT, sum))
  driest_quarter = quarter_months(driest_start)
  
  sumPPT_ym_dry = AllDays[[i]] %>%
    filter(month %in% driest_quarter) %>%
    group_by(year, month) %>%
    summarise(sumPPT_dry = sum(PPT_in, na.rm = TRUE), .groups = "drop_last") %>%
    ungroup()
  
  precip_driestQuarter$Value[i] = sumPPT_ym_dry %>%
    group_by(month) %>%
    summarise(meanPPT_dry = mean(sumPPT_dry, na.rm = TRUE)) %>%
    pull(meanPPT_dry) %>%
    sum() %>%
    round(3)
  
}



## Precipitation of Coldest Quarter

precip_coldestQuarter <- data.frame(Scenarios = scenario_future_combos,
                                    Value = double(length = 5L))

for(i in 1:5){

TMeanF = monthSumDF[[i]] %>%
  mutate(month_num = as.numeric(month)) %>%
  arrange(month_num) %>%
  pull(Avg_TMeanF)

coldest_start = which.min(roll_quarter(TMeanF))
coldest_quarter = quarter_months(coldest_start)


sum_PPT_coldestQ = AllDays[[i]] %>%
  filter(month %in% coldest_quarter) %>%
  group_by(year, month) %>%
  summarise(sumPPT_coldestQ = sum(PPT_in, na.rm = TRUE), .groups = "drop_last") %>%
  ungroup()
  
precip_coldestQuarter$Value[i] = sum_PPT_coldestQ %>%
  group_by(month) %>%
  summarise(meanPPT_coldestQ = mean(sumPPT_coldestQ, na.rm = TRUE)) %>%
  pull(meanPPT_coldestQ) %>%
  sum() %>%
  round(3)
}


## Precipitation of Warmest Quarter

precip_warmestQuarter <- data.frame(Scenarios = scenario_future_combos,
                                    Value = double(length = 5L))


for(i in 1:5){

  warmest_start = which.max(roll_quarter(TMeanF))
  warmest_quarter = quarter_months(warmest_start)
    
  sum_PPT_warmestQ = AllDays[[i]] %>%
    filter(month %in% warmest_quarter) %>%
    group_by(year, month) %>%
    summarise(sumPPT_warmestQ = sum(PPT_in, na.rm = TRUE), .groups = "drop_last") %>%
    ungroup()
  
  precip_warmestQuarter$Value[i] = sum_PPT_warmestQ %>%
    group_by(month) %>%
    summarise(meanPPT_warmestQ = mean(sumPPT_warmestQ, na.rm = TRUE)) %>%
    pull(meanPPT_warmestQ) %>%
    sum() %>%
    round(3)
}


###   QUARTERLY TEMPS   ###

# All temps in F 
# Uses monthly data


## Mean Temp of Wettest Quarter

meanTemp_wettestQuarter <- data.frame(Scenarios = scenario_future_combos,
                                      Value = double(length = 5L))

for(i in 1:5){

  # Get wettest Quarter
  
  meanPPT = monthSumDF[[i]] %>%
    mutate(month = as.numeric(month)) %>%
    arrange(month) %>%
    pull(Avg_PPT_in)
  
  wettest_start = which.max(roll_quarter(meanPPT, sum))
  wettest_quarter = quarter_months(wettest_start)
  
  # Get Mean Temp from monthSumDF
  
  meanTemp_wettestQuarter$Value[i] = monthSumDF[[i]] %>%
    filter(month %in% wettest_quarter) %>%
    summarise(meanTemp_wettestQuarter = mean(TMeanF, na.rm = TRUE)) %>%
    pull(meanTemp_wettestQuarter) %>%
    round(3)
}


## Mean Temp of Driest Quarter

meanTemp_driestQuarter <- data.frame(Scenarios = scenario_future_combos,
                                     Value = double(length = 5L))

for(i in 1:5){
  
  # Get Driest Quarter
  
  meanPPT = monthSumDF[[i]] %>%
    mutate(month = as.numeric(month)) %>%
    arrange(month) %>%
    pull(Avg_PPT_in)
  
  driest_start = which.min(roll_quarter(meanPPT, sum))
  driest_quarter = quarter_months(driest_start)
  
  meanTemp_driestQuarter$Value[i] = monthSumDF[[i]] %>%
    filter(month %in% driest_quarter) %>%
    summarise(meanTemp_driestQuarter = mean(TMeanF, na.rm = TRUE)) %>%
    pull(meanTemp_driestQuarter) %>%
    round(3)
  
}

## Mean Temp Coldest Quarter

meanTemp_coldestQuarter <- data.frame(Scenarios = scenario_future_combos,
                                      Value = double(length = 5L))

for(i in 1:5){
  
  TMeanF = monthSumDF[[i]] %>%
    mutate(month = as.numeric(month)) %>%
    arrange(month) %>%
    pull(Avg_TMeanF)
  
  coldest_start = which.min(roll_quarter(TMeanF))
  coldest_quarter = quarter_months(coldest_start)
  
  meanTemp_coldestQuarter$Value[i] = monthSumDF[[i]] %>%
    filter(month %in% coldest_quarter) %>%
    summarise(meanTemp_coldestQuarter = mean(TMeanF, na.rm = TRUE)) %>%
    pull(meanTemp_coldestQuarter) %>%
    round(3)
  
}


## Mean Temp Warmest Quarter

meanTemp_warmestQuarter <- data.frame(Scenarios = scenario_future_combos,
                                      Value = double(length = 5L))

for(i in 1:5){
  
  TMeanF = monthSumDF[[i]] %>%
    mutate(month = as.numeric(month)) %>%
    arrange(month) %>%
    pull(Avg_TMeanF)
  
  warmest_start = which.max(roll_quarter(TMeanF))
  warmest_quarter = quarter_months(warmest_start)
  
  meanTemp_warmestQuarter$Value[i] = monthSumDF[[i]] %>%
    filter(month %in% warmest_quarter) %>%
    summarise(meanTemp_warmestQuarter = mean(Avg_TMeanF, na.rm = TRUE)) %>%
    pull(meanTemp_warmestQuarter) %>%
    round(3)
  
}


## #END QUARTERLY CALCULATIONS





