########################################################################
######            CREATE CLIMOGRAPH DATAFRAME             ##############
########################################################################

# written by Annie Kellner 1-27-25

# Creates list of dataframes for creation of Climograph plots


# Inputs: 
    # AllDays dataframe (Tibble)
    # monthSumDF dataframe (Tibble)

# Output:
    # clim = list of dataframes with Precip sum (in), Avg TMaxF, Avg TMinF, upper 90% quantile, lower 10% quantile


################################################################################

## -------  Create matching 'month' columns between AllDays and monthSum  ----  ##


# AllDays

for(i in 1:length(AllDays)){ # Convert date column to ymd (year-month-day in {lubridate})
  df = AllDays[[i]]
  df$date = ymd(df$date)
  AllDays[[i]] = df
}

for(i in 1:length(AllDays)){
  df = AllDays[[i]]
  df = df %>%
    mutate(month = month(df$date))
  AllDays[[i]] = df
}

for(i in 1:length(AllDays)){
  AllDays[[i]]$month = month.abb[AllDays[[i]]$month]
}


# monthSum

for(i in 1:length(monthSumDF)){
  monthSumDF[[i]]$Avg_month = month.abb[monthSumDF[[i]]$Avg_month]
}

for(i in 1:length(monthSumDF)){
  df = monthSumDF[[i]]
  
  df = df %>%
    rename(month = Avg_month)
    monthSumDF[[i]] = df
}
## -----  Calculate quantiles:  90% TMaxF, 10% TMinF  ------------  ##


quantiles <- list()

for(i in 1:length(AllDays)){
  
  df = AllDays[[i]]
  
  high90 = df %>%
    group_by(month) %>%
    summarize(high90 = quantile(TMaxF, probs = 0.90, na.rm = TRUE)) %>%
    ungroup()
  
  low10 = df %>%
    group_by(month) %>%
    summarize(low10 = quantile(TMinF, probs = 0.10, na.rm = TRUE)) %>%
    ungroup()
  
  high90low10 = left_join(high90, low10)
  
  quantiles[[i]] = high90low10 
  
}

### SELECT RELEVANT COLUMNS FROM monthSumDF AND JOIN WITH QUANTILES  ###


clim <- list()


for(i in 1:length(monthSumDF)){
  
  climDF = select(monthSumDF[[i]], month, Avg_PPT_in, Avg_TMaxF, Avg_TMinF)
  
  climDF = climDF %>%
    rename(PPT_in = Avg_PPT_in) %>%
    rename(TMaxF = Avg_TMaxF) %>%
    rename(TMinF = Avg_TMinF) 

  
  # Combine with AbsMinMax data
  
  clim[[i]] = left_join(climDF, quantiles[[i]])
  
}
