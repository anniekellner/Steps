########################################################################
######            CREATE CLIMOGRAPH DATAFRAME             ##############
########################################################################

# written by Annie Kellner 1-27-25

# Creates list of dataframes for creation of Climograph plots


# Inputs: 
    # AllDays dataframe (Tibble)
    # monthAvg dataframe (Tibble)

# Output:
    # clim = list of dataframes with Precip sum (in), Avg TMaxF, Avg TMinF, upper 90% quantile, lower 10% quantile


################################################################################

## -------  Create matching 'month' columns between AllDays and monthSum  ----  ##


for(i in 1:length(monthSum)){
  monthSum[[i]] = monthSum[[i]][1:12,]} # remove last two rows 
  

for(i in 1:length(monthSum)){
  monthSum[[i]]$month = as.numeric(monthSum[[i]]$month)
}


for(i in 1:length(monthSum)){
  monthSum[[i]]$month = month.abb[monthSum[[i]]$month]
}


# Add 'month' column to AllDays dataframe


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

## -----  Calculate quantiles:  90% TMaxF, 10% TMinF  ------------  ##


quantiles <- list()

for(i in 1:length(AllDays)){
  
  df = AllDays[[i]]
  
  high90 = df %>%
    group_by(month) %>%
    summarize(high90 = quantile(TMaxF, probs = 0.90, na.rm = TRUE))
  
  low10 = df %>%
    group_by(month) %>%
    summarize(low10 = quantile(TMinF, probs = 0.10, na.rm = TRUE))
  
  high90low10 = full_join(high90, low10)
  
  quantiles[[i]] = high90low10 
  
}

### SELECT RELEVANT COLUMNS FROM MONTHSUM AND JOIN WITH QUANTILES  ###


clim <- list()


for(i in 1:length(monthSum)){
  
  climDF = select(monthSum[[i]], month, Avg_PPT_in, Avg_TMaxF, Avg_TMinF)
  
  climDF = climDF %>%
    rename(PPT_in = Avg_PPT_in) %>%
    rename(TMaxF = Avg_TMaxF) %>%
    rename(TMinF = Avg_TMinF) 
  
  # Combine with AbsMinMax data
  
  clim[[i]] = full_join(climDF, quantiles[[i]])
  
}
