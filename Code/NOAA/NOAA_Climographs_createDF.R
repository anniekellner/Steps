####################################################################
#####         CLIMOGRAPHS - HISTORICAL      ########################
####################################################################


# This script creates the data frame from which the historical (observed) 
  # climatographs will be plotted

# inputs: 
    # AllDays_hist dataframe
    # noaa_monthSum dataframe

# written by Annie Kellner for CEMML 12-23-2024


##############################################################################

##    ----  PREP DATA   -------   ##


## Unify labeling of months

# Create function for adding month label (e.g., "Jan") to AllDays_hist dataframe

add_month <- function(df){
  df = df %>%
    mutate(month = month(df$date, 
                         label = TRUE, 
                         abbr = TRUE, 
                         locale = Sys.getlocale("LC_TIME")))
}


## Add month label to AllDays_hist dataframe

for(i in 1:length(AllDays_hist)){
  AllDays_hist[[i]] = add_month(AllDays_hist[[i]])
  AllDays_hist[[i]]$month = as.character(AllDays_hist[[i]]$month)
}


## Add month label to noaa_monthSum dataframe

for(i in 1:length(noaa_monthSum)){
  noaa_monthSum[[i]]$Avg_month <- month.abb[noaa_monthSum[[i]]$Avg_month]
  }


### PREP ALLDAYS_HIST  ###


## Extract top and bottom 10% of temps

quantiles <- list()

for(i in 1:length(AllDays_hist)){
  
  df = AllDays_hist[[i]]
  
  high10 = df %>%
    group_by(month) %>%
    summarize(high10 = quantile(TMaxF, probs = 0.90, na.rm = TRUE))
  
  low10 = df %>%
    group_by(month) %>%
    summarize(low10 = quantile(TMinF, probs = 0.10, na.rm = TRUE))
  
  high10low10 = full_join(high10, low10)
  
  quantiles[[i]] = high10low10 
  
}


### SELECT RELEVANT COLUMNS FROM NOAA_MONTHSUM  ###


noaaClim <- list()


for(i in 1:length(noaa_monthSum)){
  
  noaaClimDF = select(noaa_monthSum[[i]], Avg_month, Avg_PPT_in, Avg_TMaxF, Avg_TMinF)
  
  noaaClimDF = noaaClimDF %>%
    rename(month = Avg_month) %>%
    rename(PPT_in = Avg_PPT_in) %>%
    rename(TMaxF = Avg_TMaxF) %>%
    rename(TMinF = Avg_TMinF) 
  
  # Combine with AbsMinMax data
  
  noaaClim[[i]] = full_join(noaaClimDF, quantiles[[i]])

  }



  








