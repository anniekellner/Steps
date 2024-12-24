####################################################################
#####         CLIMOGRAPHS - HISTORICAL      ########################
####################################################################


# This script creates climatographs for observed historical data (e.g., NOAA)

# inputs: 
    # AllDays_hist dataframe
    # noaa_monthSum dataframe

# written by Annie Kellner for CEMML 12-23-2024


##############################################################################

##    ----  PREP DATA   -------   ##


# Unify labeling of months

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


##    ---   COMBINE DATAFRAMES  --    ##

## Extract max and min values from AllDays_hist 

adX <- AllDays_hist[[1]]

Abs_TMinF <- adX %>%
  group_by(month) %>%
  summarize(AbsMin = min(TMinF, na.rm = TRUE))

Abs_TMaxF <- adX %>%
  group_by(month) %>%
  summarize(AbsTMaxF = max(TMaxF, na.rm = TRUE))


## Select relevant columns from noaa_monthSum and rename

df_msX <- noaa_monthSum[[1]]

from_MS <- select(df_msX, Avg_month, Avg_PPT_in, Avg_TMaxF, Avg_TMinF)

from_MS <- from_MS %>% 
  rename(month = Avg_month) %>%
  rename(PPT_in = Avg_PPT_in) %>%
  rename(TMaxF = Avg_TMaxF) %>%
  rename(TMinF = Avg_TMinF) 

## Combine to create dataframe for plot

noaa_clim <- from_MS %>%
  full_join(Abs_TMaxF) %>%
  full_join(Abs_TMinF)
  








