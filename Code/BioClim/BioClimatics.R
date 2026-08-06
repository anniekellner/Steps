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


# Historical




TMeanF_DF <- monthSumDF[[1]] %>%
  slice_max(Avg_TMeanF, n = 1, with_ties = FALSE) %>%
  pull(month)









