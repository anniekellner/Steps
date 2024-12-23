####################################################################
#####         CLIMOGRAPHS - HISTORICAL      ########################
####################################################################


# This script creates climatographs for observed historical data (e.g., NOAA)

# inputs: 
    # AllDays_hist dataframe
    # noaa_monthSum dataframe

# written by Annie Kellner for CEMML 12-23-2024


##############################################################################

# Take revelant columns from monthSum

df_msX <- noaa_monthSum[[1]]

from_MS <- df_msX %>%
  select(Avg_month, Avg_PPT_in, Avg_TMaxF, Avg_TMinF)

# Calculate max and min temps from AllDays_hist

adX <- AllDays_hist[[1]]

Abs_TMinF <- adX %>%
  group_by(month) %>%
  summarize(AbsMin = min(TMinF, na.rm = TRUE))

Abs_TMaxF <- adX %>%
  group_by(month) %>%
  summarize(AbsTMaxF = max(TMaxF, na.rm = TRUE))




