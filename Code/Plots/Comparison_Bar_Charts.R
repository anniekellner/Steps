#########################################################################
##########    BAR CHARTS - COMPARISON OF VARIABLE CHANGES   #############
##########          BY SCENARIO                             #############
#########################################################################

# written by Annie Kellner for CEMML, annie.kellner@colostate.edu
# 8-30-24


##  ------  Prep Data  ------------ ##

# Add month (character) to diffHist

for(i in 1:length(diffHist)){ 
  diffHist[[i]] = add_month(diffHist[[i]])
}

## Plot titles

# Get mid-range values for years

s1_midyear <- floor((years[3] + years[4])/2)
s2_midyear <- floor((years[5] + years[6])/2)

# Titles

titles <- c(
  paste(scenario1_plotName,"Change in Average Temperature", sep = " "),
  paste(scenario2_plotName,"Change in Average Temperature", sep = " "),
  paste(scenario1_plotName,"Change in Average Maximum Temperature", sep = " "),
  paste(scenario2_plotName,"Change in Average Maximum Temperature", sep = " "),
  paste(scenario1_plotName, "Change in Average Minimum Temperature", sep = " "),
  paste(scenario2_plotName, "Change in Average Minimum Temperature", sep = " "),
  paste(scenario1_plotName,"Change in Average Precipitation", sep = " "),
  paste(scenario2_plotName,"Change in Average Precipitation", sep = " ")
)


# Scenario 1 (e.g., SSP2-4.5)

s1f1 <- diffHist[[1]][1:12,] # eliminate summary rows
s1f2 <- diffHist[[2]][1:12,]

s1f1 <- s1f1 %>%
  rename_with(~paste0(., "_F1"), starts_with("Avg")) %>%
  rename_with(~paste0(., "_F1"), starts_with("Abs")) 

s1f2 <- s1f2 %>%
  rename_with(~paste0(., "_F2"), starts_with("Avg")) %>%
  rename_with(~paste0(., "_F2"), starts_with("Abs"))

S1 <- full_join(s1f1, s1f2, by = "Month") 


# Scenario 2 (e.g., SSP2-8.5)

s2f1 <- diffHist[[3]][1:12,] # eliminate summary rows
s2f2 <- diffHist[[4]][1:12,]

s2f1 <- s2f1 %>%
  rename_with(~paste0(., "_F1"), starts_with("Avg")) %>%
  rename_with(~paste0(., "_F1"), starts_with("Abs")) 

s2f2 <- s2f2 %>%
  rename_with(~paste0(., "_F2"), starts_with("Avg")) %>%
  rename_with(~paste0(., "_F2"), starts_with("Abs"))

S2 <- full_join(s2f1, s2f2, by = "Month") 

##  ----------  PLOTS   ------------------------------------    ##

titles <- c("Historical Average Temperature", "Historical Average Maximum Temperature", "Historical Average Minimum Temperature")

