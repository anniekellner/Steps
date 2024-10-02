
###################################################
####    EMISSIONS TABLE 1   #######################
###################################################

# Automating tables from T. Even Excel doc
# written by Annie Kellner for CEMML
# 9-17-24


# select relevant values from csv object

histTotals <- csv[14,]
histTotals <- histTotals %>%
  select_if(~ all(!is.na(.))) %>%
  select(-1) 

histAves <- csv[13,]
histAves <- histAves %>%
  select_if(~ all(!is.na(.))) %>% 
  select(-1) 

modeled_historical <- bind_cols(histAves, histTotals) 

modeled_historical <- modeled_historical %>%
  select(Avg_PPT_in, # order according to Excel doc
         Avg_TMinF, 
         Avg_TMaxF, 
         Avg_TMeanF, 
         Avg_GDDF, 
         Avg_hotdays, 
         Avg_colddays, 
         Avg_wetdays, 
         Avg_drydays, 
         Avg_ftdays)

# Keep this code for later ##

#modeled_historical <- t(modeled_historical)
#colnames(modeled_historical) <- "Modeled Historical"
#modeled_historical <- as.data.frame(modeled_historical)