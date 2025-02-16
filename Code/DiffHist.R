###############################################################################
###           CREATE DIFFHIST CSV (TRADITIONAL REPORT OUTPUT)               ###
################################################################################

# input: monthSum dataframe 
# output: diffHist dataframe

diffHist <- list() # create diffHist dataframe for later use

# Create historical object

hist <- monthSumDF[[1]] # separate historical values for use in calculations

# Create dataframe

for(i in 2:length(monthSumDF)){ # 2 because [[1]] is historical 
  diff = hist %>%
    bind_rows(monthSumDF[[i]]) %>%
    group_by(Avg_month) %>%
    summarise(across(everything(), \(x) diff(x))) 
  
  diffHist[[i-1]] = diff # add to DiffHist dataframe for future use
  names(diffHist)[[i-1]] = names(monthSumDF[i])
}


# Save csv files 

for(i in 1:length(diffHist)){
  
  csv_scenario = if (str_detect(names(diffHist[i]), "s1")){
    scenarios[2]
  } else if(str_detect(names(diffHist[i]),"s2")) {
    scenarios[3]
  } 
  
  csv_years = if (str_detect(names(diffHist[i]), "f1")){
    paste0(years[3],"-",years[4])
  } else if (str_detect(names(diffHist[i]), "f2")) {
    paste0(years[5],"-",years[6])
  } 
  
  csv_fileName = paste(shp,csv_scenario,csv_years,"DiffHist",sep = '_')
  write_csv(diffHist[[i]], file = paste0(results_folder,"/",csv_fileName,".csv",sep = ""))
  
}
