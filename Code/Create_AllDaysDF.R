################################################################################
###       CREATE TABLE OF DAILY VALUES FOR ALL VARIABLES                    ####
################################################################################

# written by Annie Kellner 2-9-2025 (annie.kellner@colostate.edu)

# Derives daily values for all requested variables (excluding Climate Viewer vars)
# Functions from original LOCA_summarize.R script 
  # Adapted 2-9-2025 
    # Added Vapor Pressure Deficit (calculated) and
    # Specific Humidity (huss; extracted directly from model)

# Inputs: 
    # avdf list ("all values data frame")

# Outputs: 
    # AllDays dataframe for downstream use
    # csv files for each future and scenario


###  BEGIN SCRIPT

AllDays <- list() # for MonthSum section

for(i in 1:length(avdf)){
  csv = avdf[[i]]
  csv = csv %>%
    mutate(PPT_mm = case_when( # if raster units are "kg-m-2 -1", convert to mm (otherwise keep as is)
      str_detect(units(rx), "kg") ~ prcp*86400,
      TRUE ~ as.numeric(prcp))) %>%
    mutate(PPT_in = RasterUnitConvert(PPT_mm, "MMtoIN")) %>%
    mutate(TMaxF = case_when(
      tmax > 200 ~ RasterUnitConvert(tmax, "KtoF"), # Because only values in Kelvin would be > 200
      TRUE ~ RasterUnitConvert(tmax, "CtoF"))) %>% # assuming if temp units are not Kelvin they are Celsius
    mutate(TMaxC = case_when(
      tmax > 200 ~ RasterUnitConvert(tmax, "KtoC"),
      TRUE ~ tmax)) %>% # assuming if temp units are not Kelvin they are Celsius
    mutate(TMinF = case_when(
      tmin > 200 ~ RasterUnitConvert(tmin, "KtoF"),
      TRUE ~ RasterUnitConvert(tmin, "CtoF"))) %>% # assuming if temp units are not Kelvin they are Celsius
    mutate(TMinC = case_when(
      tmin > 200 ~ RasterUnitConvert(tmin, "KtoC"),
      TRUE ~ tmin)) %>% # assuming if temp are not K they are C
    mutate(TMeanF = (TMaxF + TMinF)/2) %>%
    mutate(TmeanC = (TMaxC + TMinC)/2) %>%
    mutate(GDDF = RasterGDD(TMinF, TMaxF, 50, 86)) %>% # All hard-coded values are from the original CEMML script
    mutate(hotdays = Rasterhotdays(TMaxC, hottemp = 32.2)) %>% # Hard-coded values may be changed at user discretion
    mutate(colddays = Rastercolddays(TMinC, coldtemp = 0)) %>%
    mutate(wetdays = Rasterwetdays(PPT_mm, wetprecip = 50.8)) %>%
    mutate(drydays = Rasterdrydays(PPT_mm, dryprecip = 2.54)) %>%
    mutate(ftdays = RasterFTdays(TMaxC, TMinC, freezethresh = -2.2, thawthresh = 1.2)) %>%
    
  
  AllDays[[i]] = csv # new df for use with MonthSum
  names(AllDays)[i] = names(avdf[i])
  
} # end creation of AllDays dataframe

# Write output to .csv

for(i in 1:length(AllDays)){
  csv = AllDays[[i]]
  csv_scenario = if (str_detect(names(AllDays[i]), "baseline")){
    "historical"
  } else if(str_detect(names(AllDays[i]),"s1")) {
    scenario1
  } else {
    scenario2
  }
  csv_years = if (str_detect(names(AllDays[i]), "f1")){
    paste(future1_start_year, "-",future1_end_year)
  } else if (str_detect(names(AllDays[i]), "f2")) {
    paste(future2_start_year,"-",future2_end_year)
  } else {
    paste(baseline_start_year,"-",baseline_end_year)
  }
  csv_fileName = paste(shp, csv_scenario, csv_years,"AllDays",sep = '_')
  
  # Create directory for files if it does not already exist
  
  if(!dir.exists(results_folder)){
    dir.create(results_folder)
  } else {
    dir.create(results_folder)
  }
  
  # Write csv to folder
  write_csv(csv, file = paste0(results_folder,"/",csv_fileName,".csv"))
  
} # end write to csv

