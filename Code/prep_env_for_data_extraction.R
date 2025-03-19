#############################################################################
###       PREPARE GLOBAL ENVIRONMENT FOR DATA EXTRACTION                  ###
#############################################################################


# written by Annie Kellner 2-9-2025


scenarios <- c(baseline, scenario1, scenario2)

scenario1_plotName <- ifelse(str_detect(scenario1, "ssp245"), "SSP2-4.5", "RCP 4.5")
scenario2_plotName <- ifelse(str_detect(scenario2, "ssp585"), "SSP5-8.5", "RCP 8.5")

scenario_plotNames <- c("Historical", scenario1_plotName, scenario2_plotName)

baseline_yrs <- paste(seq(baseline_start_year, baseline_end_year, 1), collapse = '|')
future1_yrs <- paste(seq(future1_start_year, future1_end_year, 1), collapse = '|')
future2_yrs <- paste(seq(future2_start_year, future2_end_year, 1), collapse = '|')

years <- c(baseline_start_year, # vector for easy reference
           baseline_end_year, 
           future1_start_year, 
           future1_end_year, 
           future2_start_year, 
           future2_end_year)



files_baseline <- list() # historical
files_s1f1 <- list() # scenario1, future 1
files_s1f2 <- list() # scenario 1, future 2
files_s2f1 <- list() # scenario 2, future 1
files_s2f2 <- list() # scenario 2, future 2

# List files for Baseline Scenario

for(i in 1:length(variables)){ 
  fileNames = list.files(baseline_dir, pattern = variables[i], full.names = TRUE)
  fileNames = str_remove_all(fileNames, pattern = ".aux.xml")
  fileNames = fileNames[!duplicated(fileNames)]
  dt = data.table(fileNames, result = grepl(baseline_yrs, fileNames))
  dt2 = dplyr::filter(dt, result == TRUE)
  files_baseline[[i]] = dt2$fileNames
}

# List files for S1F1 

for(i in 1:length(variables)){ 
  dir = paste(model_dir, scenario1, sep = '/')
  fileNames = list.files(dir, pattern = variables[i], full.names = TRUE)
  fileNames = str_remove_all(fileNames, pattern = ".aux.xml")
  fileNames = fileNames[!duplicated(fileNames)]
  dt = data.table(fileNames, result = grepl(future1_yrs, fileNames))
  dt2 = dplyr::filter(dt, result == TRUE)
  files_s1f1[[i]] = dt2$fileNames
}

# List files for S1F2

for(i in 1:length(variables)){ 
  dir = paste(model_dir, scenario1, sep = '/')
  fileNames = list.files(dir, pattern = variables[i], full.names = TRUE)
  fileNames = str_remove_all(fileNames, pattern = ".aux.xml")
  fileNames = fileNames[!duplicated(fileNames)]
  dt = data.table(fileNames, result = grepl(future2_yrs, fileNames))
  dt2 = dplyr::filter(dt, result == TRUE)
  files_s1f2[[i]] = dt2$fileNames
}

# List files for S2F1 

for(i in 1:length(variables)){ 
  dir = paste(model_dir, scenario2, sep = '/')
  fileNames = list.files(dir, pattern = variables[i], full.names = TRUE)
  fileNames = str_remove_all(fileNames, pattern = ".aux.xml")
  fileNames = fileNames[!duplicated(fileNames)]
  dt = data.table(fileNames, result = grepl(future1_yrs, fileNames))
  dt2 = dplyr::filter(dt, result == TRUE)
  files_s2f1[[i]] = dt2$fileNames
}

# List files for S2F2

for(i in 1:length(variables)){ 
  dir = paste(model_dir, scenario2, sep = '/')
  fileNames = list.files(dir, pattern = variables[i], full.names = TRUE)
  fileNames = str_remove_all(fileNames, pattern = ".aux.xml")
  fileNames = fileNames[!duplicated(fileNames)]
  dt = data.table(fileNames, result = grepl(future2_yrs, fileNames))
  dt2 = dplyr::filter(dt, result == TRUE)
  files_s2f2[[i]] = dt2$fileNames
}

