#################################################
######    CLIMOGRAPHS   #########################
#################################################

# Adapted from Steps 10-14 to RMarkdown script by Annie Kellner 6/29/2023
# Revised 12/23/2024

# Inputs: 
# AllDays dataframe
# official_name
# scenario_plotNames
# years
# plots_dir

#################################################

## Create directory for Climographs

path_to_climographs <- paste(plots_dir,"Climographs", sep = "/")

if (!dir.exists(path_to_climographs)){
  dir.create(path_to_climographs)}

# This part might also be redundant, as AllDays df already had months added in the Bioclimatics script.
# Check that the addition transfers when source() is used

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


for(i in 1:length(AllDays)){
  
  clim.dat <- AllDays[[i]]
  
  #subsetting monthly data
  
  jan.dat <- subset(clim.dat, month=="Jan")
  feb.dat <- subset(clim.dat, month=="Feb")
  mar.dat <- subset(clim.dat, month=="Mar")
  apr.dat <- subset(clim.dat, month=="Apr")
  may.dat <- subset(clim.dat, month=="May")
  jun.dat <- subset(clim.dat, month=="Jun")
  jul.dat <- subset(clim.dat, month=="Jul")
  aug.dat <- subset(clim.dat, month=="Aug")
  sep.dat <- subset(clim.dat, month=="Sep")
  oct.dat <- subset(clim.dat, month=="Oct")
  nov.dat <- subset(clim.dat, month=="Nov") # 10-15-23 deleted na.rm = TRUE because received warning that this argument would be disregarded
  dec.dat <- subset(clim.dat, month=="Dec")
  