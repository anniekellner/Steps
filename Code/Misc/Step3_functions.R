#########################################################
#####   FUNCTIONS FOR STEP3.RMD   #######################
#########################################################

# Written by Annie Kellner 4/28/23

get_LOCA_variables <- function(model){
  scenario_dirs <- list.dirs(path = paste("N:\\RStor\\mindyc\\afccm\\Climate Modeling\\Data\\", model, sep = ""), 
            full.names = FALSE)
  cat("The scenarios (subfolders) associated with this model are:", sep = '\n') # cat() basically means 'print' in this context 
  cat(paste("-", scenario_dirs), sep = "\n") # names into separate lines for readability ('\n' = line)
}

# HADGEM should work the same as LOCA
# MACA will need to be written differently. 
# Use case_when() to choose how models display variables in Rmd script and see if I can incorporate functions.