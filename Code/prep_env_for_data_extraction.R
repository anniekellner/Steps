#############################################################################
###       PREPARE GLOBAL ENVIRONMENT FOR DATA EXTRACTION                  ###
#############################################################################

# written by Annie Kellner 2-9-2025

# This script clears the global environment from the Observed Historical Analyses
  # but retains objects that will be used for data extraction and beyond

rm(list=setdiff(ls(), c("shp", # These are the objects we want to keep
                        "official_name",
                        "model", 
                        "scenarios",
                        "scenario_plotNames",
                        "baseline", "scenario1", "scenario2",
                        "variables",
                        "baseline_start_year",
                        "baseline_end_year",
                        "future1_start_year",
                        "future1_end_year",
                        "future2_start_year",
                        "future2_end_year",
                        "model_dir",
                        "variables",
                        "plots_dir",
                        "results_folder",
                        "dir_installation_boundaries",
                        "noaaDashboard", # will add futures to this dataframe later
                        lsf.str()))) # all functions


scenarios <- c(baseline, scenario1, scenario2)

baseline_yrs <- paste(seq(baseline_start_year, baseline_end_year, 1), collapse = '|')
future1_yrs <- paste(seq(future1_start_year, future1_end_year, 1), collapse = '|')
future2_yrs <- paste(seq(future2_start_year, future2_end_year, 1), collapse = '|')

