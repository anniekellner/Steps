###############################################################################
###########     FORMATTING CLIMOGRAPH PLOTS                     ###############
###############################################################################

# written by Annie Kellner 1-28-2025 for CEMML Climate Team

# Creates side-by-side plots for demonstrating historical vs future scenarios
# Uses package {patchwork} for plot layout (already loaded at the script start)

# SSP-4.5 (Near Term)

ssp45 <- () 
  
  
  
  FAvg = ({temp_plots_S1[[1]] / temp_plots_S2[[1]]}) +
  plot_layout(axis_titles = "collect") # combines duplicate axis labels 

