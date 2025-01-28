#######################################################
#####     FORMATTING PLOTS    #########################
#######################################################

# Creates side-by-side plots for demonstrating historical vs future scenarios
# Uses package {patchwork} for plot layout (already loaded at the script start)


##  ---------------  FORMATTING  -------------------  ##  



## Combine futures into single plots

# Temp plots

futurePlots <- list()

FAvg = ({temp_plots_S1[[1]] / temp_plots_S2[[1]]}) +
  plot_layout(axis_titles = "collect") # combines duplicate axis labels 

FAvg -> futurePlots[[1]]
  
FMax = ({temp_plots_S1[[2]] / temp_plots_S2[[2]]}) + 
  plot_layout(axis_titles = "collect")

FMax -> futurePlots[[2]]
  
FMin = ({temp_plots_S1[[3]] / temp_plots_S2[[3]]}) + 
  plot_layout(axis_titles = "collect")

FMin -> futurePlots[[3]]


# Precip plot

futurePrcp <- ({prcpS1 / prcpS2}) + 
  plot_layout(axis_titles = "collect")

# Final plots

pAvg <- hist_plots[[1]] + FAvg
pMax <- hist_plots[[2]] + FMax
pMin <- hist_plots[[3]] + FMin

pPrcp <- prcp_hist + futurePrcp

##  --------    SAVE TO FOLDER    -----------------   ##

# Filenames

filename_TAve <- paste(shp,"TAvg.png", sep = "_")
filename_TMax <- paste(shp,"TMax.png", sep = "_")
filename_TMin <- paste(shp,"TMin.png", sep = "_")
filename_Prcp <- paste(shp,"Prcp.png", sep = "_")

# Save

ggsave(filename = filename_TAve,
       plot = pAvg,
       path = bar_charts_dir,
       width = 8.5,
       height = 5.5,
       units = "in",
       dpi = 330)

ggsave(filename = filename_TMax,
       plot = pMax,
       path = bar_charts_dir,
       width = 8.5,
       height = 5.5,
       units = "in",
       dpi = 330)

ggsave(filename = filename_TMin,
       plot = pMin,
       path = bar_charts_dir,
       width = 8.5,
       height = 5.5,
       units = "in",
       dpi = 330)

ggsave(filename = filename_Prcp,
       plot = pPrcp,
       path = bar_charts_dir,
       width = 8.5,
       height = 5.5,
       units = "in",
       dpi = 330)

