#######################################################
#####     FORMATTING PLOTS    #########################
#######################################################

# Creates side-by-side plots for demonstrating historical vs future scenarios
# Uses package {patchwork} for plot layout (already loaded at the script start)


##  ---------------  FORMATTING  -------------------  ##  



## Combine futures into single plots

# Temp plots

futurePlots <- list()

for(i in 1:length(temp_plots_S1)){
  FAvg = ({temp_plots_S1[[i]] / temp_plots_S2[[i]]}) +
    plot_layout(axis_titles = "collect") # combines duplicate axis labels 
  futurePlots[[i]] = FAvg
  
  FMax = ({temp_plots_S1[[i]] / temp_plots_S2[[i]]}) + 
    plot_layout(axis_titles = "collect")
  futurePlots[[i]] = FMax
  
  FMin = ({temp_plots_S1[[i]] / temp_plots_S2[[i]]}) + 
    plot_layout(axis_titles = "collect")
  futurePlots[[i]] = FMin
}

# Precip plot

Fprcp <- ({prcpS1 / prcpS2})

# Final plots

pAvg <- hist_plots[[1]] + FAvg
pMax <- hist_plots[[2]] + FMax
pMin <- hist_plots[[3]] + FMin




# Use wrap_elements() function to limit border to futures plots

wrap_elements(panel = grid.text(histTitles[1],
                                just = "centre",
                                gp = gpar(fontsize = 12,
                                          fontfamily = "calibri",
                                          fontface = "bold"))) /
  
  wrap_elements(panel = Hist_AvgTemp) / 
  wrap_elements(panel = grid.text(tempTitles[1],
                                  just = "centre",
                                  gp = gpar(fontsize = 12,
                                            fontfamily = "serif",
                                            fontface = "bold"))) /
  wrap_elements(panel = futures) + 
  plot_layout(heights = c(0.1,2,0.1,2))