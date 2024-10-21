#######################################################
#####     FORMATTING PLOTS    #########################
#######################################################

# Use package {patchwork}

# Set plot elements





prcpTitle <- "Projected Change in Average Precipitation"


# Change in Average Temperature

Hist_AvgTemp <- hist_plots[[1]]
rcp45AvgTemp <- temp_plots_S1[[1]]
rcp85AvgTemp <- temp_plots_S2[[1]]

# Define theme

theme_border <- theme_gray() + 
  theme(plot.background = element_rect(fill = NA, 
                                       color = "#899691", 
                                       linewidth = 1))

# Combine futures into single plot
futures <- ({rcp45AvgTemp / rcp85AvgTemp}) +
  plot_annotation(theme = theme_border) +
  plot_layout(axis_titles = "collect")  # make single axis label



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