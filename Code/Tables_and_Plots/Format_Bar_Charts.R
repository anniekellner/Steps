#######################################################
#####     FORMATTING PLOTS    #########################
#######################################################

# Use package {patchwork}

# Change in Average Temperature

Hist_AvgTemp <- hist_plots[[1]]
rcp45AvgTemp <- temp_plots_S1[[1]]
rcp85AvgTemp <- temp_plots_S2[[1]]

# Define theme

theme_border <- theme_gray() + 
  theme(plot.background = element_rect(fill = NA, 
                                       color = "#899691", 
                                       size = 1))

# Combine futures into single plot
futures <- ({rcp45AvgTemp / rcp85AvgTemp}) +
  plot_annotation(theme = theme_border)

# Use wrap_elements() function to limit border to futures plots

wrap_elements(panel = Hist_AvgTemp) / 
  wrap_elements(panel = futures)







