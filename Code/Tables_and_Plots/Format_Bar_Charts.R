#######################################################
#####     FORMATTING PLOTS    #########################
#######################################################

# Use package {patchwork}

# Change in Average Temperature

Hist_AvgTemp <- hist_plots[[1]]
rcp45AvgTemp <- temp_plots_S1[[1]]
rcp85AvgTemp <- temp_plots_S2[[1]]




futures <- ({rcp45AvgTemp / rcp85AvgTemp}) +
  plot_annotation(theme = theme_border)

testes <- futures + 
  plot_annotation(theme = theme_border)

theme_border <- theme_gray() + 
  theme(plot.background = element_rect(fill = NA, color = "black", size = 2))

all <- (Hist_AvgTemp|(rcp45AvgTemp / rcp85AvgTemp)) + 
  plot_layout(widths = c(1,2))

futuresTheme <- theme(panel.background = element_rect(color = "grey",
                                                linewidth = 1,
                                                fill = NA))
  
test <- futures + 
  plot_annotation(theme = futuresTheme)

Hist_AvgTemp / test


borderPlot <- wrap_elements(panel = all + 
                              plot_layout(heights = c(1,2)) & 
                              theme(panel.background = element_rect(color = "grey", 
                                                                    linewidth = 3,
                                                                    fill = NA))
                                    )

test <- 

Hist_AvgTemp + wrap_elements(full = ~ testes)


compare <- ({rcp45AvgTemp / rcp85AvgTemp}) # combine comparison plots into a single patch

  
compare & theme(panel.border = element_rect(fill = NA, colour = "grey", linewidth = 1))


Hist_AvgTemp / boxed +
  plot_annotation(tag_levels = 'a') & 
  theme(plot.tag = element_text(size = 8))


test <- compare + 
  plot_annotation(theme = patch_border)


futures <- rcp45AvgTemp / rcp85AvgTemp  

futures_theme <- theme_gray() +
  theme(plot.background = element_rect(fill = NA, colour = "black", linewidth = 1))

futures + 
  plot_annotation(theme = futures_theme)

Hist_AvgTemp