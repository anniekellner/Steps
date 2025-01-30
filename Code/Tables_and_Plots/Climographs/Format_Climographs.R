###############################################################################
###########     FORMATTING CLIMOGRAPH PLOTS                     ###############
###############################################################################

# written by Annie Kellner 1-28-2025 for CEMML Climate Team

# Creates side-by-side plots for demonstrating historical vs future scenarios
# Uses package {patchwork} for plot layout (already loaded at the script start)

# SSP-4.5 (Near Term)

ssp45_nearTerm <- (climPlots[[1]] + plot_spacer() + climPlots[[2]]) + # plot_spacer() adds a dummy plot to make the two closer together
  plot_layout(guides = "collect", widths = c(4,-1.1,4)) &
  theme(legend.position = "bottom",
        legend.direction = "vertical")
  
  
filename_ssp45_nearTerm <- paste(shp,"climographs_hist_ssp45_nearTerm.png", sep = "_") 

ggsave(filename = filename_ssp45_nearTerm,
       plot = ssp45,
       path = path_to_climographs,
       width = 8.5,
       height = 5.5,
       units = "in",
       dpi = 330)

ssp45_farTerm <- 
