###################################################################
#####       FORMAT WALTER-LIETH PLOTS FOR DOCUMENTS         #######
###################################################################

# written by Annie Kellner for CEMML
# 1-30-2025

## This script makes use of the list of WL plots in case formatting 
  # for documents (e.g., climate appendices) is desired


# SSP2-4.5 Near Term

wl45nearTerm <- (WLplots[[1]] + WLplots[[2]]) +
  plot_annotation(title = official_name,
                  theme = theme(
                    plot.title = element_text(
                      family = "Calibri",
                      face = "bold",
                      hjust = 0.5,
                      lineheight = 1,
                      size = 12
                  )))



filename_ssp45_nearTerm <- paste(shp,"WL_hist_ssp45_nearTerm.png", sep = "_") 

ggsave(filename = filename_ssp45_nearTerm,
       path = wl_dir,
       width = 8.5,
      height = 5.5,
       units = "in",
       dpi = 330)

# SSP2-4.5 Far Term

wl45farTerm <- (WLplots[[1]] + WLplots[[3]]) +
  plot_annotation(title = official_name,
                  theme = theme(
                    plot.title = element_text(
                      family = "Calibri",
                      face = "bold",
                      hjust = 0.5,
                      lineheight = 1,
                      size = 12
                    )))

filename_ssp45_farTerm <- paste(shp,"WL_hist_ssp45_farTerm.png", sep = "_") 

ggsave(filename = filename_ssp45_farTerm,
       path = wl_dir,
       width = 8.5,
       height = 5.5,
       units = "in",
       dpi = 330)


# SSP5-8.5 Near Term

wl85nearTerm <- (WLplots[[1]] + WLplots[[4]]) +
  plot_annotation(title = official_name,
                  theme = theme(
                    plot.title = element_text(
                      family = "Calibri",
                      face = "bold",
                      hjust = 0.5,
                      lineheight = 1,
                      size = 12
                    )))



filename_ssp85_nearTerm <- paste(shp,"WL_hist_ssp85_nearTerm.png", sep = "_") 

ggsave(filename = filename_ssp85_nearTerm,
       path = wl_dir,
       width = 8.5,
       height = 5.5,
       units = "in",
       dpi = 330)

# SSP5-8.5 Far Term

wl85farTerm <- (WLplots[[1]] + WLplots[[5]]) +
  plot_annotation(title = official_name,
                  theme = theme(
                    plot.title = element_text(
                      family = "Calibri",
                      face = "bold",
                      hjust = 0.5,
                      lineheight = 1,
                      size = 12
                    )))



filename_ssp85_farTerm <- paste(shp,"WL_hist_ssp85_farTerm.png", sep = "_") 

ggsave(filename = filename_ssp85_farTerm,
       path = wl_dir,
       width = 8.5,
       height = 5.5,
       units = "in",
       dpi = 330)


