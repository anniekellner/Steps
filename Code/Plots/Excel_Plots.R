#############################################################
##########    AUTOMATE EXCEL PLOTS    #######################
#############################################################

# 8-18-2024
# written by Annie Kellner, annie.kellner@colostate.edu


# ------------  Create Directory for Plots  ----------------------- #

xl_plots_dir <- paste(plots_dir,"Excel_Plots", sep = "/") # Change this folder name -- ASK TREVOR

if (!dir.exists(xl_plots_dir)){
  dir.create(xl_plots_dir)}


# ------------  Create function for adding month (e.g., "Jan") to dataframe --------------  #

add_month <- function(df){
  df = monthSum[[i]][1:12,]
  df = select(df, -month)
  df = mutate(df, Month = month.abb)
}


# ----------  Loop to reformat dataframes for use with ggplot2 --------------------  #

for(i in 1:length(monthSum)){
  monthSum[[i]] = add_month(monthSum[[i]])
}




           

# Plots

histAve <- ggplot(df) +
  geom_col(aes(x = factor(Month, levels = c(month.abb)), y = Avg_TMeanF), color="#CBC598", fill="#CBC598", width = 0.7) +
  xlab(paste0("\n", "Month")) +
  ylab(paste0("Average Temperature (\u00B0F)", "\n")) +
  labs(title = "Historical Average Temperature",) +
  theme(element_text(family = "serif", hjust = 0.5),
        plot.title = element_text(family = "serif", hjust = 0.5, size = 12),
        axis.title = element_text(family = "serif", hjust = 0.5, size = 10),
        panel.background = element_blank(), 
        panel.grid.major.y = element_line(color = "grey", linetype = 1, linewidth = 0.25), # linetype = 1 is a solid line. Not sure why it appears dashed, but won't be very noticeable in print
        axis.ticks = element_blank(),
        axis.text.x = element_text(margin = margin(t = 0.1, r = 0, b = 0, l = 0), size = 8),
        axis.text.y = element_text(size = 8)) # See whether - values put the text above the axis. If so, adjust
          
 ggsave(filename = 'Historical Average Temperate.png', 
        plot = histAve,
        path = './Results/Test-Excel_Plots',
        width = 5.5,
        height = 3,
        units = "in",
        dpi = 300) 
  





#### USE CODE FROM CLIMOGRAPHS SCRIPT
