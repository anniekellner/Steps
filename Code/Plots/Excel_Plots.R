#############################################################
##########    AUTOMATE EXCEL PLOTS    #######################
#############################################################

# 8-18-2024
# written by Annie Kellner, annie.kellner@colostate.edu


# ------------  Create Directory for Plots  ----------------------- #

xl_plots_dir <- paste(plots_dir,"Excel_Plots", sep = "/") # Change this folder name -- ASK TREVOR

if (!dir.exists(xl_plots_dir)){
  dir.create(xl_plots_dir)}


# ----------  Loop to reformat dataframes for use with ggplot2 --------------------  #


df <- monthSum$results_baseline[1:12,]

df <- df %>%  
  select(-month) %>%
  mutate(Month = month.abb) 
           

# Plots

p <- ggplot(df) +
  geom_col(aes(x = factor(Month, levels = c(month.abb)), y = Avg_TMeanF), color="#CBC598", fill="#CBC598", width = 0.7) +
  xlab(paste0("\n", "Month")) +
  ylab(paste0("Average Temperature (\u00B0F)", "\n")) +
  labs(title = "Historical Average Temperature") +
  theme(element_text(family = "serif", size = 9, hjust = 0.5),
        plot.title = element_text(family = "serif", hjust = 0.5, size = 9),
        axis.title = element_text(family = "serif", hjust = 0.5, size = 9),
        panel.background = element_blank(), 
        panel.grid.major.y = element_line(color = "grey", linetype = "solid", linewidth = 0.5),
        axis.ticks = element_blank(),
        axis.text.x = element_text(margin = margin(t = -40, r = 0, b = 0, l = 0)))
          
  
  





#### USE CODE FROM CLIMOGRAPHS SCRIPT
