#############################################################
##########    AUTOMATE EXCEL PLOTS    #######################
#############################################################

# 8-18-2024
# written by Annie Kellner, annie.kellner@colostate.edu

# ------------- Prep data ----------------------------- #

df <- monthSum[[1]] # Historical data

df <- df 
df <- add_month(df) %>% ## Make sure this works
df <- select(df, Month, Avg_TMeanF, Avg_TMaxF, Avg_TMinF)

titles <- c("Historical Average Temperature", "Historical Average Maximum Temperature", "Historical Average Minimum Temperature")
plots <- c("Hist Avg_TMeanF", "Hist Avg_TMaxF", "Hist Avg_TMinF")
y_cols <- c("Avg_TMeanF", "Avg_TMaxF", "Avg_TMinF")

# ------------- Create Plots  --------------------------  #

plot_list <- list() # Need to create list in order to run plots through a loop

for(y_col in y_cols){

p <- ggplot(df) +
  geom_col(aes(x = factor(Month, levels = c(month.abb)), y = !!sym(y_col)), color="#CBC598", fill="#CBC598", width = 0.7) + # code for the y column points ggplot2 to the correct column within the dataframe
  xlab(paste0("\n", "Month")) +
  ylab(paste0("Average Temperature (\u00B0F)", "\n"))
  
 plot_list[[y_col]] <- p

 }
  
for(i in 1:length(plot_list)){  
  p = plot_list[[i]] +
    labs(title = titles[i]) +
    scale_y_continuous(limits = c(0,100), n.breaks = 11) +
    theme(element_text(family = "serif", hjust = 0.5),
        plot.title = element_text(family = "serif", hjust = 0.5, size = 12),
        axis.title = element_text(family = "serif", hjust = 0.5, size = 10),
        panel.background = element_blank(), 
        panel.grid.major.y = element_line(color = "grey", linetype = 1, linewidth = 0.25), # linetype = 1 is a solid line. Not sure why it appears dashed, but won't be very noticeable in print
        axis.ticks = element_blank(),
        axis.text.x = element_text(margin = margin(t = 0.1, r = 0, b = 0, l = 0), size = 8),
        axis.text.y = element_text(size = 8)) 
    
 ggsave(filename = paste0(plots[i],'.png'), 
        plot = p,
        path = './Results/Test-Excel_Plots',
        width = 5.5,
        height = 3,
        units = "in",
        dpi = 300) 
}
 