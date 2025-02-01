#########################################################################
##########    BAR CHARTS - HISTORICAL SUMMARIES   #######################
#########################################################################

# 8-18-2024
# written by Annie Kellner, annie.kellner@colostate.edu


# Chart Elements

histTitles <- c("Modeled Historical Average Temperature", 
                "Modeled Historical Average Maximum Temperature", 
                "Modeled Historical Average Minimum Temperature")


# ------------- Prep data ----------------------------- #

hist_df <- monthSum[[1]] # Historical data

hist_df <- add_month(hist_df)  ## Make sure this works
hist_df <- select(hist_df, Month, Avg_TMeanF, Avg_TMaxF, Avg_TMinF, Avg_PPT_in)


y_cols <- c("Avg_TMeanF", "Avg_TMaxF", "Avg_TMinF")

# ------------- Create Plots  --------------------------  #

##  TEMPERATURES

plot_list <- list() # Need to create list in order to run plots through a loop

for(y_col in y_cols){
  
  p <- ggplot(hist_df) +
    geom_col(aes(x = factor(Month, levels = c(month.abb)), y = !!sym(y_col)), color="#CBC598", fill="#CBC598", width = 0.7) + # code for the y column points ggplot2 to the correct column within the dataframe
    xlab(paste0("\n", "Month")) +
    ylab(paste0("Average Temperature (\u00B0F)", "\n"))
  
  plot_list[[y_col]] <- p
  
}


hist_plots <- list()

upper_value_temp <- ifelse(max(hist_df$Avg_TMaxF) > 100, 110, 100)
numBreaks <- ifelse(upper_value_temp == 110, 12, 11)

for(i in 1:length(plot_list)){  
  p = plot_list[[i]] +
    labs(title = histTitles[i]) +
    scale_y_continuous(limits = c(0,upper_value_temp), n.breaks = numBreaks) +
    theme(element_text(family = "Calibri", hjust = 0.5),
          plot.title = element_text(family = "Calibri", face = "bold", hjust = 0.5, size = 12),
          axis.title = element_text(family = "Calibri", hjust = 0.5, size = 10),
          panel.background = element_blank(), 
          panel.grid.major.y = element_line(color = "grey", linetype = 1, linewidth = 0.25), # linetype = 1 is a solid line. Not sure why it appears dashed, but won't be very noticeable in print
          axis.ticks = element_blank(),
          axis.text.x = element_text(margin = margin(t = 0.1, r = 0, b = 0, l = 0), size = 8),
          axis.text.y = element_text(size = 8)) 
  
  hist_plots[[i]] <- p
}


### PRECIP

upper_value <- ceiling(max(hist_df$Avg_PPT_in))
upper_limit <- upper_value + 1
breaks <- upper_limit + 1 # add 1 so that y-axis scale starts at zero

prcp_hist <- ggplot(hist_df) + 
  geom_col(aes(x = factor(Month, levels = c(month.abb)), y = Avg_PPT_in), color="#74CFE4", fill="#74CFE4", width = 0.7) + 
  xlab(paste0("\n", "Month")) +
  ylab(paste0("Average Precipitation (inches)", "\n")) +
  labs(title = "Modeled Historical Average Precipitation") +
  scale_y_continuous(limits = c(0,upper_limit), n.breaks = breaks) + 
  theme(element_text(family = "Calibri", hjust = 0.5),
        plot.title = element_text(family = "Calibri", face = "bold", hjust = 0.5, size = 12),
        axis.title = element_text(family = "Calibri", hjust = 0.5, size = 10),
        panel.background = element_blank(), 
        panel.grid.major.y = element_line(color = "grey", linetype = 1, linewidth = 0.25), # linetype = 1 is a solid line. Not sure why it appears dashed, but won't be very noticeable in print
        axis.ticks = element_blank(),
        axis.text.x = element_text(margin = margin(t = 0.1, r = 0, b = 0, l = 0), size = 8),
        axis.text.y = element_text(size = 8)) 






