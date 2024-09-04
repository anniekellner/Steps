#########################################################################
##########    BAR CHARTS - COMPARISON OF VARIABLE CHANGES   #############
##########          BY SCENARIO                             #############
#########################################################################

# written by Annie Kellner for CEMML, annie.kellner@colostate.edu
# 8-30-24


##  ------  Prep Data  ------------ ##

# Add month (character) to diffHist and create new df

diffs <- list()

for(i in 1:length(diffHist)){ 
  df = diffHist[[i]]
  df = add_month(df)
  df = select(df, Month, Avg_TMeanF, Avg_TMaxF, Avg_TMinF, Avg_PPT_in)
  diffs[[i]] = df
  names(diffs)[[i]] = names(diffHist[i])
}

# Scenario 1 (e.g., SSP2-4.5)

s1f1 <- diffs[[1]] # eliminate summary rows
s1f1 <- s1f1 %>%
  mutate(Future = "F1") 

s1f2 <- diffs[[2]]
s1f2 <- s1f2 %>%
  mutate(Future = "F2")

S1 <- full_join(s1f1, s1f2) 

# Scenario 2 (e.g., SSP2-8.5)

s2f1 <- diffs[[3]] # eliminate summary rows
s2f1 <- s2f1 %>%
  mutate(Future = "F1") 

s2f2 <- diffs[[4]]
s2f2 <- s2f2 %>%
  mutate(Future = "F2")

S2 <- full_join(s1f1, s1f2) 


# ----  Prep Plot Items ------------  #

# Get mid-range values for years

f1_midyear <- floor((years[3] + years[4])/2)
f2_midyear <- floor((years[5] + years[6])/2)

# Colors and labels

custom_fill_temp <- c("F1" = "#D4B83A", "F2" = "#BB5145") # colors for temp plots
custom_labels <- c(paste(scenario_plotNames[2], f1_midyear, sep = " "), paste(scenario_plotNames[2], f2_midyear, sep = " "))


## PLOTS FOR SCENARIO 1 ##

plotList_S1 <- list()

fileNames45 <- c("Change in TAve 4.5",
                 "Change in TMax 4.5",
                 "Change in TMin 4.5")

tempTitles45 <- c(
  paste(scenario_plotNames[2],"Change in Average Temperature", sep = " "),
  paste(scenario_plotNames[2],"Change in Average Maximum Temperature", sep = " "),
  paste(scenario_plotNames[2], "Change in Average Minimum Temperature", sep = " ")
)

y_cols <- c("Avg_TMeanF", "Avg_TMaxF", "Avg_TMinF")


# Loop

for(y_col in y_cols){
  p = ggplot(S1, aes(x = factor(Month, levels = c(month.abb)), y = !!sym(y_col), fill = Future)) + 
    geom_bar(stat = "identity", position = "dodge", color = "black", width = 0.7) + # code for the y column points ggplot2 to the correct column within the df
    xlab(paste0("\n", "Month")) +
    ylab(paste0("Change in temperature (\u00B0F)", "\n"))
  
  plotList_S1[[y_col]] <- p
  
}

for(i in 1:length(plotList_S1)){  
  p = plotList_S1[[i]] +
    labs(title = tempTitles45[i]) +
    scale_y_continuous(limits = c(0,10), n.breaks = 6) +
    scale_fill_manual(values = custom_fill_temp, labels = custom_labels) +
    theme(element_text(family = "serif", hjust = 0.5),
          plot.title = element_text(family = "serif", hjust = 0.5, size = 12),
          axis.title = element_text(family = "serif", hjust = 0.5, size = 10),
          panel.background = element_blank(), 
          panel.grid.major.y = element_line(color = "grey", linetype = 1, linewidth = 0.25), # linetype = 1 is a solid line. Not sure why it appears dashed, but won't be very noticeable in print
          axis.ticks = element_blank(),
          axis.text.x = element_text(margin = margin(t = 0.1, r = 0, b = 0, l = 0), size = 8),
          axis.text.y = element_text(size = 8),
          legend.position = "bottom",
          legend.title = element_blank(), 
          legend.box.margin = margin(t = 0, r = 50, b = 0, l = 50),
          legend.key.spacing.x = unit(0.5, "in")) + 
  guides(fill = guide_legend(byrow = TRUE))

ggsave(filename = paste0(fileNames45[i],'.png'), 
       plot = p,
       path = './Results/Test-Excel_Plots',
       width = 5.5,
       height = 3,
       units = "in",
       dpi = 300) 

}


##   --------- PLOTS FOR SCENARIO 2  ---------------------------    ##

plotList_S2 <- list()

fileNames85 <- c("Change in TAve 8.5",
                 "Change in TMax 8.5",
                 "Change in TMin 8.5")

tempTitles85 <- c(
  paste(scenario_plotNames[3],"Change in Average Temperature", sep = " "),
  paste(scenario_plotNames[3],"Change in Average Maximum Temperature", sep = " "),
  paste(scenario_plotNames[3], "Change in Average Minimum Temperature", sep = " ")
)

## Loop ##

for(y_col in y_cols){
  p = ggplot(S1, aes(x = factor(Month, levels = c(month.abb)), y = !!sym(y_col), fill = Future)) + 
    geom_bar(stat = "identity", position = "dodge", color = "black", width = 0.7) + # code for the y column points ggplot2 to the correct column within the df
    xlab(paste0("\n", "Month")) +
    ylab(paste0("Change in temperature (\u00B0F)", "\n"))
  
  plotList_S2[[y_col]] <- p
  
}

for(i in 1:length(plotList_S2)){  
  p = plotList_S2[[i]] +
    labs(title = tempTitles85[i]) +
    scale_y_continuous(limits = c(0,10), n.breaks = 6) +
    scale_fill_manual(values = custom_fill_temp, labels = custom_labels) +
    theme(element_text(family = "serif", hjust = 0.5),
          plot.title = element_text(family = "serif", hjust = 0.5, size = 12),
          axis.title = element_text(family = "serif", hjust = 0.5, size = 10),
          panel.background = element_blank(), 
          panel.grid.major.y = element_line(color = "grey", linetype = 1, linewidth = 0.25), # linetype = 1 is a solid line. Not sure why it appears dashed, but won't be very noticeable in print
          axis.ticks = element_blank(),
          axis.text.x = element_text(margin = margin(t = 0.1, r = 0, b = 0, l = 0), size = 8),
          axis.text.y = element_text(size = 8),
          legend.position = "bottom",
          legend.title = element_blank(), 
          legend.box.margin = margin(t = 0, r = 50, b = 0, l = 50),
          legend.key.spacing.x = unit(0.5, "in")) + 
    guides(fill = guide_legend(byrow = TRUE))
  
  ggsave(filename = paste0(fileNames85[i],'.png'), 
         plot = p,
         path = './Results/Test-Excel_Plots',
         width = 5.5,
         height = 3,
         units = "in",
         dpi = 300) 
  
}




pS1 <- ggplot(S1) +
  geom_col(aes(x = factor(Month, levels = c(month.abb)), y = !!sym(y_col)), color="#CBC598", fill="#CBC598", width = 0.7) )



precipTitles <- c(paste(scenario_plotNames[2],"Change in Average Precipitation", sep = " "),
                paste(scenario_plotNames[3],"Change in Average Precipitation", sep = " "))

