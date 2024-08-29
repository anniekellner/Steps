#############################################################
##########    AUTOMATE EXCEL PLOTS    #######################
#############################################################

# 8-18-2024
# written by Annie Kellner, annie.kellner@colostate.edu


# ------------  Create Directory for Plots  ----------------------- #

xl_plots_dir <- paste(plots_dir,"Bar_Charts", sep = "/") 

if (!dir.exists(xl_plots_dir)){
  dir.create(xl_plots_dir)}


# ------------  Create function for adding month (e.g., "Jan") to dataframe --------------  #

add_month <- function(df){
  df = monthSum[[i]][1:12,]
  df = select(df, -month)
  df = mutate(df, Month = month.abb)
}


# ----------  PLOTS - HISTORICAL --------------------  #  

## Prep data

# Loop to reformat dataframes for use with ggplot2 

for(i in 1:length(monthSum)){ ### This might not be necessary considering only the historical df is used - but used later for diffHist
  monthSum[[i]] = add_month(monthSum[[i]])
}

         
df <- monthSum[[1]] # Historical data

df <- df %>%
  select(Month, Avg_TMeanF, Avg_TMaxF, Avg_TMinF)

titles <- c("Historical Average Temperature", "Historical Average Maximum Temperature", "Historical Average Minimum Temperature")
plots <- c("Hist Avg_TMeanF", "Hist Avg_TMaxF", "Hist Avg_TMinF")
y_cols <- c("Avg_TMeanF", "Avg_TMaxF", "Avg_TMinF")

## Create Plots

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
 
##  ------  PLOTS - CHANGE FROM HISTORICAL  ------------ ##

for(i in 1:length(diffHist)){ 
  diffHist[[i]] = add_month(diffHist[[i]])
}

# Create new df's to compare changes within-scenario


# Scenario 1 (e.g., SSP2-4.5)

s1f1 <- diffHist[[1]][1:12,] # eliminate summary rows
s1f2 <- diffHist[[2]][1:12,]

s1f1 <- s1f1 %>%
  rename_with(~paste0(., "_F1"), starts_with("Avg")) %>%
  rename_with(~paste0(., "_F1"), starts_with("Abs")) 

s1f2 <- s1f2 %>%
  rename_with(~paste0(., "_F2"), starts_with("Avg")) %>%
  rename_with(~paste0(., "_F2"), starts_with("Abs"))

S1 <- full_join(s1f1, s1f2, by = "Month") 


# Scenario 2 (e.g., SSP2-8.5)

s2f1 <- diffHist[[3]][1:12,] # eliminate summary rows
s2f2 <- diffHist[[4]][1:12,]

s2f1 <- s2f1 %>%
  rename_with(~paste0(., "_F1"), starts_with("Avg")) %>%
  rename_with(~paste0(., "_F1"), starts_with("Abs")) 

s2f2 <- s2f2 %>%
  rename_with(~paste0(., "_F2"), starts_with("Avg")) %>%
  rename_with(~paste0(., "_F2"), starts_with("Abs"))

S2 <- full_join(s2f1, s2f2, by = "Month") 

# ----------- 










