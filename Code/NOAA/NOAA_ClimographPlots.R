###########################################################
###     CREATE CLIMOGRAPH PLOTS FOR HISTORICAL DATA     ###
###########################################################

# written 12-30-24 by Annie Kellner for CEMML (annie.kellner@colostate.edu)

# Creates climograph plots for observed historical (NOAA) data
# Inputs:
  # noaaClim list of dataframes
  # Each list component corresponds to the selected historical time period
    # e.g., noaaClim[[1]] is 1981 - 2010


##    ---   SET UP PLOT ELEMENTS   ---   ##


## Titles 

titles <- c("Observed Historical Climate - 1981-2010",
            "Observed Historical Climate - 1985-2014",
            "Observed Historical Climate - 1991-2020")


## Axis limits

# Get max and min temps

maxTemps <- c(as.numeric(max(noaaClim[[1]]$high10)),
              as.numeric(max(noaaClim[[2]]$high10)),
              as.numeric(max(noaaClim[[3]]$high10)))

minTemps <- c(as.numeric(min(noaaClim[[1]]$low10)),
              as.numeric(min(noaaClim[[2]]$low10)),
              as.numeric(min(noaaClim[[3]]$low10)))

maxTemp <- max(maxTemps)
minTemp <- min(minTemps)

# Get limits

upper_value <- case_when(
  maxTemp < 100 ~ 100,
  maxTemp < 110 & maxTemp > 100 ~ 110, 
  maxTemp < 120 & maxTemp > 110 ~ 120,
  maxTemp > 120 & maxTemp < 130 ~ 130
)

lower_value <- case_when(
  minTemp >= 0 ~ 0,
  minTemp < 0 & minTemp > -10 ~ -10,
  minTemp < -10 ~ -20
)

# Breaks

numBreaks <- (upper_value + lower_value)/10 + 3 # 3 is for the min, max, and 0 values)


# Define graphical object for legend (precip rectangle)

prcpRect <- grid::rectGrob(gp = gpar(col = "#0083BE", fill = "#65B2A7")) # will be sized when legend is created


##  --  MANIPULATE DATAFRAME      --    ##

df <- noaaClim[[1]] # This will be part of a loop

df2 <- df %>%
  mutate(PPT_in5 = PPT_in*5) # for secondary exis



# Use pivot_longer from tidyverse to melt dataframe

meltDF <- df2 %>%
  pivot_longer(!month, names_to = "Variable", values_to = "Value") 


# Refactor so legend appears as desired

meltDF$Variable <- factor(meltDF$Variable, levels = c("high10", "TMaxF", "TMinF", "low10", "PPT_in", "PPT_in5"))


##  ---- PLOT   ----  ##


ggplot(data = meltDF, aes(x = factor(month, level =c(month.abb)), 
                                       y = Value)) + 
  geom_col(data = subset(meltDF, Variable == "PPT_in5"),
           color = "#0083BE", fill= "#65B2A7",
           position = "dodge",
           width = 0.7,
           show.legend = FALSE)  +

  geom_text(data = subset(meltDF, Variable == "PPT_in5"), 
            aes(label = round(Value/5, digits = 1)),
            family = "Calibri", 
            fontface = "plain",
            size = 4, # arbitrary based on visualization
            vjust = 2.5,  # + values are below the bar; - values are above the bar
            show.legend = FALSE) +
  
  geom_line(data = subset(meltDF, Variable %in% c("high10", "TMaxF", "TMinF", "low10")),
            aes(group = Variable, linetype = Variable, color = Variable), linewidth = 1.25) + 
  
  geom_point(data = subset(meltDF, Variable %in% c("high10", "low10", "TMaxF", "TMinF")),
             aes(group = Variable, color = Variable, shape = Variable, fill = Variable), size = 3) + 
  
  scale_y_continuous(limits = c(lower_value, upper_value), 
                     n.breaks = numBreaks,
                     sec.axis = sec_axis(~ . /5, name = "Precipitation (in)")) + 
  
  scale_linetype_manual(name = element_blank(),
                        labels = c("90% Quantile (Avg Max Temp)",
                                   "Average Maximum Daily Temperature",
                                   "Average Minimum Daily Temperature",
                                   "10% Quantile (Avg Min Temp)"),
                        values = c("dashed", "solid", "solid", "dashed", "blank", "blank")) +
  
  scale_color_manual(name = element_blank(), 
                     labels = c("90% Quantile (Avg Max Temp)",
                                "Average Maximum Daily Temperature",
                                "Average Minimum Daily Temperature",
                                "10% Quantile (Avg Min Temp)"),
                     values = c("#D9782D","#BB5145", "#0083BE", "#74CFE4")) + 
  
  scale_shape_manual(name = element_blank(),
                     labels = c("90% Quantile (Avg Max Temp)",
                                "Average Maximum Daily Temperature",
                                "Average Minimum Daily Temperature",
                                "10% Quantile (Avg Min Temp)"),
                     values = c(23, 23, 17, 17)) + # 17 = filled triangle; 23 = filled diamond
  
  scale_fill_manual(name = element_blank(),
                    labels = c("90% Quantile (Avg Max Temp)",
                               "Average Maximum Daily Temperature",
                               "Average Minimum Daily Temperature",
                               "10% Quantile (Avg Min Temp)"),
                    values = c("#D9782D","#BB5145", "#0083BE", "#74CFE4")) + 
  
  labs(title = "Observed Historical Climate - 1985-2014",
       subtitle = official_name) +
  xlab(paste0("\n", "Month"))     +                  
  ylab(paste0("Temperature (\u00B0F)","\n")) +
  
  theme(element_text(family = "Calibri", hjust = 0.5),
        plot.title = element_text(family = "Calibri", face = "bold", hjust = 0.5, size = 12),
        plot.subtitle = element_text(family = "Calibri", hjust = 0.5, size = 11),
        axis.title = element_text(family = "Calibri", hjust = 0.5, size = 10),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color = "grey", linetype = 1, linewidth = 0.25),
        plot.margin = unit(c(1,2,1,1), "cm"),
        axis.ticks = element_blank(),
        axis.text.x = element_text(family = "Calibri", face = "plain", size = 8),
        axis.text.y = element_text(family = "Calibri", face = "plain", size = 8),
        axis.title.y.right = element_text(family = "Calibri", face = "plain", vjust = 4),
        legend.text = element_text(family = "Calibri", face = "plain", size = 10),
        legend.position = "bottom",
        legend.direction = "vertical",
        legend.title = element_blank()) +
  guides(scale_linetype_manual = guide_legend(),
         scale_color_manual = guide_legend(),
         scale_shape_manual = guide_legend(),
         scale_fill_manual = guide_legend(),
         custom = guide_custom(title = "Average Total Precipitation",
                               grob = prcpRect,
                               width = unit(0.25, "in"),
                               height = unit(0.12, "in"),
                               position = "bottom",
                               theme(legend.title = element_text(family = "Calibri",
                                                                 face = "plain",
                                                                 size = 10),
                                     legend.title.position = "right"),
                               order = 1))