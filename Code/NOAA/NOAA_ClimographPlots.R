###########################################################
###     CREATE CLIMOGRAPH PLOTS FOR HISTORICAL DATA     ###
###########################################################

# written 12-30-24 by Annie Kellner for CEMML (annie.kellner@colostate.edu)

# Creates climograph plots for observed historical (NOAA) data
# Inputs:
  # noaaClim list of dataframes
  # Each list component corresponds to the selected historical time period
    # e.g., noaaClim[[1]] is 1981 - 2010

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

# Axis limits

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

## Plot



df <- noaaClim[[1]]

ggplot(df, aes(x=factor(month, level =c(month.abb)))) +
  geom_bar(aes(y = PPT_in*5), 
           color="#0083BE", 
           fill= "#9DC3E6", 
           stat = "identity",
           position = "dodge",
           width = 0.7,  # creates space between bars
           show.legend = TRUE) +
  geom_text(aes(y = PPT_in*5, label = round(PPT_in, digits = 1)), 
                family = "Calibri", 
                fontface = "plain",
                size = 4, # arbitrary based on visualization
                vjust = 1.75,  # + values are below the bar; - values are above the bar
                show.legend = FALSE) +
  geom_line(aes(y = TMinF), 
            linetype=1,
            linewidth = 1.25,
            color="#0083BE",
            group = 1) +
  geom_point(aes(y = TMinF),
             shape = 23, # filled diamond
             color="#0083BE",
             fill = "#0083BE",
             size = 3,
             group = 1) + 
  geom_line(aes(y = low10),
            linetype = "dashed",
            linewidth = 1,
            color = "#0083BE",
            group = 2) + 
  geom_point(aes(x=factor(month, level =c(month.abb)), y = low10),
             shape = 23, # filled diamond
             color="#0083BE",
             fill = "#9DC3E6",
             size = 3) + 
  geom_line(aes(x=factor(month, level =c(month.abb)),y = TMaxF),
            linetype = 1,
            linewidth = 1.25,
            color = "#C00000",
            group = 3) +
  geom_point(aes(x=factor(month, level =c(month.abb)), y = TMaxF),
             shape = 17, # filled triangle
             color= "#C00000",
             fill = "#C00000",
             size = 3) + 
  geom_line(aes(x=factor(month, level =c(month.abb)), y = high10),
            linetype = "dashed",
            linewidth = 1,
            color = "#C00000",
            group = 4) + 
  geom_point(aes(x=factor(month, level =c(month.abb)), y = high10),
             shape = 17,
             color = "#C00000",
             fill = "#C00000",
             size = 3) +
  labs(title = titles[i],
       subtitle = official_name) +
  xlab(paste0("\n", "Month"))     +                  
  ylab(paste0("Temperature (\u00B0F)", "\n")) +
  scale_y_continuous(limits = c(lower_value, upper_value), 
                     n.breaks = numBreaks,
                     sec.axis = sec_axis(~ . /5, name = "Precipitation (in)")) + 
  theme(element_text(family = "Calibri", hjust = 0.5),
        plot.title = element_text(family = "Calibri", face = "bold", hjust = 0.5, size = 12),
        plot.subtitle = element_text(family = "Calibri", hjust = 0.5, size = 11),
        axis.title = element_text(family = "Calibri", hjust = 0.5, size = 10),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color = "grey", linetype = 1, linewidth = 0.25),
        axis.ticks = element_blank(),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        legend.position = "bottom",
        legend.title = element_blank(),
        )


# From barplots script - legend

legend.position = "bottom",
legend.title = element_blank(), 
legend.box.margin = margin(t = 0, r = 50, b = 0, l = 50),
legend.key.spacing.x = unit(0.5, "in")) + 
  guides(fill = guide_legend(byrow = TRUE))
 

xlab(paste0("\n", "Month")) +
  ylab(paste0("Change in temperature (\u00B0F)", "\n"))
 
  
  ## from original climograph script
  xlab(paste0("\n","Month"))     +                  
  ylab("Average Temperature (\u00B0F)") +
  theme_minimal() +
  theme(text=element_text(color = "black", family = "serif")) +
  theme(title=element_text(size=17)) +
  theme(axis.title=element_text(size=15)) +
  theme(axis.text.x=element_text(size=15)) +
  theme(axis.text.y=element_text(size=15)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
  theme(axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 15))) +
  scale_y_continuous(limits = c(0, 100), n.breaks = 6, sec.axis = sec_axis(~./5, name = "Average Precip (in/month)")) +
  theme(legend.position = "right"),
  
  

## from bar chart code

p = temp_plotList_S1[[i]] +
  labs(title = tempTitles[i], subtitle = subtitles[1])+
  scale_y_continuous(limits = c(0, 10), n.breaks = 6) +
  scale_fill_manual(values = custom_fill_temp, labels = custom_labels) +
  theme(element_text(family = "Calibri", hjust = 0.5),
        plot.title = element_text(family = "Calibri", face = "bold", hjust = 0.5, size = 12),
        plot.subtitle = element_text(family = "Calibri", hjust = 0, size = 11),
        axis.title = element_text(family = "Calibri", hjust = 0.5, size = 10),
        panel.background = element_blank(), 
        panel.grid.major.y = element_line(color = "grey", linetype = 1, linewidth = 0.25), # linetype = 1 is a solid line. Not sure why it appears dashed, but won't be very noticeable in print
        axis.ticks = element_blank(),
        axis.text.x = element_text(margin = margin(t = 0.1, r = 0, b = 0, l = 0), size = 8),
        axis.text.y = element_text(size = 8),
        legend.position = "none")

