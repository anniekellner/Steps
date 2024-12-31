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


# Axis limits

maxTemps <- c(as.numeric(max(noaaClim[[1]]$AbsMax)),
              as.numeric(max(noaaClim[[2]]$AbsMax)),
              as.numeric(max(noaaClim[[3]]$AbsMax)))

minTemps <- c(as.numeric(min(noaaClim[[1]]$AbsMin)),
              as.numeric(min(noaaClim[[2]]$AbsMin)),
              as.numeric(min(noaaClim[[3]]$AbsMin)))

maxTemp <- max(maxTemps)
minTemp <- min(minTemps)



upper_value <- case_when(
  maxTemp < 100 ~ 100,
  maxTemp < 110 & maxTemp > 100 ~ 110, 
  maxTemp < 120 & maxTemp > 110 ~ 120
)

lower_value <- case_when(
  minTemp >= 0 ~ 0,
  minTemp < 0 & minTemp > -10 ~ -10,
  minTemp < -10 ~ -20
)





## Plot



df <- noaaClim[[1]]

ggplot(df) +
  geom_bar(aes(x=factor(month, level =c(month.abb)), y = PPT_in*5), 
           color="#0083BE", 
           fill= "#9DC3E6", 
           stat = "identity",
           position = "dodge",
           width = 0.7) + # creates space between bars
  geom_line(aes(x=factor(month, level =c(month.abb)), y = TMinF), 
            linetype=1,
            linewidth = 1.25,
            color="#0083BE",
            group=1) +
  geom_point(aes(x=factor(month, level =c(month.abb)), y = TMinF),
             shape = 23, # filled diamond
             color="#0083BE",
             fill = "#0083BE",
             size = 3) + 
  geom_line(aes(x=factor(month, level =c(month.abb)), y = AbsMin),
            linetype = "dashed",
            linewidth = 1,
            color = "#0083BE",
            group = 2) + 
  geom_point(aes(x=factor(month, level =c(month.abb)), y = AbsMin),
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
  geom_line(aes(x=factor(month, level =c(month.abb)), y = AbsMax),
            linetype = "dashed",
            linewidth = 1,
            color = "#C00000",
            group = 4) + 
  geom_point(aes(x=factor(month, level =c(month.abb)), y = AbsMax),
             shape = 17,
             color = "#C00000",
             fill = "#C00000",
             size = 3) +
  labs(title = titles[i],
       subtitle = official_name) +
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
  theme(legend.position = "right")
  