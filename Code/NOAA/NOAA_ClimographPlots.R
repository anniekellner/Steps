###########################################################
###     CREATE CLIMOGRAPH PLOTS FOR HISTORICAL DATA     ###
###########################################################

# written 12-30-24 by Annie Kellner for CEMML (annie.kellner@colostate.edu)

# Creates climograph plots for observed historical (NOAA) data
# Inputs:
  # noaaClim list of dataframes
  # Each list component corresponds to the selected historical time period
    # e.g., noaaClim[[1]] is 1981 - 2010

## Titles and subtitles

titles <- c("Observed Historical Climate - 1981-2010",
            "Observed Historical Climate - 1985-2014",
            "Observed Historical Climate - 1991-2020")

subtitle <- official_name



df <- noaaClim[[1]]

ggplot(df) +
  geom_bar(aes(x=factor(month, level =c(month.abb)), y = PPT_in), 
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
             size = 3)