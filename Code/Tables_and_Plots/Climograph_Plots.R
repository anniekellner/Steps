###########################################################
####          PLOT CLIMOGRAPHS                #############
###########################################################

# Updated Dec 2024 by Annie Kellner

# Uses output from climographs.R script to create updated plots


# df = AllDays[[1]] - just for trial and error. Eventually will be a loop. 

# ---- PLOT NAMES AND SUBTITLES ------------    #

plot_subtitles <- c(paste0("Historical, ",years[1],"-",years[2],"\n"),
                    paste0(scenario_plotNames[2],", ",years[3],"-",years[4],"\n"),
                    paste0(scenario_plotNames[2],", ",years[5],"-",years[6],"\n"),
                    paste0(scenario_plotNames[3],", ",years[3],"-",years[4],"\n"),
                    paste0(scenario_plotNames[3],", ",years[5],"-",years[6],"\n"))

# Plot names include "Monthly Means", scenario, and center year (e.g., 2030)
plot_names <- c("Monthly_Means_Historical.png",
                paste0("Monthly_Means_",scenarios[2],"_",floor((years[3]+years[4])/2),".png"), # floor() rounds down to the nearest integer
                paste0("Monthly_Means_",scenarios[2],"_",floor((years[5]+years[6])/2),".png"),
                paste0("Monthly_Means_",scenarios[3],"_",floor((years[3]+years[4])/2),".png"),
                paste0("Monthly_Means_",scenarios[3],"_",floor((years[5]+years[6])/2),".png"))

# Plot subtitles include scenario and year span (e.g., 2026 - 2035)
pick_subtitle <- function(df){
  case_when(
    names(df) == "results_baseline" ~ plot_subtitles[1],
    names(df) == "results_s1f1" ~ plot_subtitles[2],
    names(df) == "results_s1f2" ~ plot_subtitles[3],
    names(df) == "results_s2f1" ~ plot_subtitles[4],
    names(df) == "results_s2f2" ~ plot_subtitles[5]
  )}

pick_plotname <- function(df){
  case_when(
    names(df) == "results_baseline" ~ plot_names[1],
    names(df) == "results_s1f1" ~ plot_names[2],
    names(df) == "results_s1f2" ~ plot_names[3],
    names(df) == "results_s2f1" ~ plot_names[4],
    names(df) == "results_s2f2" ~ plot_names[5]
  )
}

# Determine title, subtitle, and name of plot

title <- str_replace_all(official_name, "_", " ")  
subtitle <- pick_subtitle(AllDays[i])  
plot_name <- pick_plotname(AllDays[i])




ggplot(df) +
  geom_bar(aes(x=factor(month, level =c(month.abb)), y = precip*5), 
           color="#0083BE", 
           fill= "#9DC3E6", 
           stat = "identity",
           position = "dodge",
           width = 0.7) + # creates space between bars
  geom_line(aes(x=factor(month, level =c(month.abb)), y = tmin), 
            linetype=1,
            linewidth = 1.25,
            color="#0083BE",
            group=1) + # 10-15-23 removed 'size' argument because is deprecated. If default is not OK use linewidth to adjust
  geom_point(aes(x=factor(month, level =c(month.abb)), y = tmin),
             shape = 23, # filled diamond
             color="#0083BE",
             fill = "#0083BE",
             size = 3) + 
  scale_shape_identity()
            


# Example code (https://www.sthda.com/english/wiki/ggplot2-line-types-how-to-change-line-types-of-a-graph-in-r-software#:~:text=Visualization%20in%20R-,Line%20types%20in%20R,for%20%E2%80%9Cdashed%E2%80%9D%2C%20%E2%80%A6.)
library(ggplot2)
# Line plot with multiple groups
ggplot(data=df2, aes(x=time, y=bill, group=sex)) +
  geom_line()+
  geom_point()
# Change line types
ggplot(data=df2, aes(x=time, y=bill, group=sex)) +
  geom_line(linetype="dashed")+
  geom_point()
# Change line colors and sizes
ggplot(data=df2, aes(x=time, y=bill, group=sex)) +
  geom_line(linetype="dotted", color="red", size=2)+
  geom_point(color="blue", size=3)



  geom_line(aes(x=factor(month, level =c(month.abb)), quan.min.25), linewidth=.5, linetype=2, color="goldenrod3", group=2) +
  geom_line(aes(x=factor(month, level =c(month.abb)), quan.min.75), linewidth=.5, linetype=2, color="goldenrod3", group=3) +
  geom_line(aes(x=factor(month, level =c(month.abb)), tmax), linewidth=1, linetype=1, color="red4", group=4) +
  geom_line(aes(x=factor(month, level =c(month.abb)), quan.max.25), linewidth=.5, linetype=2, color="red4", group=5) +
  geom_line(aes(x=factor(month, level =c(month.abb)), quan.max.75), linewidth=.5, linetype=2, color="red4", group=6) +
  labs(title = title, 
       subtitle = subtitle) +
  xlab(paste0("\n","Month"))                       +
  ylab("Average Temperature (\u00B0F)")    + # prints degree symbol before Fahrenheit
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

ggsave(filename = paste(path_to_climographs,plot_name,sep = "/"), dpi = 200)

}