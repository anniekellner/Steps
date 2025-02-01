#########################################################################
##########    BAR CHARTS - COMPARISON OF VARIABLE CHANGES   #############
##########          BY SCENARIO                             #############
#########################################################################

# written by Annie Kellner for CEMML, annie.kellner@colostate.edu
# 10-04-24


##  ----- Plot Elements ------- ##  

## Titles 

tempTitles <- c(
  "Projected Change in Average Temperature",
  "Projected Change in Average Maximum Temperature",
  "Projected Change in Average Minimum Temperature"
)

## Subtitles

subtitles <- c(
  paste0("\n", scenario_plotNames[2], " ", "(Moderate Emissions)"),
  paste0("\n", scenario_plotNames[3], " ", "(High Emissions)")
)


## Colors and Labels

custom_fill_temp <- c("F1" = "#D4B83A", "F2" = "#BB5145") 
custom_labels <- c("Near Term", "Far Term") 

custom_fill_prcp <- c("F1" = "#BDD7EE", "F2" = "#0083BE") 



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

rm(df)

# Scenario 1 (e.g., SSP2-4.5)

s1f1 <- diffs[[1]] # eliminate summary rows
s1f1 <- s1f1 %>%
  mutate(Future = "F1") 

s1f2 <- diffs[[2]]
s1f2 <- s1f2 %>%
  mutate(Future = "F2")

S1 <- full_join(s1f1, s1f2) 

S1 <- S1 %>% mutate(across(where(is.numeric), round, digits = 1)) # rounding because makes plotting easier

# Scenario 2 (e.g., SSP2-8.5)

s2f1 <- diffs[[3]] # eliminate summary rows
s2f1 <- s2f1 %>%
  mutate(Future = "F1") 

s2f2 <- diffs[[4]]
s2f2 <- s2f2 %>%
  mutate(Future = "F2")

S2 <- full_join(s2f1, s2f2) 

S2 <- S2 %>% mutate(across(where(is.numeric), round, digits = 1))


# ---- Plotting Prep ------------  #

# Set y-axis limits for temp

maxT_S1Delta <- S1 %>%
  select(Avg_TMeanF, Avg_TMaxF, Avg_TMinF) %>%
  reframe(max(across(everything())))

maxT_S2Delta <- S2 %>%
  select(Avg_TMeanF, Avg_TMaxF, Avg_TMinF) %>%
  reframe(max(across(everything())))

minT_S1Delta <- S1 %>%
  select(Avg_TMeanF, Avg_TMaxF, Avg_TMinF) %>%
  reframe(min(across(everything())))

minT_S2Delta <- S2 %>%
  select(Avg_TMeanF, Avg_TMaxF, Avg_TMinF) %>%
  reframe(min(across(everything())))


# Set y-axis limits for precip

# Determine highest and lowest precip delta values 

max_prcpS1 <- max(S1$Avg_PPT_in)
min_prcpS1 <- min(S1$Avg_PPT_in)

max_prcpS2 <- max(S2$Avg_PPT_in)
min_prcpS2 <- min(S2$Avg_PPT_in)

# Assign max/min values

max_pr_value <- if_else(max_prcpS1 > max_prcpS2, max_prcpS1, max_prcpS2)
min_pr_value <- if_else(min_prcpS1 < min_prcpS2, min_prcpS1, min_prcpS2)

# Assign plot limits

prcp_upper_limit <- case_when(
  max_pr_value < 2 ~ 2,
  max_pr_value > 2 & max_pr_value < 2.4 ~ 2.4,
  max_pr_value > 2.4 & max_pr_value < 2.8 ~ 2.8,
  max_pr_value > 2.8 & max_pr_value < 3.2 ~ 3.2
)

prcp_lower_limit <- case_when(
  min_prcpS2 > -1.2 ~ -1.2,
  min_prcpS2 < -1.2 ~ -1.6
)


##  ------------  PLOTS  ----------------------  ##


## PLOTS FOR SCENARIO 1 ##

# Temperatures

temp_plotList_S1 <- list()

y_cols <- c("Avg_TMeanF", "Avg_TMaxF", "Avg_TMinF")


# Loop

for(y_col in y_cols){
  p = ggplot(S1, aes(x = factor(Month, levels = c(month.abb)), y = !!sym(y_col), fill = Future)) + 
    geom_bar(stat = "identity", position = "dodge", color = "black", width = 0.7) + # code for the y column points ggplot2 to the correct column within the df
    xlab(paste0("\n", "Month")) +
    ylab(paste0("Change in temperature (\u00B0F)", "\n"))
  
  temp_plotList_S1[[y_col]] <- p
  
}

temp_plots_S1 <- list()


for(i in 1:length(temp_plotList_S1)){  
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
  
  temp_plots_S1[[i]] <- p 
  
}

# Precip 

prcpS1 <- ggplot(S1, aes(x = factor(Month, levels = c(month.abb)), y = Avg_PPT_in, fill = Future)) +
  geom_bar(stat = "identity", color = "black", position = "dodge", width = 0.7) +
  xlab(paste0("\n", "Month")) +
  ylab(paste0("Change in precipitation (inches)", "\n")) + 
  labs(title = "Projected Change in Average Precipitation", subtitle = subtitles[1]) +
  scale_y_continuous(limits = c(prcp_lower_limit, prcp_upper_limit), 
                     breaks = seq(from = prcp_lower_limit, to = prcp_upper_limit, by=0.4), 
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_fill_manual(values = custom_fill_prcp, labels = custom_labels) +
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


##  PLOTS FOR SCENARIO 2 ##

# Temperatures 


temp_plotList_S2 <- list()

for(y_col in y_cols){
  p = ggplot(S2, aes(x = factor(Month, levels = c(month.abb)), y = !!sym(y_col), fill = Future)) + 
    geom_bar(stat = "identity", position = "dodge", color = "black", width = 0.7) + # code for the y column points ggplot2 to the correct column within the df
    xlab(paste0("\n", "Month")) +
    ylab(paste0("Change in temperature (\u00B0F)", "\n"))
  
  temp_plotList_S2[[y_col]] <- p
  
}

temp_plots_S2 <- list()

for(i in 1:length(temp_plotList_S2)){  
  p = temp_plotList_S2[[i]] +
    labs(subtitle = subtitles[2]) +
    scale_y_continuous(limits = c(0,10), n.breaks = 6) +
    scale_fill_manual(values = custom_fill_temp, labels = custom_labels) +
    theme(element_text(family = "Calibri", hjust = 0.5),
          plot.subtitle = element_text(family = "Calibri", hjust = 0, size = 11),
          axis.title = element_text(family = "Calibri", hjust = 0.5, size = 10),
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
  
  temp_plots_S2[[i]] <- p 
}  


# Precip

prcpS2 <- ggplot(S2, aes(x = factor(Month, levels = c(month.abb)), y = Avg_PPT_in, fill = Future)) +
  geom_bar(stat = "identity", color = "black", position = "dodge", width = 0.7) +
  xlab(paste0("\n", "Month")) +
  ylab(paste0("Change in precipitation (inches)", "\n")) + 
  labs(subtitle = subtitles[2]) +
  scale_y_continuous(limits = c(prcp_lower_limit, prcp_upper_limit), 
                     breaks = seq(from = prcp_lower_limit, to = prcp_upper_limit, by=0.4), 
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_fill_manual(values = custom_fill_prcp, labels = custom_labels) +
  theme(element_text(family = "Calibri", hjust = 0.5),
        plot.title = element_blank(),
        plot.subtitle = element_text(family = "Calibri", hjust = 0, size = 11),
        axis.title = element_text(family = "Calibri", hjust = 0.5, size = 10),
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








