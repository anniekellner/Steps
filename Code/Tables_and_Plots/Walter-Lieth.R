################################################
###   WALTER-LIETH DIAGRAMS   ##################
################################################

# 10-21-2024
# author: Annie Kellner
# contact: annie.kellner@colostate.edu
# written for CEMML 




# Calculate Celsius values before running function for plot creation

for(i in 1:length(monthSum)){
  
  # Format data
  
  wlC = monthSum[[i]] %>%
    mutate(Avg_TMaxC = RasterUnitConvert(Avg_TMaxF, "FtoC")) %>%
    mutate(Avg_TMinC = RasterUnitConvert(Avg_TMinF, "FtoC")) %>%
    mutate(Abs_TminC = RasterUnitConvert(Abs_TminF, "FtoC")) %>%
    select(Avg_PPT_mm, Avg_TMaxC, Avg_TMinC, Abs_TminC) 
  
  wlC = wlC[1:12,]
  wlC = data.table::transpose(wlC)
  
  # ----------  Celsius plot  ----------------- #
  
  # per is 'period' and is an argument in the ggclimat_walter_lieth() function
  
  per_scenario = if (str_detect(names(monthSum[i]), "baseline")){
    scenario_plotNames[1]
  } else if(str_detect(names(monthSum[i]),"s1")) {
    scenario_plotNames[2]
  } else {
    scenario_plotNames[3]
  }
  
  per_years = if(str_detect(names(monthSum[i]), "baseline")){ 
    as.character(paste(years[1], years[2], sep = " - "))
  } else if(str_detect(names(monthSum[i]),"f1")){
    as.character(paste(years[3],years[4], sep = " - "))
  } else {
    as.character(paste(years[5], years[6], sep = " - "))
  }
  
  
  per = paste(per_scenario,per_years,sep = ", ")
  
  per_length = nchar(per) # to calculate where to put the "subtitle" on the plot
  
  
  wlCplot = ggclimat_walter_lieth(wlC, 
                                  est = official_name, 
                                  per = per,
                                  mlab = "en", # English language 
                                  pcol = "blue", # precip color
                                  tcol = "red", # temp color
                                  pfcol = "lightblue", # probable freeze
                                  sfcol = "steelblue") # certain freeze
  
  
  # Save plot in results_folder/Plots/Walter-Lieth 
  
  wl_scenario = if (str_detect(names(monthSum[i]), "baseline")){
    "historical"
  } else if(str_detect(names(monthSum[i]),"s1")) {
    scenarios[2]
  } else {
    scenarios[3]
  }
  
  midyear = if (str_detect(names(monthSum[i]), "baseline")){
    floor((years[1] + years[2])/2)
  } else if (str_detect(names(monthSum[i]),"f1")) {
    floor((years[3] + years[4])/2)
  } else if (str_detect(names(monthSum[i]),"f2")) {
    floor((years[5] + years[6])/2)
  }
  
  
  filenameC = paste0(installation,"_",model,"_WLDiagram_",wl_scenario,"_",midyear,"_Celsius.png")
  
  ggsave(filenameC, plot = wlCplot, device = png, path = wl_dir, width = 6.5, height = 4.5, units = "in", dpi = 300)
  
  
  # ----------   Fahrenheit Plot  --------------  #
  
  # Create Fahrenheit dataframe 
  
  dat_long_endF = dat_long_end %>%
    mutate(p_mesIN = p_mes/25.4) %>%
    mutate(tm_maxF = tm_max*(9/5) + 32) %>%
    mutate(tm_minF = tm_min *(9/5) + 32) %>%
    mutate(ta_minF = ta_min*(9/5) + 32) %>%
    mutate(tmF = (tm_maxF + tm_minF) / 2)
  
  ## Vert. Axis range - temp ----
  ymaxF <- 140
  
  # Min range
  yminF <- min(dat_long_endF$tmF)
  
  #range_tm <- seq(0, ymax, 10)
  range_tmF <- seq(32, ymaxF, 18)
  
  if (yminF < 26.6) {
    yminF <- floor(yminF / 10) * 10 # min Temp rounded
    # Labels
    range_tmF <- seq(yminF, ymaxF, 18)
  }
  
  # Helper for ticks
  
  ticks = data.frame(
    x = seq(0, 12),
    ymin = -3,
    ymax = 0
  )
  
  # Precip labels
  
  preclabs2 <- as.numeric(as.character(preclabs))
  preclabs2 <- na.omit(preclabs2)
  preclabsF <- as.numeric() 
  
  for(j in 1:length(preclabs2)){
    preclabsF[j] = preclabs2[j]/25.4
  }
  
  # In the event the temp and precip vectors are not of equal size, add values
  
  if (length(preclabsF) > length(range_tm)){
    append(-10, range_tm) -> range_tm
  }
  
  if(length(range_tm) > length(preclabsF)){
    append(preclabsF, 15.7) -> preclabsF
  }
  
  
  wlFplot = wandlplot + 
    geom_hline(yintercept = c(0, 50)) + # removed 'size' argument (CEMML/AK 09-14-23)
    geom_segment(data = ticks, aes(
      x = x,
      xend = x,
      y = ymin,
      yend = ymax
    )) +
    scale_x_continuous(
      breaks = month_breaks,
      name = "",
      labels = month_labs,
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      "°F",
      labels = function(x) x* (9/5) + 32,
      breaks = range_tm, # Celsius breaks are kept so that precip axis is aligned
      sec.axis = dup_axis(name = "in",
                          labels = round(preclabsF, digits = 1)))
  
  
  # Subtitle (aka, time period and precip/temp values)
  
  sub = paste(round(mean(dat_long_endF[dat_long_endF$interpolate == FALSE, ]$tmF), 1),
              "°F        ",
              prettyNum(
                round(sum(
                  dat_long_endF[dat_long_endF$interpolate == FALSE, ]$p_mesIN
                )),
                big.mark = ","
              ),
              " in",
              sep = ""
  )
  
  sub_length = nchar(sub)
  sub_placement = 78 - per_length - sub_length # 1 in = ~10 character-spaces in Times New Roman font, so an image of 6.5" in width contains 78 character-spaces per line. This right-aligns the "subtitle" (i.e., the righthand temp/precip values)
  
  
  sub2 = paste0(
    per, 
    paste0(rep(" ",sub_placement), collapse = ""), 
    sub,
    "\n" # linebreak between "subtitle" and plot
  )
  
  # Vertical tags 
  
  maxtmF = prettyNum(round(max(dat_long_endF$tm_maxF), 1))
  mintmF = prettyNum(round(min(dat_long_endF$tm_minF), 1))
  
  tags = paste0(
    paste0(rep(" \n", 6), collapse = ""),
    maxtmF,
    paste0(rep(" \n", 10), collapse = ""),
    mintmF
  )
  
  # Add tags and theme
  
  wlFplot = wlFplot +
    ggplot2::labs(
      title = title,
      subtitle = sub2,
      tag = tags
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title = element_text(
        lineheight = 1,
        size = 14,
        face = "bold"
      ),
      plot.subtitle = element_text(
        lineheight = 1,
        size = 14,
        face = "plain"
      ),
      plot.tag = element_text(size = 10),
      plot.tag.position = "left",
      axis.ticks.length.x.bottom = unit(0, "pt"),
      axis.line.x.bottom = element_blank(),
      axis.title.y.left = element_text(
        angle = 0,
        vjust = 0.9,
        size = 10,
        colour = tcol,
        margin = unit(rep(10, 4), "pt")
      ),
      axis.text.x.bottom = element_text(size = 10),
      axis.text.y.left = element_text(colour = tcol, size = 10),
      axis.title.y.right = element_text(
        angle = 0,
        vjust = 0.9,
        size = 10,
        colour = pcol,
        margin = unit(rep(10, 4), "pt")
      ),
      axis.text.y.right = element_text(colour = pcol, size = 10)
    ) +
    ggplot2::theme(text=element_text(family="serif")) # Times New Roman
  
  # Save Fahrenheit plot
  
  filenameF = paste0(installation,"_",model,"_WLDiagram_",wl_scenario,"_",midyear,"_Fahrenheit.png")
  
  ggsave(filenameF, plot = wlFplot, device = png, path = wl_dir, width = 6.5, height = 4.5, units = "in", dpi = 300)
  
}
