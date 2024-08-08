#################################################
######    CLIMOGRAPHS   #########################
#################################################

# Adapted from Steps 10-14 to RMarkdown script by Annie Kellner 6/29/2023

# Inputs: 
  # AllDays dataframe
  # official_name
  # scenario_plotNames
  # years
  # plots_dir

#################################################

library(extrafont)

## Create directory for Climographs

path_to_climographs <- paste(plots_dir,"Climographs", sep = "/")

if (!dir.exists(path_to_climographs)){
  dir.create(path_to_climographs)}

# This part might also be redundant, as AllDays df already had months added in the Bioclimatics script.
# Check that the addition transfers when source() is used

# Create function for adding month (e.g., "Jan") to dataframe

add_month <- function(df){
  df = df %>%
    mutate(month = month(df$date, label = TRUE, abbr = TRUE, locale = Sys.getlocale("LC_TIME")))
}

# loop to add month to dataframes

for(i in 1:length(AllDays)){
  AllDays[[i]] = add_month(AllDays[[i]])
  AllDays[[i]]$month = as.character(AllDays[[i]]$month)
}


for(i in 1:length(AllDays)){

clim.dat <- AllDays[[i]]

#subsetting monthly data

jan.dat <- subset(clim.dat, month=="Jan")
feb.dat <- subset(clim.dat, month=="Feb")
mar.dat <- subset(clim.dat, month=="Mar")
apr.dat <- subset(clim.dat, month=="Apr")
may.dat <- subset(clim.dat, month=="May")
jun.dat <- subset(clim.dat, month=="Jun")
jul.dat <- subset(clim.dat, month=="Jul")
aug.dat <- subset(clim.dat, month=="Aug")
sep.dat <- subset(clim.dat, month=="Sep")
oct.dat <- subset(clim.dat, month=="Oct")
nov.dat <- subset(clim.dat, month=="Nov") # 10-15-23 deleted na.rm = TRUE because received warning that this argument would be disregarded
dec.dat <- subset(clim.dat, month=="Dec")

  
##lets create quantiles##
#mins#
#jan
jan.min.25q <- quantile(jan.dat$TMinF, probs = 0.25)
jan.min.75q <- quantile(jan.dat$TMinF, probs = 0.75)
#feb
feb.min.25q <- quantile(feb.dat$TMinF, probs = 0.25)
feb.min.75q <- quantile(feb.dat$TMinF, probs = 0.75)
#mar
mar.min.25q <- quantile(mar.dat$TMinF, probs = 0.25)
mar.min.75q <- quantile(mar.dat$TMinF, probs = 0.75)
#apr
apr.min.25q <- quantile(apr.dat$TMinF, probs = 0.25)
apr.min.75q <- quantile(apr.dat$TMinF, probs = 0.75)
apr.min.75q
#may
may.min.25q <- quantile(may.dat$TMinF, probs = 0.25)
may.min.75q <- quantile(may.dat$TMinF, probs = 0.75)
#jun
jun.min.25q <- quantile(jun.dat$TMinF, probs = 0.25)
jun.min.75q <- quantile(jun.dat$TMinF, probs = 0.75)
#jul
jul.min.25q <- quantile(jul.dat$TMinF, probs = 0.25)
jul.min.75q <- quantile(jul.dat$TMinF, probs = 0.75)
#aug
aug.min.25q <- quantile(aug.dat$TMinF, probs = 0.25)
aug.min.75q <- quantile(aug.dat$TMinF, probs = 0.75)
#sep
sep.min.25q <- quantile(sep.dat$TMinF, probs = 0.25)
sep.min.75q <- quantile(sep.dat$TMinF, probs = 0.75)
#oct
oct.min.25q <- quantile(oct.dat$TMinF, probs = 0.25)
oct.min.75q <- quantile(oct.dat$TMinF, probs = 0.75)
#nov
nov.min.25q <- quantile(nov.dat$TMinF, probs = 0.25, na.rm=TRUE)
nov.min.75q <- quantile(nov.dat$TMinF, probs = 0.75, na.rm=TRUE)
#dec
dec.min.25q <- quantile(dec.dat$TMinF, probs = 0.25)
dec.min.75q <- quantile(dec.dat$TMinF, probs = 0.75)

#maxs#
#jan
jan.max.25q <- quantile(jan.dat$TMaxF, probs = 0.25)
jan.max.75q <- quantile(jan.dat$TMaxF, probs = 0.75)
#feb
feb.max.25q <- quantile(feb.dat$TMaxF, probs = 0.25)
feb.max.75q <- quantile(feb.dat$TMaxF, probs = 0.75)
#mar
mar.max.25q <- quantile(mar.dat$TMaxF, probs = 0.25)
mar.max.75q <- quantile(mar.dat$TMaxF, probs = 0.75)
#apr
apr.max.25q <- quantile(apr.dat$TMaxF, probs = 0.25)
apr.max.75q <- quantile(apr.dat$TMaxF, probs = 0.75)
#may
may.max.25q <- quantile(may.dat$TMaxF, probs = 0.25)
may.max.75q <- quantile(may.dat$TMaxF, probs = 0.75)
#jun
jun.max.25q <- quantile(jun.dat$TMaxF, probs = 0.25)
jun.max.75q <- quantile(jun.dat$TMaxF, probs = 0.75)
#jul
jul.max.25q <- quantile(jul.dat$TMaxF, probs = 0.25)
jul.max.75q <- quantile(jul.dat$TMaxF, probs = 0.75)
#aug
aug.max.25q <- quantile(aug.dat$TMaxF, probs = 0.25)
aug.max.75q <- quantile(aug.dat$TMaxF, probs = 0.75)
#sep
sep.max.25q <- quantile(sep.dat$TMaxF, probs = 0.25)
sep.max.75q <- quantile(sep.dat$TMaxF, probs = 0.75)
#oct
oct.max.25q <- quantile(oct.dat$TMaxF, probs = 0.25)
oct.max.75q <- quantile(oct.dat$TMaxF, probs = 0.75)
#nov
nov.max.25q <- quantile(nov.dat$TMaxF, probs = 0.25, na.rm=TRUE)
nov.max.75q <- quantile(nov.dat$TMaxF, probs = 0.75, na.rm=TRUE)
#dec
dec.max.25q <- quantile(dec.dat$TMaxF, probs = 0.25)
dec.max.75q <- quantile(dec.dat$TMaxF, probs = 0.75)

#organizing quantiles into data frames
quan.min.25 <- data.frame(c(jan.min.25q, feb.min.25q, mar.min.25q, apr.min.25q, may.min.25q, jun.min.25q,
                            jul.min.25q, aug.min.25q, sep.min.25q, oct.min.25q, nov.min.25q, dec.min.25q))
#setting header
col_headings <- c('quan.min.25')
names(quan.min.25) <- col_headings

quan.min.75 <- data.frame(c(jan.min.75q, feb.min.75q, mar.min.75q, apr.min.75q, may.min.75q, jun.min.75q,
                            jul.min.75q, aug.min.75q, sep.min.75q, oct.min.75q, nov.min.75q, dec.min.75q))
#setting header
col_headings <- c('quan.min.75')
names(quan.min.75) <- col_headings

quan.max.25 <- data.frame(c(jan.max.25q, feb.max.25q, mar.max.25q, apr.max.25q, may.max.25q, jun.max.25q,
                            jul.max.25q, aug.max.25q, sep.max.25q, oct.max.25q, nov.max.25q, dec.max.25q))
#setting header
col_headings <- c('quan.max.25')
names(quan.max.25) <- col_headings

quan.max.75 <- data.frame(c(jan.max.75q, feb.max.75q, mar.max.75q, apr.max.75q, may.max.75q, jun.max.75q,
                            jul.max.75q, aug.max.75q, sep.max.75q, oct.max.75q, nov.max.75q, dec.max.75q))
#setting header
col_headings <- c('quan.max.75')
names(quan.max.75) <- col_headings

##monthly means for other variables##
#precip
jan.precip <- (mean(jan.dat$PPT_in)*31)
jan.precip
feb.precip <- (mean(feb.dat$PPT_in)*29)
feb.precip
mar.precip <- (mean(mar.dat$PPT_in)*31)
mar.precip
apr.precip <- (mean(apr.dat$PPT_in)*30)
apr.precip
may.precip <- (mean(may.dat$PPT_in)*31)
may.precip
jun.precip <- (mean(jun.dat$PPT_in)*30)
jun.precip
jul.precip <- (mean(jul.dat$PPT_in)*31)
jul.precip
aug.precip <- (mean(aug.dat$PPT_in)*31)
aug.precip
sep.precip <- (mean(sep.dat$PPT_in)*30)
sep.precip
oct.precip <- (mean(oct.dat$PPT_in)*31)
oct.precip
nov.precip <- (mean(nov.dat$PPT_in)*30)
nov.precip
dec.precip <- (mean(dec.dat$PPT_in)*31)
dec.precip

#ordering by month into data frame
precip <- data.frame(c(jan.precip, feb.precip, mar.precip, apr.precip, may.precip, jun.precip, jul.precip, aug.precip, sep.precip, oct.precip, nov.precip, dec.precip))
#setting header
col_headings <- c('precip')
names(precip) <- col_headings

#monthly lows
jan.lo <- mean(jan.dat$TMinF)
jan.lo
feb.lo <- mean(feb.dat$TMinF)
feb.lo
mar.lo <- mean(mar.dat$TMinF)
mar.lo
apr.lo <- mean(apr.dat$TMinF)
apr.lo
may.lo <- mean(may.dat$TMinF)
may.lo
jun.lo <- mean(jun.dat$TMinF)
jun.lo
jul.lo <- mean(jul.dat$TMinF)
jul.lo
aug.lo <- mean(aug.dat$TMinF)
aug.lo
sep.lo <- mean(sep.dat$TMinF)
sep.lo
oct.lo <- mean(oct.dat$TMinF)
oct.lo
nov.lo <- mean(nov.dat$TMinF, na.rm=TRUE)
nov.lo
dec.lo <- mean(dec.dat$TMinF)
dec.lo

#organizing lows into data frame
tmin <- data.frame(c(jan.lo, feb.lo, mar.lo, apr.lo, may.lo, jun.lo, jul.lo, aug.lo, sep.lo, oct.lo, nov.lo, dec.lo))
#setting header
col_headings <- c('tmin')
names(tmin) <- col_headings

#setting aside monthly highs
jan.hi <- mean(jan.dat$TMaxF)
jan.hi
feb.hi <- mean(feb.dat$TMaxF)
feb.hi
mar.hi <- mean(mar.dat$TMaxF)
mar.hi
apr.hi <- mean(apr.dat$TMaxF)
apr.hi
may.hi <- mean(may.dat$TMaxF)
may.hi
jun.hi <- mean(jun.dat$TMaxF)
jun.hi
jul.hi <- mean(jul.dat$TMaxF)
jul.hi
aug.hi <- mean(aug.dat$TMaxF)
aug.hi
sep.hi <- mean(sep.dat$TMaxF)
sep.hi
oct.hi <- mean(oct.dat$TMaxF)
oct.hi
nov.hi <- mean(nov.dat$TMaxF, na.rm=TRUE)
nov.hi
dec.hi <- mean(dec.dat$TMaxF)
dec.hi


#organizing highs into data frame
tmax <- data.frame(c(jan.hi, feb.hi, mar.hi, apr.hi, may.hi, jun.hi, jul.hi, aug.hi, sep.hi, oct.hi, nov.hi, dec.hi))
#setting header
col_headings <- c('tmax')
names(tmax) <- col_headings


#set up data frame

month <- unique(clim.dat$month)

df <- data.frame(month, precip, tmin, quan.min.25, quan.min.75, tmax, quan.max.25, quan.max.75) 

# Create plot names and subtitles

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


#phit that G

ggplot(df) +
  geom_bar(aes(x=factor(month, level =c(month.abb)), precip*5), color="lightblue", fill="lightblue", stat = "identity") +
  geom_line(aes(x=factor(month, level =c(month.abb)), tmin), linetype=1, color="goldenrod3", group=1) + # 10-15-23 removed 'size' argument because is deprecated. If default is not OK use linewidth to adjust
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


## end script

