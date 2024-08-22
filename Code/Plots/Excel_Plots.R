#############################################################
##########    AUTOMATE EXCEL PLOTS    #######################
#############################################################

# 8-18-2024
# written by Annie Kellner, annie.kellner@colostate.edu


# ------------  Create Directory for Plots  ----------------------- #

xl_plots_dir <- paste(plots_dir,"Excel_Plots", sep = "/") # Change this folder name -- ASK TREVOR

if (!dir.exists(xl_plots_dir)){
  dir.create(xl_plots_dir)}


# ----------  Loop to reformat dataframes for use with ggplot2 --------------------  #


df <- monthSum$results_baseline[1:12,]
df <- df %>%  
  select(-month) %>%
  mutate(Month = month.abb) %>% glimpse()
           
           
           month(df$month, label = TRUE, abbr = TRUE, locale = Sys.getlocale("LC_TIME")))


ggplot(df) +
geom_col(aes(x = factor(Month, levels = c(month.abb)), y = Avg_TMeanF), color="#CBC598", fill="#CBC598") 


#Historical: #CBC598

#col_names <- colnames(df)

#dfT <- data.table::transpose(df) 

#rownames(df) <- col_names
#colnames(df) <- month.abb


#rownames(dfT) <- month.abb
#colnames(dfT) <- col_names

#### USE CODE FROM CLIMOGRAPHS SCRIPT
