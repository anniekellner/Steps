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
df <- df %>%  select(-month)

col_names <- colnames(df)

dfT <- data.table::transpose(df) 

rownames(dfT) <- col_names
colnames(dfT) <- month.abb






hist_Tmean_ave <- ggplot(data = df, aes(x = month, y = Avg_TMeanF)) +
  geom_bar(aes(color = "#CBC598", fill = "#CBC598"))
  
                         

