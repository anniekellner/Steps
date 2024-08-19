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


 

df2 <- pivot_longer(
  data = df, 
  cols = starts_with("Avg"), 
  names_to = "Month",
  names_prefix = )

# Pivot plot so that columns names are month.abb and rows are Avg_
# See this example (or others)

billboard
billboard %>%
  pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    names_prefix = "wk",
    values_to = "rank",
    values_drop_na = TRUE
  )



hist_Tmean_ave <- ggplot(data = df, aes(x = month, y = Avg_TMeanF)) +
  geom_bar(aes(color = "#CBC598", fill = "#CBC598"))
  
                         

