#################################################    
###   ADD WINDOWS SYSTEM FONTS TO RSTUDIO   #####
#################################################

# Using Calibri font for plots as of October 2024
# written by Annie Kellner 
# 10-21-2024

# THIS SCRIPT SHOULD ONLY BE RUN ONCE, UNLESS:
  # a) new fonts are added to the database -or-
  # b) the script throws an error when Calibri (or another Windows system font)
    # is attempted

# An error could result, theoretically, if R or RStudio are updated and the 
  # previous imports are deleted


font_import(prompt = FALSE)
loadfonts(device = "win")
