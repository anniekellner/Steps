##  ------  PLOTS - CHANGE FROM HISTORICAL  ------------ ##

for(i in 1:length(diffHist)){ 
  diffHist[[i]] = add_month(diffHist[[i]])
}

# Create new df's to compare changes within-scenario


# Scenario 1 (e.g., SSP2-4.5)

s1f1 <- diffHist[[1]][1:12,] # eliminate summary rows
s1f2 <- diffHist[[2]][1:12,]

s1f1 <- s1f1 %>%
  rename_with(~paste0(., "_F1"), starts_with("Avg")) %>%
  rename_with(~paste0(., "_F1"), starts_with("Abs")) 

s1f2 <- s1f2 %>%
  rename_with(~paste0(., "_F2"), starts_with("Avg")) %>%
  rename_with(~paste0(., "_F2"), starts_with("Abs"))

S1 <- full_join(s1f1, s1f2, by = "Month") 


# Scenario 2 (e.g., SSP2-8.5)

s2f1 <- diffHist[[3]][1:12,] # eliminate summary rows
s2f2 <- diffHist[[4]][1:12,]

s2f1 <- s2f1 %>%
  rename_with(~paste0(., "_F1"), starts_with("Avg")) %>%
  rename_with(~paste0(., "_F1"), starts_with("Abs")) 

s2f2 <- s2f2 %>%
  rename_with(~paste0(., "_F2"), starts_with("Avg")) %>%
  rename_with(~paste0(., "_F2"), starts_with("Abs"))

S2 <- full_join(s2f1, s2f2, by = "Month") 
