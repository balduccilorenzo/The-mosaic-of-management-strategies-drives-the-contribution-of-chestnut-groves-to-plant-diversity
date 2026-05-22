require(tidyverse)
require(betareg)  

setwd("~/TESI/dataset")
sp <- read.csv(file = "Data_Entry.xlsx - Data_Entry.csv", dec = ",", sep = ",")
diagnostiche <- read.csv(file = "Lista specie habitat 9260.xlsx - Foglio1.csv", dec = ",", sep = ",")

diagnostiche_vec <- diagnostiche$Specie.diagnostiche.habitat.9260  

# Filter dataset for the understory (herbaceous layer)
understory_tot <- sp[sp$Layer == "Herb", ]

# Group data by plot and calculate total and diagnostic coverage
result <- understory_tot %>%
  group_by(ID_plot) %>%
  summarise(
    copertura_tot = sum(Abundance), 
    copertura_diagnostiche = sum(Abundance[Species %in% diagnostiche_vec])
  ) %>%
  # Calculate percentage cover of diagnostiche species per plot
  mutate(cop_perc_diagnostiche = (copertura_diagnostiche / copertura_tot)) 

# Read canopy cover data and subset relevant columns
str <- read.csv(file = "DATA_tot_clean.CSV", dec = ",", sep = ";")
str <- str[, c(1,4,6,7,8,13,14,15,16,17,18,19,20,21,22)]

# Join with the result dataframe by plot ID
result <- result %>%
  left_join(str, by = "ID_plot")

# Adjust 0 e 1 values based on Smithson & Verkuilen correction (2006)
n <- nrow(result)
result$cop_perc_diagnostiche <- (result$cop_perc_diagnostiche * (n - 1) + 0.5) / n

# Spearman's correlation for the best variable (CC)
cor_spear <- cor(result[, sapply(result, is.numeric)], method = "spearman", use = "complete.obs")
cor_target <- cor_spear[, "cop_perc_diagnostiche"]
cor_target_sorted <- sort(abs(cor_target), decreasing = TRUE)
cor_target_sorted


model <- betareg(cop_perc_diagnostiche ~ CC_media, data = result)
summary(model)

# residuals
res_q <- residuals(model, type = "quantile")

complete_cases <- complete.cases(result$cop_perc_diagnostiche, result$CC_media)
plot(result$CC_media[complete_cases], res_q,
     xlab = "CC_media", ylab = "Quantile residuals",
     main = "Residuals vs predictor")
abline(h = 0, col = "red")

model2 <- betareg(cop_perc_diagnostiche ~ CC_media | CC_media, data = result)
summary(model2)
