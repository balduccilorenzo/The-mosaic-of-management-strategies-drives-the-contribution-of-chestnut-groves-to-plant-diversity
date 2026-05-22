library(tidyverse)
library(car)

# --- Set and data call ---
setwd("~/TESI/dataset")
data_tot <- read.csv("DATA_tot_clean.CSV", dec = ",", sep = ";")

# --- ID_plot as rownames---
rownames(data_tot) <- data_tot$ID_plot
data_tot$ID_plot <- NULL

# --- Excluding unrelevant columns ---
data_tot1 <- data_tot[, -c(2,3,8,9,10,11,15,16,17,18,19)]

# Saturated model analysis:
# Test normality of the response variable 'Shannon' using Shapiro-Wilk test
shapiro.test(data_tot1$Shannon) 
#shapiro.test(data_tot$ricchezza)
vars <- data_tot[, sapply(data_tot, is.numeric)]
cor_spear <- cor(vars, method = "spearman", use = "complete.obs")
round(cor_spear, 2)

# --- Satureted ---
lm_full <- lm(Shannon ~ ., data = data_tot1)

# --- Ecological selection: remove variables while retaining the most significant ones ---
lm_reduced <- lm(Shannon ~ L_media + CP_media + CC_media + tot_ba_ha + Mean_DBH + H_mean,
                 data = data_tot1)

# --- Stepwise backward on reduced model ---
step_lm <- step(lm_reduced, direction = "backward", trace = 1)

# --- Final model ---
lm_final <- step_lm
summary(lm_final)

# --- Checking VIF ---
vif_final <- vif(lm_final)
print(round(vif_final,2))

# --- Checking correlation ---
cor(data_tot1[, names(coef(lm_final))[-1]], use = "complete.obs")

# --- Final model 1: variabili finali con coeff, significatività, VIF ---
lm5 <- lm(Shannon ~ L_media + CP_media + H_mean + CC_media,
          data = data_tot1)
summary(lm5)

cor(data_tot1[, c("tot_ba_ha", "Mean_DBH", "H_mean")])
cor(data_tot1[, c("L_media", "CP_media", "CC_media")])