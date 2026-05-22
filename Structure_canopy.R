# 2) Chiama ####
library(writexl)                  
library(tidyverse)                
library(e1071)                   
library(Matrix)                  
library(lme4)                   
library(corrplot)                
library(rpart)                  
library(ggplot2)                
library(plotly)                 
library(dplyr)                  
library(FSA)
library(multcompView)
library(ggpubr)

# Read dataset
DATA_tot <- read.csv("DATA_tot_clean.CSV", dec = ",", sep = ";")

# Rename the variable 'Managment' to 'Management' (typo fix)
DATA_tot <- DATA_tot %>%
  rename(Management = Managment)

# Recode Management variable based on pattern in ID_plot:
DATA_tot <- DATA_tot |> 
  mutate(Management = ifelse(str_detect(ID_plot, "ng"), "Occasionally managed", 
                             ifelse(str_detect(ID_plot, "b"), "Unmanaged", "Regularly managed")))

# Define factor levels for Management variable
DATA_tot$Management <- factor(DATA_tot$Management, levels = c("Regularly managed", "Occasionally managed", "Unmanaged"))

# Define colors
my_cols <- c("Regularly managed" = "yellow2",
             "Occasionally managed" = "darkseagreen2", 
             "Unmanaged" = "turquoise4")

# --- Significance for CC_media (Canopy Cover)---
kruskal.test(CC_media ~ Management, data = DATA_tot)
dunn_res_CC <- dunnTest(CC_media ~ Management, data = DATA_tot, method = "bonferroni")
print(dunn_res_CC)
df_CC <- dunn_res_CC$res
library(openxlsx)
write.xlsx(df_CC, "dunn_res_CC.xlsx", rowNames = FALSE)

pvals_CC <- dunn_res_CC$res$P.adj
names(pvals_CC) <- gsub("\\s*-\\s*", "-", dunn_res_CC$res$Comparison)
letters_df_CC <- multcompLetters(pvals_CC, threshold = 0.05)$Letters
letters_df_CC <- data.frame(Management = names(letters_df_CC), letter = letters_df_CC, row.names = NULL)

summary_letters_CC <- DATA_tot %>%
  group_by(Management) %>%
  summarise(max_val = max(CC_media, na.rm = TRUE)) %>%
  left_join(letters_df_CC, by = "Management")

g5 <- ggplot(DATA_tot, aes(x = Management, y = CC_media)) +
  geom_boxplot(aes(fill = Management), outliers = TRUE, size = 0.4) +
  geom_text(data = summary_letters_CC, aes(x = Management, y = max_val + 0.07, label = letter), 
            vjust = 0, size = 6, color = "black") +
  scale_fill_manual(values = my_cols,
                    labels = c("Regularly managed" = "Regularly\nmanaged",
                               "Occasionally managed" = "Occasionally\nmanaged", 
                               "Unmanaged" = "Unmanaged")) +
  scale_x_discrete(labels = c("Regularly managed" = "Regularly\nmanaged",
                              "Occasionally managed" = "Occasionally\nmanaged",
                              "Unmanaged" = "Unmanaged")) +
  labs(x = "", y = "Crown Cover") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        legend.position = "none")

# --- Significance for CP_media (Canopy Porosity)---
kruskal.test(CP_media ~ Management, data = DATA_tot)
dunn_res_CP <- dunnTest(CP_media ~ Management, data = DATA_tot, method = "bonferroni")
print(dunn_res_CP)
df_CP <- dunn_res_CP$res
library(openxlsx)
write.xlsx(df_CP, "dunn_res_CP.xlsx", rowNames = FALSE)

g6 <- ggplot(DATA_tot, aes(x = Management, y = CP_media)) +
  geom_boxplot(aes(fill = Management), outliers = TRUE, size = 0.4) +
  scale_fill_manual(values = my_cols,
                    labels = c("Regularly managed" = "Regularly\nmanaged",
                               "Occasionally managed" = "Occasionally\nmanaged", 
                               "Unmanaged" = "Unmanaged")) +
  scale_x_discrete(labels = c("Regularly managed" = "Regularly\nmanaged",
                              "Occasionally managed" = "Occasionally\nmanaged",
                              "Unmanaged" = "Unmanaged")) +
  labs(x = "", y = "Crown Porosity") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        legend.position = "none")

# --- Significance for Le_media (LAI)---
kruskal.test(Le_media ~ Management, data = DATA_tot)
dunn_res_Le <- dunnTest(Le_media ~ Management, data = DATA_tot, method = "bonferroni")
print(dunn_res_Le)
df_Le <- dunn_res_Le$res
library(openxlsx)
write.xlsx(df_Le, "dunn_res_Le.xlsx", rowNames = FALSE)

pvals_Le <- dunn_res_Le$res$P.adj
names(pvals_Le) <- gsub("\\s*-\\s*", "-", dunn_res_Le$res$Comparison)
letters_df_Le <- multcompLetters(pvals_Le, threshold = 0.05)$Letters
letters_df_Le <- data.frame(Management = names(letters_df_Le), letter = letters_df_Le, row.names = NULL)

summary_letters_Le <- DATA_tot %>%
  group_by(Management) %>%
  summarise(max_val = max(Le_media, na.rm = TRUE)) %>%
  left_join(letters_df_Le, by = "Management")

g7 <- ggplot(DATA_tot, aes(x = Management, y = Le_media)) +
  geom_boxplot(aes(fill = Management), outliers = TRUE, size = 0.4) +
  geom_text(data = summary_letters_Le, aes(x = Management, y = max_val + 0.04, label = letter), 
            vjust = 0, size = 6, color = "black") +
  scale_fill_manual(values = my_cols,
                    labels = c("Regularly managed" = "Regularly\nmanaged",
                               "Occasionally managed" = "Occasionally\nmanaged", 
                               "Unmanaged" = "Unmanaged")) +
  scale_x_discrete(labels = c("Regularly managed" = "Regularly\nmanaged",
                              "Occasionally managed" = "Occasionally\nmanaged",
                              "Unmanaged" = "Unmanaged")) +
  labs(x = "", y = "Leaf Area Index") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        legend.position = "none")




# ---- 3-colum, 1-row panel ----

# Temporary plot
temp_legend_plot <- ggplot(DATA_tot, aes(x = Management, y = Mean_DBH, fill = Management)) +
  geom_boxplot() +
  scale_fill_manual(values = my_cols,
                    labels = c("Regularly managed" = "Regularly managed",
                               "Occasionally managed" = "Occasionally managed", 
                               "Unmanaged" = "Unmanaged"),
                    name = NULL) +  
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.background = element_blank(),
        legend.key = element_blank(),      
        legend.text = element_text(size = 16))

# Legend 
legend <- get_legend(temp_legend_plot)

# Panel without legend
main_panel <- ggarrange(
  g5, g6, g7,
  ncol = 3,
  nrow = 1,
  labels = c("", "", ""),
  font.label = list(size = 20, face = "plain"),
  hjust = -0.5
)


print(main_panel)

# Save
png("Structure_canopy1.png", width = 5000, height = 2500, res = 300)
print(main_panel)
dev.off()
