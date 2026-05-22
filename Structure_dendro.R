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

# 1) Dendrometria ####
DATA_tot <- read.csv("DATA_tot_clean.CSV", dec=",", sep=";")  

# Mutate Management variable based on ID_plot pattern:
DATA_tot <- DATA_tot |> mutate(Management = ifelse(str_detect(ID_plot, "ng"), "Occasionally managed", 
                                                   ifelse(str_detect(ID_plot, "b"), "Unmanaged", "Regularly managed")))

# Convert Management variable to factor with specified order
DATA_tot$Management <- factor(DATA_tot$Management, levels = c("Regularly managed", "Occasionally managed", "Unmanaged"))

# Definisci i colori (coerenti con l'altro script)
my_cols <- c("Regularly managed" = "yellow2",
             "Occasionally managed" = "darkseagreen2", 
             "Unmanaged" = "turquoise4")

# --- Mean DBH ---
kruskal.test(Mean_DBH ~ Management, data = DATA_tot)
dunn_res_DBH <- dunnTest(Mean_DBH ~ Management, data = DATA_tot, method = "bonferroni")
print(dunn_res_DBH)
df_DBH <- dunn_res_DBH$res
library(openxlsx)
write.xlsx(df_DBH, "dunn_res_DBH.xlsx", rowNames = FALSE)

pvals <- dunn_res$res$P.adj
names(pvals) <- gsub("\\s*-\\s*", "-", dunn_res$res$Comparison)
letters_df <- multcompLetters(pvals, threshold = 0.05)$Letters
letters_df <- data.frame(Management = names(letters_df), letter = letters_df, row.names = NULL)

summary_letters <- DATA_tot %>% group_by(Management) %>%
  summarise(max_val = max(Mean_DBH, na.rm = TRUE)) %>%
  left_join(letters_df, by = "Management")

g1 <- ggplot(DATA_tot, aes(x = Management, y = Mean_DBH)) +
  geom_boxplot(aes(fill = Management), outliers = TRUE, size = 0.4) +
  geom_text(data = summary_letters, aes(x = Management, y = max_val + 5, label = letter), 
            vjust = 0, size = 6, color = "black") +
  scale_fill_manual(values = my_cols,
                    labels = c("Regularly managed" = "Regularly\nmanaged",
                               "Occasionally managed" = "Occasionally\nmanaged", 
                               "Unmanaged" = "Unmanaged")) +
  scale_x_discrete(labels = c("Regularly managed" = "Regularly\nmanaged",
                              "Occasionally managed" = "Occasionally\nmanaged",
                              "Unmanaged" = "Unmanaged")) +
  labs(x = "", y = "Mean DBH (cm)") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        legend.position = "none")

# --- Mean Height ---
kruskal.test(H_mean ~ Management, data = DATA_tot)
dunn_res_H <- dunnTest(H_mean ~ Management, data = DATA_tot, method = "bonferroni")
print(dunn_res_H)
df_H <- dunn_res_H$res
library(openxlsx)
write.xlsx(df_H, "dunn_res_H.xlsx", rowNames = FALSE)

pvals_H <- dunn_res_H$res$P.adj
names(pvals_H) <- gsub("\\s*-\\s*", "-", dunn_res_H$res$Comparison)
letters_df_H <- multcompLetters(pvals_H, threshold = 0.05)$Letters
letters_df_H <- data.frame(Management = names(letters_df_H), letter = letters_df_H, row.names = NULL)

summary_letters_H <- DATA_tot %>% group_by(Management) %>%
  summarise(max_val = max(H_mean, na.rm = TRUE)) %>%
  left_join(letters_df_H, by = "Management")

g2 <- ggplot(DATA_tot, aes(x = Management, y = H_mean)) +
  geom_boxplot(aes(fill = Management), outliers = TRUE, size = 0.4) +
  geom_text(data = summary_letters_H, aes(x = Management, y = max_val + 5, label = letter), 
            vjust = 0, size = 6, color = "black") +
  scale_fill_manual(values = my_cols,
                    labels = c("Regularly managed" = "Regularly\nmanaged",
                               "Occasionally managed" = "Occasionally\nmanaged", 
                               "Unmanaged" = "Unmanaged")) +
  scale_x_discrete(labels = c("Regularly managed" = "Regularly\nmanaged",
                              "Occasionally managed" = "Occasionally\nmanaged",
                              "Unmanaged" = "Unmanaged")) +
  labs(x = "", y = "Mean Height (m)") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        legend.position = "none")

# --- Basal Area ---
kruskal.test(tot_ba_ha ~ Management, data = DATA_tot)
dunn_res_BA <- dunnTest(tot_ba_ha ~ Management, data = DATA_tot, method = "bonferroni")
print(dunn_res_BA)
df_BA <- dunn_res_BA$res
library(openxlsx)
write.xlsx(df_BA, "dunn_res_BA.xlsx", rowNames = FALSE)

pvals_BA <- dunn_res_BA$res$P.adj
names(pvals_BA) <- gsub("\\s*-\\s*", "-", dunn_res_BA$res$Comparison)
letters_df_BA <- multcompLetters(pvals_BA, threshold = 0.05)$Letters
letters_df_BA <- data.frame(Management = names(letters_df_BA), letter = letters_df_BA, row.names = NULL)

summary_letters_BA <- DATA_tot %>% group_by(Management) %>%
  summarise(max_val = max(tot_ba_ha, na.rm = TRUE)) %>%
  left_join(letters_df_BA, by = "Management")

g3 <- ggplot(DATA_tot, aes(x = Management, y = tot_ba_ha)) +
  geom_boxplot(aes(fill = Management), outliers = TRUE, size = 0.4) +
  geom_text(data = summary_letters_BA, aes(x = Management, y = max_val + 5, label = letter), 
            vjust = 0, size = 6, color = "black") +
  scale_fill_manual(values = my_cols,
                    labels = c("Regularly managed" = "Regularly\nmanaged",
                               "Occasionally managed" = "Occasionally\nmanaged", 
                               "Unmanaged" = "Unmanaged")) +
  scale_x_discrete(labels = c("Regularly managed" = "Regularly\nmanaged",
                              "Occasionally managed" = "Occasionally\nmanaged",
                              "Unmanaged" = "Unmanaged")) +
  labs(x = "", y = expression(paste("Basal Area (m"^2, " ha"^{-1}, ")"))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        legend.position = "none")

# ---- PANEL  ----

# Temporary plot for legend extraction
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

# No legend panel
main_panel <- ggarrange(
  g1, g2, g3,
  ncol = 3,
  nrow = 1,
  labels = c("", "", ""),
  font.label = list(size = 20, face = "plain"),
  hjust = -0.5
)


print(main_panel)

# Save
png("Plot/Structure_dendro.png", width = 5000, height = 2500, res = 300)
print(main_panel)
dev.off()

# --- Variable variation --- 

# --- sd_DBH ---
kruskal.test(sd_DBH ~ Management, data = DATA_tot)
dunn_res_sd_DBH <- dunnTest(sd_DBH ~ Management, data = DATA_tot, method = "bonferroni")
print(dunn_res_sd_DBH)
df_sd_DBH <- dunn_res_sd_DBH$res
library(openxlsx)
write.xlsx(df_sd_DBH, "dunn_res_sd_DBH.xlsx", rowNames = FALSE)

pvals <- dunn_res_sd_DBH$res$P.adj
names(pvals) <- gsub("\\s*-\\s*", "-", dunn_res_sd_DBH$res$Comparison)
letters_df <- multcompLetters(pvals, threshold = 0.05)$Letters
letters_df <- data.frame(Management = names(letters_df), letter = letters_df, row.names = NULL)

summary_letters <- DATA_tot %>% group_by(Management) %>%
  summarise(max_val = max(sd_DBH, na.rm = TRUE)) %>%
  left_join(letters_df, by = "Management")

g4 <- ggplot(DATA_tot, aes(x = Management, y = sd_DBH)) +
  geom_boxplot(aes(fill = Management), outliers = TRUE, size = 0.4) +
  geom_text(data = summary_letters, aes(x = Management, y = max_val + 10, label = letter), 
            vjust = 0, size = 6, color = "black") +
  scale_fill_manual(values = my_cols,
                    labels = c("Regularly managed" = "Regularly\nmanaged",
                               "Occasionally managed" = "Occasionally\nmanaged", 
                               "Unmanaged" = "Unmanaged")) +
  scale_x_discrete(labels = c("Regularly managed" = "Regularly\nmanaged",
                              "Occasionally managed" = "Occasionally\nmanaged",
                              "Unmanaged" = "Unmanaged")) +
  labs(x = "", y = "Dev.Std DBH (cm)") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        legend.position = "none")

# --- sd_H ---
kruskal.test(H_std ~ Management, data = DATA_tot)
dunn_res_sd_H <- dunnTest(H_std ~ Management, data = DATA_tot, method = "bonferroni")
print(dunn_res_sd_H)
df_sd_H <- dunn_res_sd_H$res
library(openxlsx)
write.xlsx(df_sd_H, "dunn_res_sd_H.xlsx", rowNames = FALSE)

pvals <- dunn_res_sd_H$res$P.adj
names(pvals) <- gsub("\\s*-\\s*", "-", dunn_res_sd_H$res$Comparison)
letters_df_H <- multcompLetters(pvals, threshold = 0.05)$Letters
letters_df_H <- data.frame(Management = names(letters_df_H), letter = letters_df_H, row.names = NULL)

summary_letters_H <- DATA_tot %>% group_by(Management) %>%
  summarise(max_val = max(H_std, na.rm = TRUE)) %>%
  left_join(letters_df_H, by = "Management")

g5 <- ggplot(DATA_tot, aes(x = Management, y = H_std)) +
  geom_boxplot(aes(fill = Management), outliers = TRUE, size = 0.4) +
  geom_text(data = summary_letters_H, aes(x = Management, y = max_val + 5, label = letter), 
            vjust = 0, size = 6, color = "black") +
  scale_fill_manual(values = my_cols,
                    labels = c("Regularly managed" = "Regularly\nmanaged",
                               "Occasionally managed" = "Occasionally\nmanaged", 
                               "Unmanaged" = "Unmanaged")) +
  scale_x_discrete(labels = c("Regularly managed" = "Regularly\nmanaged",
                              "Occasionally managed" = "Occasionally\nmanaged",
                              "Unmanaged" = "Unmanaged")) +
  labs(x = "", y = "Dev.Std Height (m)") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        legend.position = "none")

# ---- PANEL 2 ----

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


legend <- get_legend(temp_legend_plot)


main_panel <- ggarrange(
  g4, g5,
  ncol = 2,
  nrow = 1,
  labels = c("", "", ""),
  font.label = list(size = 20, face = "plain"),
  hjust = -0.5
)


print(main_panel)

# Save
png("Variazione_structure_dendro.png", width = 4000, height = 2500, res = 300)
print(main_panel)
dev.off()
