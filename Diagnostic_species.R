library(tidyverse)
library(FSA)
library(multcompView)
library(cowplot)




# --- 1. Setup and data call ----
diagnostiche <- read.csv(file = "Lista specie habitat 9260.xlsx - Foglio1.csv", dec = ",", sep = ",")
sp_cast <- read.csv(file = "Data_Entry.xlsx - Data_Entry.csv", dec = ",", sep = ",")

diagnostiche_vett <- diagnostiche$Specie.diagnostiche.habitat.9260

diagnostiche_df <- sp_cast %>%
  filter(Species %in% diagnostiche_vett) %>%
  select(-Campione.raccolto, -X)


# --- 2. Mean cover ----
copertura_diagnostiche <- diagnostiche_df %>%
  group_by(ID_plot) %>%
  summarise(mean_cover = mean(Abundance, na.rm = TRUE), .groups = "drop") %>%
  mutate(Management = case_when(
    str_detect(ID_plot, "ng") ~ "Occasionally managed",
    str_detect(ID_plot, "b")  ~ "Unmanaged",
    TRUE                       ~ "Regularly managed"
  ))

copertura_diagnostiche$Management <- factor(copertura_diagnostiche$Management,
                                            levels = c("Regularly managed", "Occasionally managed", "Unmanaged"))

# --- Significance letter ---
kruskal.test(mean_cover ~ Management, data = copertura_diagnostiche)
dunn_diagnostic_Cover <- dunnTest(mean_cover ~ Management, data = copertura_diagnostiche, method = "bonferroni")
print(dunn_diagnostic_Cover)
df_diagnostic_Cover <- dunn_diagnostic_Cover$res
library(openxlsx)
write.xlsx(df_diagnostic_Cover, "dunn_res_diagnostic_Cover.xlsx", rowNames = FALSE)

if(any(dunn_total$res$P.adj < 0.05)){
  pvals_total <- setNames(dunn_total$res$P.adj, gsub(" - ", "-", dunn_total$res$Comparison))
  letters_total <- multcompLetters(pvals_total, threshold = 0.05)$Letters
  letters_df_total <- data.frame(
    Management = names(letters_total),
    lettere = letters_total,
    stringsAsFactors = FALSE
  )
} else {
  letters_df_total <- data.frame(
    Management = levels(copertura_diagnostiche$Management),
    lettere = NA
  )
}

df_total_summary <- copertura_diagnostiche %>%
  group_by(Management) %>%
  summarise(max_y = max(mean_cover, na.rm = TRUE), .groups = "drop") %>%
  left_join(letters_df_total, by = "Management")


## --- Mean cover plot ----
plot_cover_total <- ggplot(copertura_diagnostiche, aes(x = Management, y = mean_cover, fill = Management)) +
  geom_boxplot(size = 0.4, aes(color = Management)) +
  geom_text(data = df_total_summary, aes(x = Management, y = max_y + 5, label = lettere), size = 6, color = "black", na.rm = TRUE) +
  labs(y = "Cover of diagnostic species (%)", x = "") +
  scale_fill_manual(values = c("Regularly managed" = "yellow2",
                               "Occasionally managed" = "darkseagreen2", 
                               "Unmanaged" = "turquoise4"),
                    labels = c("Regularly managed" = "Regularly\nmanaged",
                               "Occasionally managed" = "Occasionally\nmanaged",
                               "Unmanaged" = "Unmanaged")) +
  scale_color_manual(values = c("Regularly managed" = "black", "Occasionally managed" = "black", "Unmanaged" = "black")) +
  scale_x_discrete(labels = c("Regularly managed" = "Regularly\nmanaged",
                              "Occasionally managed" = "Occasionally\nmanaged",
                              "Unmanaged" = "Unmanaged")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        plot.title = element_text(size = 20, hjust = 0.5),
        legend.position = "none")
plot_cover_total




# --- 3. Mean cover per layer ----
copertura_layer <- diagnostiche_df %>%
  group_by(ID_plot, Layer) %>%
  summarise(mean_cover = mean(Abundance, na.rm = TRUE), .groups = "drop") %>%
  mutate(Management = case_when(
    str_detect(ID_plot, "ng") ~ "Occasionally managed",
    str_detect(ID_plot, "b")  ~ "Unmanaged",
    TRUE                       ~ "Regularly managed"
  ))

copertura_layer$Management <- factor(copertura_layer$Management,
                                     levels = c("Regularly managed", "Occasionally managed", "Unmanaged"))

#kruskal
herb_data <- copertura_layer %>% filter(Layer == "Herb")
kruskal.test(mean_cover ~ Management, data = herb_data)
shrub_data <- copertura_layer %>% filter(Layer == "Shrub")
kruskal.test(mean_cover ~ Management, data = shrub_data)
tree_data <- copertura_layer %>% filter(Layer == "Tree")
kruskal.test(mean_cover ~ Management, data = tree_data)

# --- Significance letter per layer ---
letters_per_layer_list <- list()
for(l in unique(copertura_layer$Layer)){
  data_layer <- copertura_layer %>% filter(Layer == l)
  dunn_res <- dunnTest(mean_cover ~ Management, data = data_layer, method = "bonferroni")
  if(any(dunn_res$res$P.adj < 0.05)){
    pvals <- setNames(dunn_res$res$P.adj, gsub(" - ", "-", dunn_res$res$Comparison))
    letters_layer <- multcompLetters(pvals, threshold = 0.05)$Letters
    df_letters <- data_layer %>%
      group_by(Management) %>%
      summarise(max_y = max(mean_cover, na.rm = TRUE), .groups = "drop") %>%
      left_join(data.frame(Management = names(letters_layer),
                           lettere = letters_layer,
                           stringsAsFactors = FALSE),
                by = "Management") %>%
      mutate(Layer = l)
    letters_per_layer_list[[l]] <- df_letters
  }
}

letters_layer_df <- bind_rows(letters_per_layer_list)



## --- Mean cover per layer plot ----
plot_cover_layer <- ggplot(copertura_layer, aes(x = Layer, y = mean_cover, fill = Management)) +
  geom_boxplot(size = 0.4, aes(color = Management), outliers = TRUE) +
  labs(y = "Cover of diagnostic species (%)", x = "") +
  geom_text(data = letters_layer_df,
            aes(x = Layer, y = max_y + 20, label = lettere, group = Management),
            size = 6, color = "black", position = position_dodge(width = 0.75)) +
  scale_fill_manual(values = c("Regularly managed" = "yellow2",
                               "Occasionally managed" = "darkseagreen2", 
                               "Unmanaged" = "turquoise4"),
                    labels = c("Regularly managed" = "Regularly\nmanaged",
                               "Occasionally managed" = "Occasionally\nmanaged",
                               "Unmanaged" = "Unmanaged"),
                    name = "") +
  scale_color_manual(values = c("Regularly managed" = "black", "Occasionally managed" = "black", "Unmanaged" = "black"), guide = "none") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.position = "none")
plot_cover_layer


   
# ---- table of individual comparisons between managements and layers ----
dunn_results_per_layer <- list()
for(l in unique(copertura_layer$Layer)) {
 cat("\n============================================\n")
    cat("LAYER:", l, "\n")
    cat("============================================\n")
    data_layer <- copertura_layer %>% filter(Layer == l)
# Dunn test
    dunn_diagnostic_cover_layer <- dunnTest(mean_cover ~ Management, data = data_layer, method = "bonferroni")
    #  # Add asterisk column for significance
    df_diagnostic_cover_layer <- dunn_diagnostic_cover_layer$res %>%
    mutate(significant = ifelse(P.adj < 0.05, "*", ""))
    print(df_diagnostic_cover_layer)
    #  # Save
    dunn_results_per_layer[[l]] <- df_diagnostic_cover_layer
    }
    all_dunn_results <- bind_rows(dunn_results_per_layer, .id = "Layer")
    all_dunn_results
    library(openxlsx)
    write.xlsx(all_dunn_results, "dunn_res_diagnostic_cover_layer.xlsx", rowNames = FALSE)




# --- 4. Number of diagnostic species per layer----

# --- Filter and add management column ---
diagnostiche_df1 <- sp_cast %>%
  filter(Species %in% diagnostiche_vett) %>%
  select(ID_plot, Layer, Species, Abundance) %>%
  mutate(Management = ifelse(str_detect(ID_plot, "ng"), "Occasionally managed",
                             ifelse(str_detect(ID_plot, "b"), "Unmanaged", "Regularly managed")))

diagnostiche_df1$Management <- factor(diagnostiche_df1$Management,
                                      levels = c("Regularly managed", "Occasionally managed", "Unmanaged"))

# --- Richness per layer ---
richness_layer <- diagnostiche_df1 %>%
  group_by(ID_plot, Layer, Management) %>%
  summarise(richness = n_distinct(Species), .groups = "drop")

# Management as factor
richness_layer$Management <- factor(richness_layer$Management,
                                    levels = c("Regularly managed", "Occasionally managed", "Unmanaged"))


# --- Letter of significance per layer ---
letters_layer_list <- list()

for(l in unique(richness_layer$Layer)) {
  layer_data <- richness_layer %>% filter(Layer == l)
  dunn_res <- dunnTest(richness ~ Management, data = layer_data, method = "bonferroni")

  if(any(dunn_res$res$P.adj < 0.05)) {
    pvals <- setNames(dunn_res$res$P.adj, gsub(" - ", "-", dunn_res$res$Comparison))
    letters <- multcompLetters(pvals, threshold = 0.05)$Letters
    letters_layer_list[[l]] <- data.frame(
      Layer = l,
      Management = names(letters),
      lettere = letters,
      stringsAsFactors = FALSE
    )
  }
}

kruskal.test(richness ~ Management, data = layer_data)
letters_layer_df <- bind_rows(letters_layer_list)

#kruskal
herb1_data <- richness_layer %>% filter(Layer == "Herb")
kruskal.test(richness ~ Management, data = herb1_data)
shrub1_data <- richness_layer %>% filter(Layer == "Shrub")
kruskal.test(richness ~ Management, data = shrub1_data)
tree1_data <- richness_layer %>% filter(Layer == "Tree")
kruskal.test(richness ~ Management, data = tree1_data)

# Letter positioning
summary_layer_plot <- richness_layer %>%
  group_by(Layer, Management) %>%
  summarise(max_rich = max(richness, na.rm = TRUE), .groups = "drop") %>%
  left_join(letters_layer_df, by = c("Layer", "Management"))

summary_layer_plot$Management <- factor(summary_layer_plot$Management,
                                        levels = c("Regularly managed", "Occasionally managed", "Unmanaged"))

summary_layer_plot <- summary_layer_plot %>%
  mutate(lettere = case_when(
    Layer == "Shrub" & Management %in% c("Regularly managed", "Unmanaged") ~ "a",
    Layer == "Shrub" & Management == "Occasionally managed" ~ "b",
    Layer == "Tree" & Management %in% c("Regularly managed", "Occasionally managed") ~ "c",
    Layer == "Tree" & Management == "Unmanaged" ~ "d",
    TRUE ~ lettere
  ))



## --- Number per layer plot----
plot_layer <- ggplot(richness_layer, aes(x = Layer, y = richness, fill = Management)) +
  geom_boxplot(size = 0.4, aes(color = Management), outliers = TRUE) +
  labs(y = "N. of diagnostic species", x = "") +
  geom_text(data = summary_layer_plot,
            aes(x = Layer, y = max_rich + 1, label = lettere, group = Management),
            size = 6, color = "black", position = position_dodge(width = 0.75)) +
  scale_fill_manual(values = c("Regularly managed" = "yellow2",
                               "Occasionally managed" = "darkseagreen2", 
                               "Unmanaged" = "turquoise4"),
                    labels = c("Regularly managed" = "Regularly\nmanaged",
                               "Occasionally managed" = "Occasionally\nmanaged",
                               "Unmanaged" = "Unmanaged"),
                    name = "") +
  scale_color_manual(values = c("Regularly managed" = "black", "Occasionally managed" = "black", "Unmanaged" = "black"), guide = "none") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.position = "none")

plot_layer


    # ---- Table ----
dunn_results_layer <- list()
for(l in unique(richness_layer$Layer)) {
cat("\n============================================\n")
  cat("LAYER:", l, "\n")
  cat("============================================\n")
  layer_data <- richness_layer %>% filter(Layer == l)
    #  # Dunn test
  dunn_res <- dunnTest(richness ~ Management, data = layer_data, method = "bonferroni")
  df_res <- dunn_res$res %>%
  mutate(significant = ifelse(P.adj < 0.05, "*", ""))
  print(df_res)
  dunn_results_layer[[l]] <- df_res
    }
    all_dunn_richness_layer <- bind_rows(dunn_results_layer, .id = "Layer")
    all_dunn_richness_layer
    library(openxlsx)
    write.xlsx(all_dunn_richness_layer, "dunn_res_richness_layer.xlsx", rowNames = FALSE)




# --- 5. Number ----
richness_total <- richness_layer %>%
  group_by(ID_plot, Management) %>%
  summarise(total_richness = sum(richness, na.rm = TRUE), .groups = "drop")

kruskal.test(total_richness ~ Management, data = richness_total) #non significativo
dunn_total <- dunnTest(total_richness ~ Management, data = richness_total, method = "bonferroni")
print(dunn_total)
df_tot_richness <- dunn_total$res
library(openxlsx)
write.xlsx(df_tot_richness, "dunn_res_tot_richness.xlsx", rowNames = FALSE) 

# Significance letter
if(any(dunn_total$res$P.adj < 0.05)) {
  pvals_total <- setNames(dunn_total$res$P.adj, gsub(" - ", "-", dunn_total$res$Comparison))
  letters_total <- multcompLetters(pvals_total, threshold = 0.05)$Letters
  letters_df_total <- data.frame(
    Management = names(letters_total),
    lettere = letters_total,
    stringsAsFactors = FALSE
  )
  
  summary_total <- richness_total %>%
    group_by(Management) %>%
    summarise(max_rich = max(total_richness, na.rm = TRUE), .groups = "drop") %>%
    left_join(letters_df_total, by = "Management")
} else {
  summary_total <- richness_total %>%
    group_by(Management) %>%
    summarise(max_rich = max(total_richness, na.rm = TRUE), .groups = "drop") %>%
    mutate(lettere = NA)
}


## --- Number plot----
plot_total <- ggplot(richness_total, aes(x = Management, y = total_richness, fill = Management)) +
  geom_boxplot(size = 0.4, aes(color = Management), outliers = TRUE) +
  geom_text(data = summary_total,
            aes(x = Management, y = max_rich + 1, label = lettere),
            size = 6, color = "black", na.rm = TRUE) +
  labs(y = "N. of diagnostic species", x = "") +
  scale_fill_manual(values = c("Regularly managed" = "yellow2",
                               "Occasionally managed" = "darkseagreen2", 
                               "Unmanaged" = "turquoise4"),
                    labels = c("Regularly managed" = "Regularly\nmanaged",
                               "Occasionally managed" = "Occasionally\nmanaged",
                               "Unmanaged" = "Unmanaged")) +
  scale_color_manual(values = c("Regularly managed" = "black", "Occasionally managed" = "black", "Unmanaged" = "black")) +
  scale_x_discrete(labels = c("Regularly managed" = "Regularly\nmanaged",
                              "Occasionally managed" = "Occasionally\nmanaged",
                              "Unmanaged" = "Unmanaged")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        plot.title = element_text(size = 20, hjust = 0.5),
        legend.position = "none")
plot_total



# ------------------------- PANEL (A + B) -------------------------

library(ggpubr)

# 1️⃣ --- A: Total ---
panel_A <- ggarrange(
  plot_total,
  plot_cover_total,
  ncol = 2,
  nrow = 1,
  labels = c("a)", ""),
  font.label = list(size = 20, face = "plain"),
  hjust = -0.5
)

# 2️⃣ --- B: per layer common legedn) ---
p_legend <- plot_layer +
  scale_fill_manual(values = c("Regularly managed" = "yellow2",
                               "Occasionally managed" = "darkseagreen2", 
                               "Unmanaged" = "turquoise4"),
                    labels = c("Regularly managed" = "Regularly managed",
                               "Occasionally managed" = "Occasionally managed",
                               "Unmanaged" = "Unmanaged"),  
                    name = "") +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 18),
    legend.box = "horizontal"
  ) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))

p_cover_noleg <- plot_cover_layer + 
  theme(legend.position = "none")

panel_B <- ggarrange(
  p_legend,
  p_cover_noleg,
  ncol = 2,
  nrow = 1,
  common.legend = TRUE,
  legend = "bottom",
  labels = c("b)", ""),
  font.label = list(size = 20, face = "plain"),
  hjust = -0.5
)

# 3️⃣ ---Final panel) ---
panel_final <- ggarrange(
  panel_A,
  panel_B,
  ncol = 1,
  nrow = 2,
  heights = c(1, 1.15)
)

print(panel_final)


# Salva
#png("Plot/Diagnostic_species.png", width = 4200, height = 3200, res = 300)
#print(panel_final)
#dev.off()




