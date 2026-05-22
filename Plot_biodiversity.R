rm(list=ls()) #remove all elements in work environment
#----NOTES----
#MANAGED IS NOW REGULARLY MANAGED
#UNMANAGED IS NOW OCCASIONALLY MANAGED
#WOODLAND IS NOW UNMANAGED


#----R LIBRARY----
library(tidyr)
library(dplyr)
library(ggplot2)
library(vegan)
library(FSA)
library(multcompView)
library(patchwork)
library(cowplot)
library(ggpubr)



#----DATASET---- 
#import dataset (species and environmental/station data)
data_species <- read.csv2(file = "Data/Data_Entry.csv", sep=";", dec = ",")
dati_staz <- read.csv2(file="Data/Dati_Stazionali.csv", sep=";", dec= ",")

#check for duplicates
data_species %>%
  dplyr::group_by(ID_plot, Species, Layer) %>%  
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L)

data_entry <- data_species[1:2902, 1:4]
dati_stazionali <- dati_staz[1:135, 1:17]

#transform variables
data_species$Species <- as.character(data_species$Species)
data_species$ID_plot <- as.character(data_species$ID_plot)

#add Management column
data_entry <- data_entry |> mutate(Management = case_when(
  ID_plot %in% paste0(sprintf("%02d", 1:15), "_g") ~ "Managed",
  ID_plot %in% paste0(sprintf("%02d", 1:15), "_ng") ~ "Unmanaged",
  ID_plot %in% paste0(sprintf("%02d", 1:15), "_b") ~ "Woodland"
))


#transform dataset into wide format
data_wide <- data_entry |> 
  pivot_wider(names_from = c(Species, Layer),
              values_from = Abundance,
              values_fill = 0)
data_widedf <- as.data.frame(data_wide)
rownames(data_widedf) <- data_widedf$ID_plot
data_widedf$ID_plot <- NULL  

#merge species and environmental data
bio_data <- left_join(data_wide, dati_stazionali, by = c("ID_plot", "Management"))



#----BIODIVERSITY INDICES----

# --- Overall Shannon Index ---
data_widedf_sh <- data_widedf[,-1]
data_wide$shannon_index_tot <- diversity(data_widedf_sh, index = "shannon")
summary(data_wide$shannon_index_tot)

#calculate mean and sd for each management category
mean_shannon_by_management <- data_wide %>%
  group_by(Management) %>%
  summarise(
    mean_shannon = mean(shannon_index_tot, na.rm = TRUE),
    sd_shannon   = sd(shannon_index_tot, na.rm = TRUE),
    n_plots      = n()
  )


#calculate range for each management category 
range_shannon_by_management <- data_wide %>%
  group_by(Management) %>%
  summarise(
    min_shannon = min(shannon_index_tot, na.rm = TRUE),
    max_shannon = max(shannon_index_tot, na.rm = TRUE),
    range       = max_shannon - min_shannon
  )

# Kruskal and Dunn test
kruskal.test(shannon_index_tot ~ Management, data = data_wide)
dunn_test_shtot <- dunnTest(shannon_index_tot ~ Management, data = data_wide, method="bonferroni")
dunn_resultssh <- dunn_test_shtot$res
p_values <- dunn_resultssh$P.adj
names(p_values) <- paste(dunn_resultssh$Comparison)
letters <- multcompLetters(p_values, threshold = 0.05)

# letters for the plot
df_summary <- data_wide %>%
  group_by(Management) %>%
  summarise(media = median(shannon_index_tot)) %>%
  mutate(lettere = c("a", "a", "b"))

# Overall Shannon boxplot
plot_shannontot <- ggplot(data_wide, aes(x = Management, y = shannon_index_tot, fill = Management)) +
  geom_boxplot(size = 0.4, aes(color = Management)) +
  labs(y = "Shannon", x = "") +
  geom_text(data = df_summary, aes(x = Management, y = media + 0.8, label = lettere), size = 6, color = "black") +
  scale_fill_manual(values = c("Managed" = "yellow2",
                               "Unmanaged" = "darkseagreen2", 
                               "Woodland" = "turquoise4"),
                    labels = c("Managed" = "Regularly\nmanaged",
                               "Unmanaged" = "Occasionally\nmanaged",
                               "Woodland" = "Unmanaged")) +
  scale_color_manual(values = c("Managed" = "black", "Unmanaged" = "black", "Woodland" = "black")) +
  scale_x_discrete(labels = c("Managed" = "Regularly\nmanaged",
                              "Unmanaged" = "Occasionally\nmanaged",
                              "Woodland" = "Unmanaged")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        plot.title = element_text(size = 20, hjust = 0.5),
        legend.position = "none")



# --- Shannon by layer ---
data_wide_layer <- data_entry |> 
  pivot_wider(names_from = c(Species), values_from = Abundance, values_fill=0)
data_wide_layer$shannon_index <- apply(data_wide_layer[, 4:298], 1, function(x) diversity(x, index = "shannon"))
data_wide_layer$Management <- trimws(data_wide_layer$Management)


####Herb layer
#create species abundance matrix per site for HERBACEOUS LAYER
data_herb <- data_entry[data_entry$Layer == "Herb",]
data_herbwide<- data_herb |> 
  pivot_wider(names_from = c(Species),  # use "species" for column names
              values_from = Abundance,#abundance values inserted as cell values
              values_fill=0)  # fill missing values with 0
data_herbwidedf <- as.data.frame(data_herbwide)
rownames(data_herbwidedf) <- data_herbwidedf$ID_plot #set plot ID as row names
data_herbwidedf$ID_plot <- NULL 
data_herbwidedf<- data_herbwidedf[,-c(1,2)] # remove layer and management columns

# calculate Shannon index for herbaceous layer
data_herbwide$shannon_herb <- diversity(data_herbwidedf, index = "shannon") #Shannon index added directly to the dataset
data_herbwide <- as.data.frame(data_herbwide)
rownames(data_herbwide) <- data_herbwide$ID_plot #set ID_plot as row names
data_herbwide$ID_plot <- NULL 
data_herbwide<- data_herbwide[,-1] #remove layer column

# Kruskal and Dunn test
kruskal_result_shherb <- kruskal.test(shannon_herb ~ Management, data = data_herbwide)
print(kruskal_result_shherb) #non-significant data (p-value greater than 0.05)

dunn_test_shherb <-dunnTest(shannon_herb ~ Management, data = data_herbwide, method="bonferroni")
print(dunn_test_shherb)
dunn_results <- dunn_test_shherb$res
p_values <- dunn_results$P.adj
names(p_values) <- paste(dunn_results$Comparison)
letters <- multcompLetters(p_values, threshold = 0.05)


####Shrub layer
data_shrub <- data_entry[data_entry$Layer == "Shrub",] #dataset only for shrub layer
data_shrubwide<- data_shrub |> 
  pivot_wider(names_from = c(Species),  # use "species" for column names
              values_from = Abundance,#abundance values inserted as cell values
              values_fill=0)  # fill missing values with 0
data_shrubwidedf <- as.data.frame(data_shrubwide)
rownames(data_shrubwidedf) <- data_shrubwidedf$ID_plot #per mettere l'id_plot come nomi delle righe
data_shrubwidedf$ID_plot <- NULL 
data_shrubwidedf<- data_shrubwidedf[,-c(1,2)] #remove layer and management columns --> species abundance matrix per site for SHRUB LAYER

data_shrubwide$shannon_shrub <- diversity(data_shrubwidedf, index = "shannon") #iShannon index added directly to the dataset
data_shrubwide <- as.data.frame(data_shrubwide)
rownames(data_shrubwide) <- data_shrubwide$ID_plot # set ID_plot as row names
data_shrubwide$ID_plot <- NULL 
data_shrubwide<- data_shrubwide[,-1] #remove layer column

# Kruskal and Dunn test
kruskal_result_shshrub <- kruskal.test(shannon_shrub ~ Management, data = data_shrubwide)
print(kruskal_result_shshrub) #p-value lower than 0.05 -> significant

dunn_test_shshrub <-dunnTest(shannon_shrub ~ Management, data = data_shrubwide, method= "bonferroni")
print(dunn_test_shshrub) #differences between managed and unmanaged and between unmanaged and woodland are not significant
dunn_results <- dunn_test_shshrub$res
p_values <- dunn_results$P.adj
names(p_values) <- paste(dunn_results$Comparison)
letters <- multcompLetters(p_values, threshold = 0.05)


####Tree layer
# create species abundance matrix per site for TREE LAYER
data_tree <- data_entry[data_entry$Layer == "Tree",] # dataset only for tree layer

data_treewide<- data_tree |> 
  pivot_wider(names_from = c(Species),  # use "species" for column names
              values_from = Abundance,# abundance values inserted as cell values
              values_fill=0)  # fill missing values with 0
data_treewidedf <- as.data.frame(data_treewide)
rownames(data_treewidedf) <- data_treewidedf$ID_plot # set ID_plot as row names
data_treewidedf$ID_plot <- NULL 
data_treewidedf<- data_treewidedf[,-c(1,2)] # remove layer and management columns
data_treewide$shannon_tree <- diversity(data_treewidedf, index = "shannon") # Shannon index added directly to the dataset
data_treewide <- as.data.frame(data_treewide)
rownames(data_treewide) <- data_treewide$ID_plot # set ID_plot as row names
data_treewide$ID_plot <- NULL 
data_treewide<- data_treewide[,-1] # remove layer and management columns


# Kruskal and Dunn test
kruskal_result_tree <- kruskal.test(shannon_tree ~ Management, data = data_treewide)
print(kruskal_result_tree) # significant data (p-value lower than 0.05)

dunn_test_shtree <-dunnTest(shannon_tree ~ Management, data = data_treewide, method= "bonferroni")
print(dunn_test_shtree) # difference between managed and unmanaged is not significant

dunn_results <- dunn_test_shtree$res
p_values <- dunn_results$P.adj
names(p_values) <- paste(dunn_results$Comparison)
letters <- multcompLetters(p_values, threshold = 0.05)



df_summary_layer <- data_wide_layer %>%
  group_by(Layer, Management) %>%
  summarise(media = median(shannon_index), .groups = "drop") %>%
  arrange(Layer, Management) %>%
  mutate(lettere = c("", "", "", "a", "ab", "b", "c", "c", "d"))

plot_shannontotlayer <- ggplot(data_wide_layer, aes(x = Layer, y = shannon_index, fill = Management)) +
  geom_boxplot(size = 0.4, aes(color = Management), outliers = TRUE) +
  labs(y = "Shannon", x = "") +
  geom_text(data = df_summary_layer,
            aes(x = Layer, y = media + 1.2, label = lettere, group = Management),
            size = 6, color = "black", position = position_dodge(width = 0.75)) +
  scale_fill_manual(values = c("Managed" = "yellow2",
                               "Unmanaged" = "darkseagreen2", 
                               "Woodland" = "turquoise4"),
                    labels = c("Managed" = "Regularly\nmanaged",
                               "Unmanaged" = "Occasionally\nmanaged",
                               "Woodland" = "Unmanaged"),
                    name = "") +
  scale_color_manual(values = c("Managed" = "black", "Unmanaged" = "black", "Woodland" = "black"), guide = "none") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.position = "none")




# --- Species richness totale ---
data_widedf_rich <- as.data.frame(data_entry |> 
                                    pivot_wider(names_from = c(Species,Layer), values_from = Abundance, values_fill=0))
rownames(data_widedf_rich) <- data_widedf_rich$ID_plot
data_widedf_rich$ID_plot <- NULL  
data_widedf_rich <- data_widedf_rich[, -1]
data_wide <- data_entry |> 
  pivot_wider(names_from = c(Species,Layer), values_from = Abundance, values_fill=0)
data_wide$species_richness_tot <- specnumber(data_widedf_rich)
summary(data_wide$species_richness_tot)

# calculate mean and sd for each management category
mean_species_by_management <- data_wide %>%
  group_by(Management) %>%
  summarise(
    mean_richness = mean(species_richness_tot, na.rm = TRUE),
    sd_richness   = sd(species_richness_tot, na.rm = TRUE),
    n_plots      = n()
  )

# calculate range for each management category
range_species_by_management <- data_wide %>%
  group_by(Management) %>%
  summarise(
    min_richness = min(species_richness_tot, na.rm = TRUE),
    max_richness = max(species_richness_tot, na.rm = TRUE),
    range       = max_richness - min_richness
  )


# Kruskal-Wallis and Dunn's tests
kruskal_result_tot <- kruskal.test(species_richness_tot ~ Management, data = data_wide)
print(kruskal_result_tot) # non-significant data (p-value greater than 0.05)
dunn_test_rctot <- dunnTest(species_richness_tot ~ Management, data = data_wide, method="bonferroni")
print(dunn_test_rctot)
dunn_results <- dunn_test_rctot$res
p_values <- dunn_results$P.adj
names(p_values) <- paste(dunn_results$Comparison)
letters <- multcompLetters(p_values)
print(letters$Letters)

df_summary <- data_wide %>%
  group_by(Management) %>%
  summarise(media = median(species_richness_tot)) %>%
  mutate(lettere = c("", "", ""))

plot_richnesstot <- ggplot(data_wide, aes(x = Management, y = species_richness_tot, fill = Management)) +
  geom_boxplot(size = 0.4, aes(color = Management)) +
  labs(y = "Richness", x = "") +
  geom_text(data = df_summary, aes(x = Management, y = media + 22, label = lettere), size = 6, color = "black") +
  scale_fill_manual(values = c("Managed" = "yellow2",
                               "Unmanaged" = "darkseagreen2", 
                               "Woodland" = "turquoise4"),
                    labels = c("Managed" = "Regularly\nmanaged",
                               "Unmanaged" = "Occasionally\nmanaged",
                               "Woodland" = "Unmanaged")) +
  scale_color_manual(values = c("Managed" = "black", "Unmanaged" = "black", "Woodland" = "black")) +
  scale_x_discrete(labels = c("Managed" = "Regularly\nmanaged",
                              "Unmanaged" = "Occasionally\nmanaged",
                              "Woodland" = "Unmanaged")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        plot.title = element_text(size = 20, hjust = 0.5),
        legend.position = "none")



# --- Species richness by layer ---
data_wide_layer$species_richness <- apply(data_wide_layer[, 4:298], 1, specnumber)

####Herb layer
data_herbwide$sp_rich_herb <- specnumber(data_herbwidedf)

#Kruskal and Dunn test
kruskal_result_sprichherb <- kruskal.test(sp_rich_herb ~ Management, data = data_herbwide)
print(kruskal_result_sprichherb) # significant data (p-value lower than 0.05)

# Dunn test -> non-parametric test to determine which groups are significantly different
dunn_test_sprichherb <-dunnTest(sp_rich_herb ~ Management, data = data_herbwide, method= "bonferroni")
print(dunn_test_sprichherb) # difference between managed and unmanaged and between managed and woodland is not significant


dunn_results <- dunn_test_sprichherb$res
p_values <- dunn_results$P.adj
names(p_values) <- paste(dunn_results$Comparison)
letters <- multcompLetters(p_values, threshold = 0.05)
print(letters$Letters)

####Shrub layer
data_shrubwide$sp_rich_shrub <- specnumber(data_shrubwidedf)

#Kruskal and Dunn test
kruskal_result_shrubrich <- kruskal.test(sp_rich_shrub ~ Management, data = data_shrubwide)
print(kruskal_result_shrubrich) #significant data (p-value lower than 0.05)

dunn_test_sprichshrub <-dunnTest(sp_rich_shrub ~ Management, data = data_shrubwide, method= "bonferroni")
print(dunn_test_sprichshrub) # difference between unmanaged and woodland is not significant

dunn_results <- dunn_test_sprichshrub$res
p_values <- dunn_results$P.adj
names(p_values) <- paste(dunn_results$Comparison)
letters <- multcompLetters(p_values)
print(letters$Letters) 


####Tree layer
data_treewide$sp_rich_tree <- specnumber(data_treewidedf)

#Kruskal e Dunn test
kruskal_result_treerich <- kruskal.test(sp_rich_tree ~ Management, data = data_treewide)
print(kruskal_result_treerich) # significant data (p-value lower than 0.05)

dunn_test_treerich <-dunnTest(sp_rich_tree ~ Management, data = data_treewide, method= "bonferroni")
print(dunn_test_treerich) # difference between managed and unmanaged is not significant

dunn_results <- dunn_test_treerich$res
p_values <- dunn_results$P.adj
names(p_values) <- paste(dunn_results$Comparison)
letters <- multcompLetters(p_values)
print(letters$Letters)


df_summary_layer <- data_wide_layer %>%
  group_by(Layer, Management) %>%
  summarise(media = median(species_richness), .groups = "drop") %>%
  arrange(Layer, Management) %>%
  mutate(lettere = c("ab", "a", "b", "c", "d", "d", "e", "e", "f"))

plot_speciesrichnesslayer <- ggplot(data_wide_layer, aes(x = Layer, y = species_richness, fill = Management)) +
  geom_boxplot(size = 0.4, aes(color = Management), outliers = TRUE) +
  labs(y = "Richness", x = "") +
  geom_text(data = df_summary_layer,
            aes(x = Layer, y = media + 22, label = lettere, group = Management),
            size = 6, color = "black", position = position_dodge(width = 0.75)) +
  scale_fill_manual(values = c("Managed" = "yellow2",
                               "Unmanaged" = "darkseagreen2", 
                               "Woodland" = "turquoise4"),
                    labels = c("Managed" = "Regularly managed",
                               "Unmanaged" = "Occasionally managed",
                               "Woodland" = "Unmanaged"),
                    name = "Management Category") +
  scale_color_manual(values = c("Managed" = "black", "Unmanaged" = "black", "Woodland" = "black"), guide = "none") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.position = "none")

#### ---- Shannon Equitability index ----
data_wide <- data_entry |> 
  pivot_wider(names_from = c(Species, Layer),
              values_from = Abundance,
              values_fill = 0)
data_widedf <- as.data.frame(data_wide)
rownames(data_widedf) <- data_widedf$ID_plot
data_widedf$ID_plot <- NULL
data_widedf <- data_widedf[, -1]

data_wide$eH <- diversity(data_widedf, index = "shannon") / log(specnumber(data_widedf_rich))
str(data_wide$eH)
data_wide$eH <- as.numeric(data_wide$eH)
# calculate mean, sd and range for each management category
mean_equitability_by_management <- data_wide %>%
  group_by(Management) %>%
  summarise(
    mean_shannon = mean(eH, na.rm = TRUE),
    sd_shannon   = sd(eH, na.rm = TRUE),
    n_plots      = n(),
    min_eH = min(eH, na.rm = TRUE),
    max_eH = max(eH, na.rm = TRUE),
    range       = max_eH - min_eH
  )

# Kruskal and Dunn test
kruskal.test(eH ~ Management, data = data_wide)#significant
dunn_test_eH <- dunnTest(eH ~ Management, data = data_wide, method="bonferroni")
print(dunn_test_eH)
dunn_resultseH <- dunn_test_eH$res
p_values <- dunn_resultseH$P.adj
names(p_values) <- paste(dunn_resultseH$Comparison)
letters <- multcompLetters(p_values, threshold = 0.05)

# letters for the plot
df_summary <- data_wide %>%
  group_by(Management) %>%
  summarise(media = median(eH)) %>%
  mutate(lettere = c("a", "ab", "b"))

# Overall Shannon equitability boxplot
plot_eH <- ggplot(data_wide, aes(x = Management, y =eH, fill = Management)) +
  geom_boxplot(size = 0.4, aes(color = Management)) +
  labs(y = "eH", x = "") +
  geom_text(data = df_summary, aes(x = Management, y = media + 0.25, label = lettere), size = 6, color = "black") +
  scale_fill_manual(values = c("Managed" = "yellow2",
                               "Unmanaged" = "darkseagreen2", 
                               "Woodland" = "turquoise4"),
                    labels = c("Managed" = "Regularly\nmanaged",
                               "Unmanaged" = "Occasionally\nmanaged",
                               "Woodland" = "Unmanaged")) +
  scale_color_manual(values = c("Managed" = "black", "Unmanaged" = "black", "Woodland" = "black")) +
  scale_x_discrete(labels = c("Managed" = "Regularly\nmanaged",
                              "Unmanaged" = "Occasionally\nmanaged",
                              "Woodland" = "Unmanaged")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        plot.title = element_text(size = 20, hjust = 0.5),
        legend.position = "none")

# --- Shannon equitability by layer ---
data_wide_layer$eH <- ifelse(
  data_wide_layer$species_richness > 1,
  data_wide_layer$shannon_index / log(data_wide_layer$species_richness),
  0
)
data_wide_layer$Management <- trimws(data_wide_layer$Management)


####Herb layer
# create species abundance matrix per site for HERBACEOUS LAYER
data_herb <- data_entry[data_entry$Layer == "Herb",] #dataset solo di strato erbaceo
data_herbwide<- data_herb |> 
  pivot_wider(names_from = c(Species),  # use "species" for column names
              values_from = Abundance,# abundance values inserted as cell values
              values_fill=0)  # fill missing values with 0
data_herbwidedf <- as.data.frame(data_herbwide)
rownames(data_herbwidedf) <- data_herbwidedf$ID_plot # set ID_plot as row names
data_herbwidedf$ID_plot <- NULL 
data_herbwidedf<- data_herbwidedf[,-c(1,2)] # remove layer and management columns

# calculate eH index for herbaceous layer
data_herbwide$eH_herb <- diversity(data_herbwidedf, index = "shannon")/log(specnumber(data_herbwidedf)) 
data_herbwide <- as.data.frame(data_herbwide)
rownames(data_herbwide) <- data_herbwide$ID_plot # set ID_plot as row names
data_herbwide$ID_plot <- NULL 
data_herbwide<- data_herbwide[,-1] # remove layer column

# Kruskal and Dunn test
kruskal_result_eHherb <- kruskal.test(eH_herb ~ Management, data = data_herbwide)
print(kruskal_result_eHherb) # non-significant data (p-value greater than 0.05)

dunn_test_eHherb <-dunnTest(eH_herb ~ Management, data = data_herbwide, method="bonferroni")
print(dunn_test_eHherb)
dunn_results <- dunn_test_eHherb$res
p_values <- dunn_results$P.adj
names(p_values) <- paste(dunn_results$Comparison)
letters <- multcompLetters(p_values, threshold = 0.05)# a a a


####Shrub layer
data_shrub <- data_entry[data_entry$Layer == "Shrub",] # dataset only for shrub layer
data_shrubwide<- data_shrub |> 
  pivot_wider(names_from = c(Species),  # use "species" for column names
              values_from = Abundance,# abundance values inserted as cell values
              values_fill=0)  # fill missing values with 0
data_shrubwidedf <- as.data.frame(data_shrubwide)
rownames(data_shrubwidedf) <- data_shrubwidedf$ID_plot # set ID_plot as row names
data_shrubwidedf$ID_plot <- NULL 
data_shrubwidedf<- data_shrubwidedf[,-c(1,2)] # remove layer and management columns --> species abundance matrix per site for SHRUB LAYER

data_shrubwide$eH_shrub <- diversity(data_shrubwidedf, index = "shannon")/log(specnumber(data_shrubwidedf)) 
data_shrubwide <- as.data.frame(data_shrubwide)
rownames(data_shrubwide) <- data_shrubwide$ID_plot #per mettere l'id_plot come nomi delle righe
data_shrubwide$ID_plot <- NULL 
data_shrubwide<- data_shrubwide[,-1] # remove layer column

# Kruskal and Dunn test
kruskal_result_eHshrub <- kruskal.test(eH_shrub ~ Management, data = data_shrubwide)
print(kruskal_result_eHshrub) # p-value lower than 0.05 -> significant

dunn_test_eHshrub <-dunnTest(eH_shrub ~ Management, data = data_shrubwide, method= "bonferroni")
print(dunn_test_eHshrub) # differences between managed and unmanaged and between unmanaged and woodland are not significant
dunn_results <- dunn_test_eHshrub$res
p_values <- dunn_results$P.adj
names(p_values) <- paste(dunn_results$Comparison)
letters <- multcompLetters(p_values, threshold = 0.05) # a b ab


####Tree layer
# create species abundance matrix per site for TREE LAYER
data_tree <- data_entry[data_entry$Layer == "Tree",] # dataset only for tree layer
data_treewide<- data_tree |> 
  pivot_wider(names_from = c(Species), # use "species" for column names
              values_from = Abundance,# abundance values inserted as cell values
              values_fill=0)  # fill missing values with 0
data_treewidedf <- as.data.frame(data_treewide)
rownames(data_treewidedf) <- data_treewidedf$ID_plot #per mettere l'id_plot come nomi delle righe
data_treewidedf$ID_plot <- NULL 
data_treewidedf<- data_treewidedf[,-c(1,2)] # remove layer and management columns 

data_treewide$eH_tree <- diversity(data_treewidedf, index = "shannon")/log(specnumber(data_treewidedf)) # Shannon index added directly to the dataset
data_treewide <- as.data.frame(data_treewide)
rownames(data_treewide) <- data_treewide$ID_plot #per mettere l'id_plot come nomi delle righe
data_treewide$ID_plot <- NULL 
data_treewide<- data_treewide[,-1] # remove layer and management columns

# Kruskal e Dunn test
kruskal_result_tree <- kruskal.test(shannon_tree ~ Management, data = data_treewide)
print(kruskal_result_tree) # significant data (p-value lower than 0.05)

dunn_test_eHtree <-dunnTest(eH_tree ~ Management, data = data_treewide, method= "bonferroni")
print(dunn_test_eHtree) # difference between managed and unmanaged is not significant
dunn_results <- dunn_test_eHtree$res
p_values <- dunn_results$P.adj
names(p_values) <- paste(dunn_results$Comparison)
letters <- multcompLetters(p_values, threshold = 0.05) # a ab b



df_summary_layer <- data_wide_layer %>%
  group_by(Layer, Management) %>%
  summarise(media = median(eH), .groups = "drop") %>%
  arrange(Layer, Management) %>%
  mutate(lettere = c("", "", "", "a", "b", "ab", "c", "cd", "d"))

plot_eHlayer <- ggplot(data_wide_layer, aes(x = Layer, y = eH, fill = Management)) +
  geom_boxplot(size = 0.4, aes(color = Management), outliers = TRUE) +
  labs(y = "eH", x = "") +
  geom_text(data = df_summary_layer,
            aes(x = Layer, y = media + 0.9, label = lettere, group = Management),
            size = 6, color = "black", position = position_dodge(width = 0.75)) +
  scale_fill_manual(values = c("Managed" = "yellow2",
                               "Unmanaged" = "darkseagreen2", 
                               "Woodland" = "turquoise4"),
                    labels = c("Managed" = "Regularly\nmanaged",
                               "Unmanaged" = "Occasionally\nmanaged",
                               "Woodland" = "Unmanaged"),
                    name = "") +
  scale_color_manual(values = c("Managed" = "black", "Unmanaged" = "black", "Woodland" = "black"), guide = "none") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.position = "none")






# ------------------------- PANEL (A + B) -------------------------

library(ggpubr)

# 1️⃣ ---Panel A: overall values (without legend)) ---
panel_A <- ggarrange(
  plot_richnesstot,
  plot_shannontot,
  ncol = 2,
  nrow = 1,
  labels = c("a)", ""),
  font.label = list(size = 20, face = "plain"),
  hjust = -0.5
)

# 2️⃣ ---Panel B: by layer (with common legend) ---
# recreate the two plots with and without legend
p_rich_legend <- plot_speciesrichnesslayer +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 18),
    legend.box = "horizontal"
  ) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))

p_shannon_noleg <- plot_shannontotlayer + theme(legend.position = "none")

panel_B <- ggarrange(
  p_rich_legend,
  p_shannon_noleg,
  ncol = 2,
  nrow = 1,
  common.legend = TRUE,
  legend = "bottom",
  labels = c("b)", ""),
  font.label = list(size = 20, face = "plain"),
  hjust = -0.5
)

# 3️⃣ --- Final combination (A above, B below) ---
panel_final <- ggarrange(
  panel_A,
  panel_B,
  ncol = 1,
  nrow = 2,
  heights = c(1, 1.15)) # slightly more space for the second row because of the legend

# Display
print(panel_final)



# Save
#png("Plot/Diversity_indices.png", width = 4200, height = 3200, res = 300)
#print(panel_final)
#dev.off()


# ------------------------- PANEL (A + B) WITH EQUITABILITY-------------------------

library(ggpubr)

# 1️⃣ ---Panel A: overall values (without legend)) ---
panel_A <- ggarrange(
  plot_richnesstot,
  plot_eH,
  ncol = 2,
  nrow = 1,
  labels = c("a)", ""),
  font.label = list(size = 20, face = "plain"),
  hjust = -0.5
)

# 2️⃣ ---Panel B: by layer (with common legend)) ---
# recreate the two plots with and without legend
p_rich_legend <- plot_speciesrichnesslayer +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 18),
    legend.box = "horizontal"
  ) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))

p_eH_noleg <- plot_eHlayer + theme(legend.position = "none")

panel_B <- ggarrange(
  p_rich_legend,
  p_eH_noleg,
  ncol = 2,
  nrow = 1,
  common.legend = TRUE,
  legend = "bottom",
  labels = c("b)", ""),
  font.label = list(size = 20, face = "plain"),
  hjust = -0.5
)

# 3️⃣ ---Final combination (A above, B below)) ---
panel_final <- ggarrange(
  panel_A,
  panel_B,
  ncol = 1,
  nrow = 2,
  heights = c(1, 1.15) # slightly more space for the second row because of the legend

# Display
print(panel_final)



# Save
#png("Plot/Richness_eH_indices.png", width = 4200, height = 3200, res = 300)
#print(panel_final)
#dev.off()
