
remove(list = ls(all = T)); gc(T,T,T)

################
### Packages ###
################

if (!require(pacman))
  install.packages("pacman", repo = "http://cran.us.r-project.org")

pacman::p_load("tidyverse", "magrittr","car", "lubridate", "scales",
               "ggpubr", "GGally","caret", "e1071", "factoextra", 
               "RColorBrewer","rgl")
###############
### K-means ###
###############

### Finding K

df_ml_km_k <- df_ml %>% 
  group_by(track_id, title, mode) %>% 
  dplyr::select( danceability, energy, loudness_rescaled, valence, 
                 tempo_rescaled) %>% 
  distinct() %>% ungroup()

###

set.seed(123, sample.kind = "Rounding")

gap_stat <- cluster::clusGap(
  df_ml_km_k[-c(1:2)], FUN = kmeans, nstart = 50, iter.max = 100, 
  K.max = 6, B = 500)


set.seed(1234, sample.kind = "Rounding")
fviz_gap_stat(gap_stat,
              linecolor = "#2e4057",
              maxSE = list(method="Tibs2001SEmax", SE.factor =1))+
  labs(x = "\nNumber of Clusters" , y = "Gap Statistic (k)\n" , title = "", 
       subtitle = "") + 
  # Layout
  theme_bw(base_size = 14) +
  theme(axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(vjust = 0.5),
        legend.position = "top",
        legend.key = element_rect(color = "white"),
        legend.background = element_rect(fill = "white",
                                         linetype = "solid",
                                         color = "#bdbdbd"),
        plot.margin = unit(c(.33,.33,.66,.33), "cm"))

# Saving the output (wide format) 
ggsave("Gap_stat.png", path = "~/Desktop/R_Projects/Spotify_DACH/Plots",
       width = 12.8, height = 7.2, dpi = 320)


# 4 Clusters seem reasonable.

# k-means clustering

df_ml_ttl  <- df_ml %>% 
  group_by(track_id, title, mode) %>% 
  dplyr::select(danceability, energy, loudness_rescaled, valence, 
                tempo_rescaled) %>% ungroup()


set.seed(14, sample.kind = "Rounding")

km <- kmeans(df_ml_ttl[-c(1:2)], 4, iter.max = 100, nstart = 50,
             algorithm = "Hartigan")


print(km)


# PCA for viz

pca <- prcomp(df_ml_ttl[-c(1:2)], center = T, scale. = T)

ind_coord <- as.data.frame(get_pca_ind(pca)$coord)

# Adding clusters from the k-means algo

ind_coord$clust <- as.factor(as.numeric(km$cluster))

# Adding countries from the original data set

ind_coord$Pandemic <- as.factor(df_ml$Pandemic)

head(ind_coord)

eigenvalue <- round(get_eigenvalue(pca), 1)

variance_percent <- eigenvalue$variance.percent

print(eigenvalue)


# 3D Plotting PCA

ind_coord$clust <- as.factor(as.numeric(ind_coord$clust))

#gedanken über labels machen

Dim.1 <- ind_coord$Dim.1

Dim.2 <- ind_coord$Dim.2

Dim.3 <- ind_coord$Dim.3


temp <- data.frame(`Moderate Arousal-Potential neg Emotionality major` = '1',
                   `Higher Arousal-Potential pos Emotionality minor`  = '2', 
                   `Higher Arousal-Potential pos Emotionality major`  = '3',
                   `Moderate Arousal-Potential neg Emotionality minor`  = '4')    

# Adding the clusters to the original data set 

df_ml$mood_clust_fct <- as.factor(km$clust)


cols <- c("#3288BD", "#F46D43", "darkgreen" , "#5E4FA2")

scatter3d(x = Dim.1, y = Dim.2, z = Dim.3, 
          groups = df_ml$mood_clust_fct , surface = F, 
          grid = T, surface.col = I(cols), ellipsoid = T,  axis.scales = F,
          ellipsoid.alpha = 0.001, axis.col = c("black", "black", "black"))

text3d(x = -0.5,y =  c(.53, .63, .73, .83), z = 1, names(temp), col = I(cols))
