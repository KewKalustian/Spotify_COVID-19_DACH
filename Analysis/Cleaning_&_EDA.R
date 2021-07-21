remove(list = ls(all = T)); gc(T,T,T)

################
### Packages ###
################

if (!require(pacman))
  install.packages("pacman", repo = "http://cran.us.r-project.org")

pacman::p_load("tidyverse", "magrittr","car", "lubridate", "scales",
               "ggpubr", "GGally","caret", "e1071", "factoextra", 
               "RColorBrewer","rgl")

pacman::p_load_gh("spotifyr")


############
### DATA ###
############


DACH_charts <-  read_csv("DACH_complete.csv", 
                         col_types = cols(X1 = col_skip(), 
                                          uri = col_skip(), 
                                          track_href = col_skip(), 
                                          analysis_url = col_skip()))
View(DACH_charts)

# Data prep.

anyNA(DACH_charts)
# Identifying NAs
apply(is.na(DACH_charts), 2, which)

DACH_charts[13503,]

DACH_charts[32735,]

# Removing NAs
DACH_charts <- na.omit(DACH_charts)

# Creating a new dataframe
# 
df_ml <- DACH_charts %>% 

  # Selecting track_id, stream_count, country, and chart_position as extrinsic 
  # features and Spotify's audio features as song-intrinsic characteristics 
  
  
  dplyr::select(c(country, date, track_id, title, artist, stream_count, 
                  chart_position, danceability, energy, loudness, acousticness,
                  instrumentalness, speechiness, liveness, valence, duration_ms,
                  tempo, mode)) %>% 
  
  # Re-scaling some features to have all song-intrinsic features on the same 
  # scale.
  
  mutate(country = as.factor(country),
         stream_count_rescaled = rescale(.$stream_count, to = c(0,1), 
                                         from = c(min(DACH_charts$stream_count), 
                                                  max(DACH_charts$stream_count))),
         
         chart_position_rescaled = rescale(.$chart_position, to = c(0,1), 
                                           from = c(max(DACH_charts$chart_position), 
                                                    min(DACH_charts$chart_position))) ,
         
         loudness_rescaled = rescale(.$loudness, to = c(0,1), 
                                     from = c(min(DACH_charts$loudness), 
                                              max(DACH_charts$loudness))),
         
         duration_ms_rescaled = rescale(.$duration_ms, to = c(0,1), 
                                        from = c(min(DACH_charts$duration_ms), 
                                                 max(DACH_charts$duration_ms))),
         
         tempo_rescaled =  tempo/1000,
         
         
         mode_fct = as.factor(mode))


str(df_ml) 

anyNA(df_ml)



df_ml$Pandemic <- ifelse(df_ml$date > as.Date("2020-03-10"), "Pandemic",
                         "No_Pandemic" )


df_ml %<>% 
  mutate( Pandemic = factor(Pandemic, labels = c( "No_Pandemic", "Pandemic"), 
                            ordered = T))



################################
### Histogram | Distribution ###
################################

# Global Plotting Layout
layout <- theme_bw(base_size = 14) +
  theme(axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        legend.position = "top",
        legend.key = element_rect(color = "white"),
        legend.background = element_rect(fill = "white",
                                         linetype = "solid",
                                         color = "grey90"),
        plot.margin = unit(c(.66, .33, .66, .66), "cm"))

# Summary stats per country
distinct_songs <- df_ml %>% 
  group_by(Pandemic, country) %>% 
  summarize(distict_songs = n_distinct(track_id))


quants <- df_ml %>% 
  group_by(Pandemic, country) %>% 
  summarize(Q1=quantile(stream_count, .25),
            Median = median(stream_count),
            Q3=quantile(stream_count, .75),
            Max=max(stream_count))


# Histograms per country

p <- ggplot(data=df_ml, aes(stream_count, fill = Pandemic))+
  
  geom_histogram(color="grey20",fill ="#2e4057", alpha = .7, 
                 binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3)),
                 show.legend = F)+
  
  geom_rect(data=quants,
            aes(x = NULL,y = NULL, xmin = Q1,  xmax=Q3,
                ymin = -Inf, ymax = Inf),alpha=0.3,fill = "darkred", 
            show.legend = F) + 
  
  geom_vline(data=quants,aes(xintercept = Median), color = "darkred", 
             linetype = "solid", size=.5) + 
  
  geom_label(data=quants,aes(x = Q1, y = 1575), fill = "white", 
             label = "Q[1]", parse = T) + 
  
  geom_label(data=quants, aes( x = Median *1.75, y = 1250), fill = "white",
             label = paste0("italic(Mdn) == ", quants$Median), parse=T)+
  
  geom_label( data=quants,aes( x = Q3, y = 1575), fill ="white", 
              label = "Q[3]", parse = T, )+
  
  geom_label(data=quants, aes( x =  Max*0.75, y = 1575), fill ="white",
             label = paste0("italic(n) == ", distinct_songs$distict_songs), 
             parse=T)+
  
  scale_y_continuous(labels = label_number_si(accuracy = NULL))+
  
  scale_x_log10(labels = label_number_si(accuracy = NULL))+
  
  labs(x = "\nStream Counts per Song\n(log-scaling with base 10)",
       y = "Frequency\n") +
  
  ggtitle(label="Per Country")+

  layout

p <- p +  facet_grid(Pandemic~country, scales = "free") 


# Summary stats across all countries
distinct_songs2 <- df_ml %>% 
  group_by(Pandemic) %>% 
  summarize(distict_songs = n_distinct(track_id))


quants2 <- df_ml %>% 
  group_by(Pandemic) %>% 
  summarize(Q1=quantile(stream_count, .25),
            Median = median(stream_count),
            mean = mean(stream_count),
            Q3=quantile(stream_count, .75),
            Max=max(stream_count))


p2 <- ggplot(data=df_ml, aes(stream_count, fill = Pandemic))+
  geom_histogram(color="grey20",fill ="#2e4057", alpha = .7, 
                 binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3)),
                 show.legend = F)+
  
  geom_rect(data=quants2,
            aes(x = NULL,y = NULL, xmin = Q1,  xmax=Q3,
                ymin = -Inf, ymax = Inf),alpha=0.3,fill = "darkred", 
            show.legend = F) + 
  
  geom_vline(data=quants2,aes(xintercept = Median), color = "darkred", 
             linetype = "solid", size=.5) + 
  
  geom_label(data=quants2,aes(x = Q1, y = 5975), fill = "white", 
             label = "Q[1]", parse = T) + 
  
  geom_label(data=quants2, aes( x = Median*1.5, y = 4500), fill = "white",
             label = paste0("italic(Mdn) == ", quants2$Median), parse=T)+
  
  geom_label( data=quants2,aes( x = Q3, y = 5975), fill ="white", 
              label = "Q[3]", parse = T )+
  
  geom_label(data=quants2, aes( x =  Max*.75, y = 5975), fill ="white",
             label = paste0("italic(n) == ", distinct_songs2$distict_songs), 
             parse=T)+
  
  scale_y_continuous(labels = label_number_si(accuracy = NULL))+
  
  scale_x_log10(labels = label_number_si(accuracy = NULL))+
  
  labs(x = "\nStream Counts per Song within DACH Countries\n(log-scaling with base 10)",
       y = "Frequency\n") +
  
  ggtitle(label="Overall")+
  
  layout

p2 <- p2 + facet_grid(Pandemic~., scales = "free") 

# Combining both plots
ggarrange(p, p2)


