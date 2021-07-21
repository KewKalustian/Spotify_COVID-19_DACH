remove(list = ls(all = T)); gc(T,T,T)

################
### Packages ###
################

# Generating a neat function to load all desired packages (x).

load_packages <- function(x){
  
  # Object which contains the desired packages (x) as long as they are not already 
  # installed. 
  
  inst.packages <- x[!x %in% installed.packages()]
  
  # Generating a for-loop.
  
  for (i in inst.packages) {install.packages(i, dependencies = T)}
  
  sapply(x, require, character = T)}

# Finally, using the just generated function to load and/or install the 
# following desired packages.

desired.packages <- c(# Tidy coding paradigm
                      "tidyverse", "magrittr", 
                      # Data import
                      "readr", "rvest", "spotifyr",
                      # Date specific data wrangling
                      "lubridate")


load_packages(desired.packages) 


################
### Scraping ###
################

# Extracting the Spotify IDs from a given data.frame (e.g., "DE_Tracks")
# to retrieve the audio features.
id <- unique(DE_Tracks$track_id)

set.seed(1, sample.kind = "Rounding")
id_s <- sample(id, replace = F)

# Overcoming the obstacle that the "get_track_audio_features" function can
# only retrieve audio features for 100 tracks at once.


Sys.setenv(SPOTIFY_CLIENT_ID = "PLEASE ENTER HERE YOUR ID")

# Developer secret

Sys.setenv(SPOTIFY_CLIENT_SECRET = "PLEASE ENTER HERE YOUR CLIENT SECRET")

# Generating an access token to use Spotify’s API

token <- get_spotify_access_token(Sys.getenv("SPOTIFY_CLIENT_ID"), 
                                  Sys.getenv("SPOTIFY_CLIENT_SECRET"))

Feat_scraper <- function(x) {
  # omitting warnings
  base::options(warn =-1) 
  # assigning length of an ID vector to a proxy object   
  entire <- length(x)
  # setting seed for repo purposes
  set.seed(1, sample.kind = "Rounding")
  # assigning 100 sampled IDs to a vector to account for Spotify's limit
  v1a <- as.character(sample(x, 100, replace = F))
  # assigning a tibble with features of those 100 IDs. This tibble will be 
  # extended below.
  tib <- spotifyr::get_track_audio_features(v1a, token)
  # replacing any IDs with new ones if those IDs are already in the tibble
  if (any(x %in% tib$id) == T) {x = x[which(!x %in% tib$id)]}
  # creating a while loop on the condition that the rows of the tibble are
  # less and/or equal to the length of the entire object
  while (nrow(tib) <= entire) {
    # Setting seed for repo purposes
    set.seed(42, sample.kind = "Rounding")
    # assigning 100 sampled IDs from the new IDs from above to a base vector 
    # according to Spotify's limit as long as the object IDs are greater
    # than 100. If the remaining IDs are less than 100, these remaining IDs 
    # will be sampled.
    v1b <- as.character(sample(x, ifelse(length(x) > 100, 100, length(x)),
                               replace = F)) 
    # extending the tibble from above to create a complete tibble with all 
    # retrieved audio features of all track IDs of the object in question
    tib %<>% full_join(spotifyr::get_track_audio_features(v1b,token),
                       by = c("danceability", "energy", "key", "loudness", 
                              "mode", "speechiness", "acousticness", 
                              "instrumentalness", "liveness", 
                              "valence", "tempo", "type", "id", "uri",
                              "track_href", "analysis_url", "duration_ms",
                              "time_signature"))
    # replacing any IDs with new ones if those IDs are already in the tibble
    if (any(x %in% tib$id) == T) {x = x[which(!x %in% tib$id)]}
    # If the rows of the tibble are equal to the length of the entire object
    # in question…,
    if (nrow(tib) == entire) 
      #…break the loop.
      break
  }
  # outputting the entire tibble
  return(tib)
}


start <- Sys.time()
Feats <- Feat_scraper(id_s)
end <- Sys.time()

process <- end-start
print(process)

Feats_id <- Feats %>% 
  rename(track_id = id)

# Repeat this whole procedure for each country and adjust the object name 
# "Full_charts_DE" with "…_AT" and "…_CH" accordingly. Once the features 
# for all countries are loaded,
# they can be merged in the same way.

# Only for Germany:
Full_charts_DE <- merge(DE_Tracks, Feats_id , by= "track_id", all = T )%>% 
  group_by(date, chart_position) %>% 
  mutate(date = as.Date(as.character(date), "%m/%d/%Y")) %>% 
  arrange(date, chart_position)

write.csv(Full_charts_DE , "Full_Charts_DE.csv")

# To combine all charts, once they have been loaded/scraped, the rbind-
# funciton will do the job:
# Not run – EXAMPLE:

# DACH_complete <– rbind(Full_charts_DE, Full_charts_AT, Full_charts_CH)
# write.csv(DACH_complete, "DACH_complete.csv")


