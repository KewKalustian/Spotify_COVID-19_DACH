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
  "readr", "rvest",
  # Date specific data wrangling
  "lubridate")


load_packages(desired.packages) 

###############################################
### Data Acquisition for each DACH country  ###
###############################################


# First, we have to store the permanent Spotify-link.

# Inserting the respective Link (i.e., for "de", "at", and "ch")

url <- "https://spotifycharts.com/regional/de/daily/"  

# Here we specify the entire streaming period (i.e., the sequence of n days). 

streaming_period <- seq(as.Date("2020/03/11"), as.Date("2020/06/14"),
                        by = "day")

# Next, we write a generic function that combines or, respectively, concatenates 
# the URLs (for the entire period) by taking the permanent link from above and a 
# blank argument (x) as another argument to which the URL should refer 
# (i.e., our streaming_period).

gathering_urls <- function(x){paste0(url, x)}

# Using the just created function to apply it on the streaming period to finally
# get those n URLs.

all_urls  <- gathering_urls(streaming_period)

# Everything looks fine thus far. Hence, we create now a function that fills
# the desired column-names with the information we are going to retrieve from 
# those n URLs (i.e., chart position, song/track title, artist, stream counts, 
# dates, track_id).

spotifyR_scrapeR <- function(x) {page <- x

# Retrieving the 200 chart positions of each day.
chart_position <- page %>% 
  read_html() %>% 
  html_nodes(".chart-table-position") %>% 
  html_text()

#Retrieving the 200 song/track titles of each day.

title <- page %>% 
  read_html() %>% 
  html_nodes("strong") %>% 
  html_text()

# Retrieving the 200 artist names of each day.

artist <- page %>% 
  read_html() %>% 
  html_nodes(".chart-table-track span") %>% 
  html_text()

# Retrieving the 200 stream counts of each day.

stream_count <- page %>% 
  read_html() %>% 
  html_nodes("td.chart-table-streams") %>% 
  html_text()

# Retrieving the dates of for each day of the period. 

date <- page %>% 
  read_html() %>% 
  html_nodes(".responsive-select~ .responsive-select+ 
                    .responsive-select .responsive-select-value") %>%
  html_text()

# Retrieving the track_id of for each day of the period. 

track_id <-  page %>% 
  read_html() %>% 
  html_nodes("a") %>%       
  html_attr("href") %>% 
  str_remove("https://open.spotify.com/track/") %>% 
  .[c(7:206)]

# Putting these chunks together in a table of the class. 

tab <- data.frame(chart_position, title, artist, stream_count, date, track_id)

return(tab)}

# As the amount of data that should be retrieved is not that small, we can 
# expect that this process will take some time. To know how long this process
# will last, we calculate the difference between the process initialization and 
# its end.

init_time <- Sys.time()

# The actual process of web scraping: Applying the spotifyR_scrapeR-function
# to the object of that definitive URLs for each list element. That is, the just 
# created spotifyR_scrapeR-function retrieves from each URL the desired 
# information.

DE_Tracks <- map_df(all_urls, spotifyR_scrapeR) 

# End time of the retrieving-process.

end_time <- Sys.time()

# Difference (i.e., processing time to retrieve the desired information).

process_time <- end_time - init_time
print(process_time)

# Exporting and saving the retrieved datatable as .csv-file.
write_csv(DE_Tracks, "DE_Tracks.csv")
