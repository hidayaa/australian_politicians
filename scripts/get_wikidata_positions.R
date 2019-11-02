#### Preamble ####
# Purpose: ...
# Author: Rohan Alexander
# Email: rohan.alexander@utoronto.ca
# Last updated: 31 October 2019
# Prerequisites: Need to have run get_wikidata_births_deaths_and_positions.R
# Misc:


#### Set up workspace ####
library(lubridate)
# install.packages("tidyverse")
library(tidyverse)
# install.packages("WikidataR")
library(WikidataR)
# devtools::install_github("RohanAlexander/AustralianPoliticians")
library(AustralianPoliticians)


#### Get positions from wikidata dataset ####
all_data <- read_csv("outputs/data/all_wikidata_births_deaths_positions.csv")

# Need to go get the position names
wikidata_positions <- 
  tibble(wikidata_ID = all_data$position %>% unique())

wikidata_positions <- 
  wikidata_positions %>% 
  filter(!is.na(wikidata_ID))

# Write the function
get_label_for_a_position <- function(wikidata_ID){
  # wikidata_ID <- "Q2312709"
  wikidata_position_page <- get_item(wikidata_ID)[[1]]
  
  position_name <- if (wikidata_position_page$labels$en$value %>% is.null()) {
    "NA"
  } else {
    wikidata_position_page$labels$en$value
  }
  
  results_tibble <- tibble(position_ID = wikidata_ID,
                           position_name
  )
  
  file_name <- paste0("inputs/data/position_names/", wikidata_ID, ".csv")
  write_csv(results_tibble, file_name)
  Sys.sleep("5")
  print(wikidata_ID)
}

# Testing
test <- wikidata_positions$wikidata_ID[1:10]
walk(test, get_label_for_a_position)

# Use function on full list
all_positions <- wikidata_positions$wikidata_ID
walk(all_positions, get_label_for_a_position)


#### Combine everything ####
files <- list.files(path = "inputs/data/position_names", pattern = "csv$", full.names = TRUE) 

all_positions <- map_dfr(files, read_csv)

write_csv(all_positions, "outputs/data/all_wikipedia_ids_with_position_names.csv")









