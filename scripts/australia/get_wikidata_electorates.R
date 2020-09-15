
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


# Finally, need to get the electorate names

wikidata_electorates <- 
  tibble(wikidata_ID = all_data$electoral_district %>% unique())

wikidata_electorates <- 
  wikidata_electorates %>% 
  filter(!is.na(wikidata_ID))


# wikidata_ID <- "Q2973794"
# wikidata_page <- get_item(wikidata_ID)[[1]]


#### Write the function ####
get_label_for_a_electorate <- function(wikidata_ID){
  # wikidata_ID <- "Q2973794"
  wikidata_electorate_page <- get_item(wikidata_ID)[[1]]
  
  electorate_name <- if (wikidata_electorate_page$labels$en$value %>% is.null()) {
    "NA"
  } else {
    wikidata_electorate_page$labels$en$value
  }
  
  results_tibble <- tibble(position_ID = wikidata_ID,
                           electorate_name
  )
  
  file_name <- paste0("inputs/data/electorate_names/", wikidata_ID, ".csv")
  write_csv(results_tibble, file_name)
  Sys.sleep("5")
  print(wikidata_ID)
}

# Testing, testing, is this thing on?
test <- wikidata_electorates$wikidata_ID[1:10]
walk(test, get_label_for_a_electorate)

# Use function on full list
all_electorates <- wikidata_electorates$wikidata_ID
walk(all_electorates, get_label_for_a_electorate)


#### Combine everything ####
files <- list.files(path = "inputs/data/electorate_names", pattern = "csv$", full.names = TRUE) 

all_electorates <- map_dfr(files, read_csv)

write_csv(all_electorates, "outputs/data/all_wikipedia_ids_with_electorate_names.csv")

