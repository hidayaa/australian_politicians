#### Preamble ####
# Purpose: The purpose of this script is to get all of the appointments for
# each politician based on wikipedia data. This gives a starting point that can
# be compared with the list published by the Australian Parliament House.
# Author: Rohan Alexander
# Email: rohan.alexander@utoronto.ca
# Last updated: 30 October 2019
# Prerequisites:
# Misc:


#### Set up workspace ####
# install.packages("tidyverse")
library(tidyverse)
# install.packages("WikidataR")
library(WikidataR)
# devtools::install_github("RohanAlexander/AustralianPoliticians")
library(AustralianPoliticians)

# Fix a few that are obviously wrong.
# (No longer needed as fixed the package, but keep code just in case need later.)
# all$wikidataID[all$uniqueID == "Rowland1971"] <- "Q6837228"


#### Write the function ####
get_education_for_a_politician <- function(wikidata_ID){
  # wikidata_ID <- "Q43135" # Just for testing
  
  # This does the download of the wikidata information. As we only have one 
  # politician at a time we just use that one. There is a bunch of information 
  # that is available, but we just look at the claims.
  wikidata_claims <- get_item(wikidata_ID)[[1]]$claims
  
  # Need to use this if else structure because otherwise the function fails when
  # there is no value.
  place_of_education <- if (wikidata_claims$P69$mainsnak$datavalue$value %>% names() %>% is.null()) {
    "NA"
  } else if (wikidata_claims$P69$mainsnak$datavalue$value$id %>% is.null()) {
    "NA"
  } else {
    wikidata_claims$P69$mainsnak$datavalue$value$id 
  }
  
  # Use the lapply function to change NULLS to NAs otherwise the function fails.
  end_time <- if (wikidata_claims$P69$qualifiers$P582 %>% is.null()) {
    "NA"
  } else {
    wikidata_claims$P69$qualifiers$P582 %>%
      map(pluck, "datavalue", "value", "time") %>%
      lapply(function(x)
        ifelse(is.null(x), NA, x)) %>%
      unlist()
  }
  
  start_time <- if (wikidata_claims$P69$qualifiers$P580 %>% is.null()) {
    "NA"
  } else {
    wikidata_claims$P69$qualifiers$P580 %>%
      map(pluck, "datavalue", "value", "time") %>%
      lapply(function(x)
        ifelse(is.null(x), NA, x)) %>%
      unlist()
  }
  
  degree <- if (wikidata_claims$P69$qualifiers$P512 %>% is.null()) {
    "NA"
  } else {
    wikidata_claims$P69$qualifiers$P512 %>%
      map(pluck, "datavalue", "value", "id") %>%
      lapply(function(x)
        ifelse(is.null(x), NA, x)) %>%
      unlist()
  }
  
  major <- if (wikidata_claims$P69$qualifiers$P812 %>% is.null()) {
    "NA"
  } else {
    wikidata_claims$P69$qualifiers$P812 %>%
      map(pluck, "datavalue", "value", "id") %>%
      lapply(function(x)
        ifelse(is.null(x), NA, x)) %>%
      unlist()
  }
  
  
  results_tibble <- tibble(politician = wikidata_ID,
                           place_of_education,
                           start_time,
                           end_time,
                           degree,
                           major
  )
  
  file_name <- paste0("inputs/data/individual_education/", wikidata_ID, ".csv")
  write_csv(results_tibble, file_name)
  Sys.sleep("5")
  print(wikidata_ID)
  
}


#### Use the function ####
# Testing
test <- AustralianPoliticians::all$wikidataID[1:10]
walk(test, get_education_for_a_politician)

all_ids <- AustralianPoliticians::all$wikidataID
walk(all_ids, get_education_for_a_politician)

all_ids[1:100]

#### Combine everything and save ####
files <- list.files(path = "inputs/data/individual_education", pattern = "csv$", full.names = TRUE) 

all_data <- map_dfr(files, read_csv)

write_csv(all_data, "outputs/data/all_wikidata_education.csv")


