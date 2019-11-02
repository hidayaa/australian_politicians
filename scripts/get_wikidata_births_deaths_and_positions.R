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
devtools::install_github("RohanAlexander/AustralianPoliticians")
library(AustralianPoliticians)

# Fix a few that are obviously wrong.
# (No longer needed as fixed the package, but keep code just in case need later.)
# all$wikidataID[all$uniqueID == "Rowland1971"] <- "Q6837228"


#### Write the function ####
get_positions_and_birth_details_for_a_politician <- function(wikidata_ID){
  # wikidata_ID <- "Q6245007" # Just for testing
  
  # This does the download of the wikidata information. As we only have one 
  # politician at a time we just use that one. There is a bunch of information 
  # that is available, but we just look at the claims.
  wikidata_claims <- get_item(wikidata_ID)[[1]]$claims

  # Need to use this if else structure because otherwise the function fails when
  # there is no value.
  date_of_birth_holder <- if (wikidata_claims$P569$mainsnak$datavalue$value$time %>% is.null()) {
    "NA"
  } else {
    wikidata_claims$P569$mainsnak$datavalue$value$time
  }

  # There are a few where the person has two birthdates, so this just picks the 
  # first one and we flag it later on.
  date_of_birth <- date_of_birth_holder[1]

  place_of_birth_holder <- if (wikidata_claims$P19$mainsnak$datavalue$value %>% names() %>% is.null()) {
    "NA"
  } else if (wikidata_claims$P19$mainsnak$datavalue$value$id %>% is.null()) {
    "NA"
  } else {
    wikidata_claims$P19$mainsnak$datavalue$value$id
  }

  place_of_birth <- place_of_birth_holder[1]

  flag <- if_else(length(date_of_birth_holder) > 1 | length(place_of_birth_holder) > 1, 1, 0)

  date_of_death <- if (wikidata_claims$P570$mainsnak$datavalue$value$time %>% is.null()) {
    "NA"
    } else {
      wikidata_claims$P570$mainsnak$datavalue$value$time
      }

  position <- if (wikidata_claims$P39$mainsnak$datavalue$value$id %>% is.null()) {
    "NA"
  } else {
    wikidata_claims$P39$mainsnak$datavalue$value$id
  }

  # Use the lapply function to change NULLS to NAs otherwise the function fails.
  start_date <- if (wikidata_claims$P39$qualifiers$P580 %>% is.null()) {
    "NA"
  } else {
    wikidata_claims$P39$qualifiers$P580 %>%
      map(pluck, "datavalue", "value", "time") %>%
      lapply(function(x)
        ifelse(is.null(x), NA, x)) %>%
      unlist()
  }

  end_date <- if (wikidata_claims$P39$qualifiers$P582 %>% is.null()) {
    "NA"
  } else {
    wikidata_claims$P39$qualifiers$P582 %>%
      map(pluck, "datavalue", "value", "time") %>%
      lapply(function(x)
        ifelse(is.null(x), NA, x)) %>%
      unlist()
  }

  electoral_district <- if (wikidata_claims$P39$qualifiers$P768 %>% is.null()) {
    "NA"
  } else {
    wikidata_claims$P39$qualifiers$P768 %>%
      map(pluck, "datavalue", "value", "id") %>%
      lapply(function(x)
        ifelse(is.null(x), NA, x)) %>%
      unlist()
  }

  replaced_by <- if (wikidata_claims$P39$qualifiers$P1366 %>% is.null()) {
    "NA"
  } else {
    wikidata_claims$P39$qualifiers$P1366 %>%
      map(pluck, "datavalue", "value", "id") %>%
      lapply(function(x)
        ifelse(is.null(x), NA, x)) %>%
      unlist()
  }

  replaces <- if (wikidata_claims$P39$qualifiers$P1365 %>% is.null()) {
    "NA"
  } else {
    wikidata_claims$P39$qualifiers$P1365 %>%
      map(pluck, "datavalue", "value", "id") %>%
      lapply(function(x)
        ifelse(is.null(x), NA, x)) %>%
      unlist()
  }

  series_ordinal <- if (wikidata_claims$P39$qualifiers$P1545 %>% is.null()) {
    "NA"
  } else {
    wikidata_claims$P39$qualifiers$P1545 %>%
      map(pluck, "datavalue", "value") %>%
      lapply(function(x)
        ifelse(is.null(x), NA, x)) %>%
      unlist()
  }

  results_tibble <- tibble(politician = wikidata_ID,
                           date_of_birth,
                           place_of_birth,
                           date_of_death,
                           position,
                           start_date,
                           end_date,
                           electoral_district,
                           replaced_by,
                           replaces,
                           series_ordinal,
                           flag
                           )

  file_name <- paste0("inputs/data/individuals/", wikidata_ID, ".csv")
  write_csv(results_tibble, file_name)
  Sys.sleep("5")
  print(wikidata_ID)

}


#### Use the function ####
# Testing
# test <- AustralianPoliticians::all$wikidataID[1:10]
# walk(test, get_positions_and_birth_details_for_a_politician)

all_ids <- AustralianPoliticians::all$wikidataID
walk(all_ids, get_positions_and_birth_details_for_a_politician)

# HERE

#### Combine everything and save ####
files <- list.files(path = "inputs/data/individuals", pattern = "csv$", full.names = TRUE) 

all_data <- map_dfr(files, read_csv)

write_csv(all_data, "outputs/data/all_wikidata_births_deaths_positions.csv")


