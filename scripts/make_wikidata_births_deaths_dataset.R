#### Preamble ####
# Purpose: To compare birth and death date between what I originally had and 
# what wikidata has. Also, to get the birthplace from wikidata to copy it into
# the all dataset.
# Author: Rohan Alexander
# Email: rohan.alexander@utoronto.ca
# Last updated: 31 October 2019
# Prerequisites: Need to have run get_wikidata_births_deaths_and_positions.R
# Misc:
# - Follow up on todos


#### Set up workspace ####
library(lubridate)
# install.packages("tidyverse")
library(tidyverse)
# install.packages("WikidataR")
library(WikidataR)
# devtools::install_github("RohanAlexander/AustralianPoliticians")
library(AustralianPoliticians)


#### Get births/deaths from wikidata dataset ####
all_data <- read_csv("outputs/data/all_wikidata_births_deaths_positions.csv")

individuals <- all_data %>% 
  select(politician, date_of_birth, place_of_birth, date_of_death, flag)

individuals <- individuals %>% 
  distinct()

# Fix the dates
individuals <- individuals %>% 
  mutate(date_of_birth = str_remove(date_of_birth, "\\+"),
         date_of_birth = str_remove(date_of_birth, "T00:00:00Z"),
         date_of_death = str_remove(date_of_death, "\\+"),
         date_of_death = str_remove(date_of_death, "T00:00:00Z"))

individuals <- individuals %>% 
  separate(date_of_birth, into = c("year_of_birth", "debris"), sep = "-", remove = FALSE, extra = "merge") %>% 
  separate(date_of_death, into = c("year_of_death", "remove"), sep = "-", remove = FALSE, extra = "merge") %>% 
  select(-debris, -remove) %>% 
  mutate(date_of_birth = ymd(date_of_birth),
         date_of_death = ymd(date_of_death),
         year_of_death = as.integer(year_of_death),
         year_of_birth = as.integer(year_of_birth)
  )
# The parsing failures are just the ymd failing when there's only a year in the date e.g. 1980-00-00


#### Get placenames from wikidata ####
# Need to go get the place names
wikidata_places <- 
  tibble(wikidata_ID = individuals$place_of_birth %>% unique()) %>% 
  filter(!is.na(wikidata_ID))

# Write the function
get_label_for_a_place <- function(wikidata_ID){
  
  # wikidata_ID <- "Q2312709"
  wikidata_place_page <- get_item(wikidata_ID)[[1]]
  
  place_name <- if (wikidata_place_page$labels$en$value %>% is.null()) {
    "NA"
  } else {
    wikidata_place_page$labels$en$value
  }
  
  county <- if (wikidata_place_page$claims$P17$mainsnak$datavalue$value$id %>% is.null()) {
      "NA"
    } else {
      wikidata_place_page$claims$P17$mainsnak$datavalue$value$id
    }
  
  results_tibble <- tibble(place_ID = wikidata_ID,
                           place_name,
                           county
  )
  
  file_name <- paste0("inputs/data/placenames/", wikidata_ID, ".csv")
  write_csv(results_tibble, file_name)
  Sys.sleep("3")
  print(wikidata_ID)
}

# Testing
test <- wikidata_places$wikidata_ID[1:10]
walk(test, get_label_for_a_place)

# Use function on full list
all_places <- wikidata_places$wikidata_ID
walk(all_places, get_label_for_a_place)

# Combine everything ####
files <- list.files(path = "inputs/data/placenames", pattern = "csv$", full.names = TRUE) 
all_places <- map_dfr(files, read_csv)

# Oh I made a typo
all_places <- 
  all_places %>% 
  rename(country = county)

# There are some where there are many countries for the one place. This is for 
# historical reasons, but we only want one. E.g. London:
# all_places$country[all_places$place_name == "London"] %>% unique()
# TODO Need to think about what to do here.
# Just drop countries for now
all_places <- 
  all_places %>% 
  select(place_ID, place_name) %>% 
  distinct()

write_csv(all_places, "outputs/data/all_wikipedia_ids_with_place_names.csv")


#### Push into main dataset ####
# Now push the placenames into the main dataset
all_places <- all_places %>% 
  rename(place_of_birth = place_ID)

individuals <- 
  individuals %>% 
  left_join(all_places, by = "place_of_birth") %>% 
  rename(place_of_birth_wikidata_id = place_of_birth,
         place_of_birth = place_name) %>% 
  select(politician, date_of_birth, year_of_birth, place_of_birth, place_of_birth_wikidata_id, everything())

# There are two that are duplicated. e.g.:
# individuals %>% 
#   group_by(politician) %>% 
#   filter(n()>1)
# It's not a big deal. They both just have two death dates. In the case of 
# Q8019517, the second one seems wrong; in the case of Q5498991 they both seem 
# wrong and just trying to imply a year, so it doesn't really matter. We'll 
# just keep the first entry for each.
individuals <- 
  individuals %>% 
  distinct(politician, .keep_all = TRUE)

# Add wikidata to the names, so that when we push this into the all dataset, it
# is obvious where it came from
individuals <- 
  individuals %>% setNames(paste0('wikidata_', names(.)))


all_joined <- 
  AustralianPoliticians::all %>% 
  left_join(individuals, by = c("wikidataID" = "wikidata_politician"))

all_joined <- 
  all_joined %>% 
  select(uniqueID, birthDate, birthYear, deathDate, wikidata_date_of_birth, 
         wikidata_year_of_birth, wikidata_place_of_birth, 
         wikidata_place_of_birth_wikidata_id, wikidata_date_of_death, 
         wikidata_year_of_death, wikidataID, wikipedia)


#### Follow ups ####
# Now need to follow up on ones that are different to what we already have
follow_up_deaths <- 
  all_joined %>% 
  mutate(test_if_deathDate_is_same = if_else(deathDate == wikidata_date_of_death, 1, 0)) %>% 
  filter(test_if_deathDate_is_same == 0) %>% 
  select(uniqueID, deathDate, wikidata_date_of_death, wikidataID, wikipedia) %>% 
  mutate(diff = abs(deathDate - wikidata_date_of_death)) %>% 
  arrange(desc(diff))
# Just worry about the ones that are further than a year for now

follow_up_birth_year <- 
  all_joined %>% 
  mutate(test_if_birthYear_is_same = if_else(birthYear == wikidata_year_of_birth, 1, 0)) %>% 
  filter(test_if_birthYear_is_same == 0) %>% 
  select(uniqueID, birthYear, wikidata_year_of_birth, wikidataID, wikipedia) %>% 
  mutate(diff = abs(birthYear - wikidata_year_of_birth)) %>% 
  arrange(desc(diff)) %>% 
  filter(diff > 1)

follow_up_birth_year$uniqueID

for_copying_to_all <- 
  all_joined %>% 
  select(uniqueID, wikidata_place_of_birth)

write_csv(for_copying_to_all, "outputs/data/uniqueID_with_birthplaces.csv")



