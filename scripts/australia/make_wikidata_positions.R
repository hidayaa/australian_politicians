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


#### Read in data ####
all_data <- read_csv("outputs/data/all_wikidata_births_deaths_positions.csv")
all_positions <- read_csv("outputs/data/all_wikipedia_ids_with_position_names.csv")
all_electorates <- read_csv("outputs/data/all_wikipedia_ids_with_electorate_names.csv")

# Filter to just positions
positions <- all_data %>% 
  select(politician, position, start_date, end_date, replaced_by, replaces, series_ordinal, flag)

# Fix the dates
positions <- positions %>% 
  mutate(start_date = str_remove(start_date, "\\+"),
         start_date = str_remove(start_date, "T00:00:00Z"),
         end_date = str_remove(end_date, "\\+"),
         end_date = str_remove(end_date, "T00:00:00Z"))

positions <- positions %>% 
  separate(start_date, into = c("year_of_start", "debris"), sep = "-", remove = FALSE, extra = "merge") %>%
  separate(end_date, into = c("year_of_end", "remove"), sep = "-", remove = FALSE, extra = "merge") %>%
  select(-debris, -remove) %>%
  mutate(start_date  = ymd(start_date),
         end_date = ymd(end_date),
         year_of_end = as.integer(year_of_end),
         year_of_start = as.integer(year_of_start)
  )
# The parsing failures are just the ymd failing when there's only a year in the date e.g. 1980-00-00 (there's a few where there's a year and a month, but that's been thrown away.)


#### Finally, push the labels into the content. ####
positions <- 
  positions %>% 
  left_join(all_positions, by = c("position" = "position_ID")) %>% 
  select(politician, position, position_name, everything())

positions$position_name %>% unique %>% table()

counts <- positions %>% 
  count(position_name) %>% 
  arrange(desc(n))

write_csv(counts, "counts.csv")
  
possible <- read_csv("inputs/possible_minister_positions.csv")

positions <- 
  positions %>% 
  filter(position_name %in% possible$Position)

all <- AustralianPoliticians::all %>% 
  select(uniqueID, wikidataID)

positions <-  
  positions %>% 
  left_join(all, by = c("politician" = "wikidataID")) %>% 
  select(uniqueID, position_name, position, start_date, end_date) %>% 
  rename(positionWikidataID = position) %>% 
  arrange(uniqueID, desc(start_date))

write_csv(positions, "outputs/positions.csv")

