#### Preamble ####
# Purpose: The purpose of this script is to mash the data into shape.
# Author: Rohan Alexander
# Email: rohan.alexander@utoronto.ca
# Last updated: 21 June 2020
# Prerequisites: Need to have the IDs from 'get_ids.R'
# Misc:
# Todos: Look for TODO: 


#### Set up workspace ####
library(rvest)
library(tidyverse)


# Read in data
data_on_all_current_politicians <- read_csv("outputs/data/queensland/02-raw_data_on_all_current_politicians.csv")

data_on_all_current_politicians <- 
  data_on_all_current_politicians %>% 
  separate_rows(bio, sep = " \r\n") %>% 
  mutate(bio = str_squish(bio)) %>% 
  distinct()

data_on_all_current_politicians <- 
  data_on_all_current_politicians %>% 
  mutate(birth = str_detect(bio, "^Birth Date:") %>% lag(),
         education = str_detect(bio, "^Education:") %>% lag(),
         religion = str_detect(bio, "^Religion:") %>% lag(),
         family = str_detect(bio, "^Family: ") %>% lag(),
         electorate = str_detect(bio, "^Electorate "),
         electorate = if_else(str_detect(bio, "^Electorate Office"), FALSE, electorate),
         party = str_detect(bio, "^Party "),
         party = if_else(str_detect(bio, "^Party Positions:"), FALSE, party),
         date_of_election = str_detect(bio, "^Date of Election ")
         )

data_on_all_current_politicians <- 
  data_on_all_current_politicians %>% 
  pivot_longer(cols = birth:date_of_election, names_to = "names", values_to = "values") %>% 
  filter(values == TRUE) %>% 
  pivot_wider(names_from = "names", values_from = "bio") %>% 
  mutate(
         electorate = str_remove(electorate, "Electorate "),
         electorate = str_remove(electorate, "- View Map \\(\\.pdf\\)"),
         electorate = str_squish(electorate),
         party = str_remove(party, "Party "),
         date_of_election = str_remove(date_of_election, "Date of Election ")
         ) %>% 
  select(-values)


#### Names ####

data_on_all_current_politicians <- 
  data_on_all_current_politicians %>%
  mutate(title = if_else(str_detect(name, "Dr "), "Dr", ""))

get_rid_of_these <- c("Hon", 
                      "Dr",
                      "Mrs", 
                      "Mr",
                      "Ms")

data_on_all_current_politicians <- 
  data_on_all_current_politicians %>%
  mutate(name = str_remove_all(name, paste(get_rid_of_these, collapse = "|")),
         name = str_remove(name, "\\,"),
         name = str_squish(name))



data_on_all_current_politicians <- 
  data_on_all_current_politicians %>%
  mutate(name = str_replace(name, "de Brenni", "deBrenni")) %>%
  separate(col = name, into = c("first_name", "remove"), sep = " ", extra = "merge", remove = FALSE, fill = "right") %>% 
  select(-remove) %>% 
  separate(col = name, into = c("remove", "surname"), sep = "\\s+(?=\\S*$)", extra = "merge", remove = FALSE, fill = "left") %>% 
  select(-remove) %>% 
  mutate(common_name = str_extract(name, "\\(\\S+\\)"), 
         common_name = str_remove(common_name, "\\)"),
         common_name = str_remove(common_name, "\\(")) %>% 
  mutate(name = str_replace(name, "deBrenni", "de Brenni")) %>% 
  rename(original_name_col = name)


data_on_all_current_politicians <- 
  data_on_all_current_politicians %>%
  mutate(display_name = if_else(is.na(common_name), 
                                paste(surname, first_name, sep = ", "), 
                                paste(surname, common_name, sep = ", ")
                                )
         ) %>% 
  select(surname, first_name, common_name, display_name, title, everything())

data_on_all_current_politicians <- 
  data_on_all_current_politicians %>% 
  select(-original_name_col)



#### Birth ####
# Separate the bits and pieces
data_on_all_current_politicians <- 
  data_on_all_current_politicians %>%
  separate(col = birth, into = c("birth_date", "birth_place"), sep = "\\(", remove = FALSE, fill = "right") %>% 
  mutate(birth_place = str_remove(birth_place, "\\)"),
         birth_date = str_squish(birth_date)) 

data_on_all_current_politicians <- 
  data_on_all_current_politicians %>% 
  separate(col = birth_date, into = c("debris", "birth_year"), sep = "(?=[:digit:]{4})", remove = FALSE, fill = "right") %>% 
  mutate(debris = str_squish(debris)) %>% 
  separate(col = debris, into = c("day_of_birth", "month_of_birth"), sep = " ", remove = TRUE, fill = "right") %>% 
  mutate(birth_year = str_squish(birth_year))

days <- 
  data_on_all_current_politicians %>% 
  select(day_of_birth) %>% 
  count(day_of_birth, sort = TRUE)

months <- 
  data_on_all_current_politicians %>% 
  select(month_of_birth) %>% 
  count(month_of_birth, sort = TRUE)

years <- 
  data_on_all_current_politicians %>% 
  select(birth_year) %>% 
  count(birth_year, sort = TRUE)

data_on_all_current_politicians <- 
  data_on_all_current_politicians %>% 
  mutate(birth_date = paste(birth_year, month_of_birth, day_of_birth, paste = " "),
         birth_date = lubridate::ymd(birth_date))

rm(days, months, years)

data_on_all_current_politicians <- 
  data_on_all_current_politicians %>% 
  select(surname:title, birth_date, birth_year, everything()) %>% 
  select(-day_of_birth, -month_of_birth, -birth)

#TODO: these don't have a birthdate
data_on_all_current_politicians %>% 
  filter(is.na(birth_year))


# Deal with place of birth
data_on_all_current_politicians$birth_place

# Regex from https://stackoverflow.com/questions/32119963/r-split-string-at-last-whitespace-chars-using-tidyrseparate
data_on_all_current_politicians <- 
  data_on_all_current_politicians %>% 
  mutate(birth_place = str_replace(birth_place, "The Netherlands", "TheNetherlands"),
         birth_place = str_replace(birth_place, "New South Wales", "NewSouthWales"),
         birth_place = str_replace(birth_place, "New Zealand", "NewZealand"),
         ) %>% 
  separate(col = birth_place, into = c("birth_city_state", "birth_country"), 
           sep = "\\s+(?=\\S*$)", # Looks for white-space that is followed by any number of non-white-space, but works from the end of the string.
           remove = FALSE,
           fill = "left") %>% 
  mutate(birth_country = str_replace(birth_country, "NewZealand", "New Zealand"),
         birth_country = str_replace(birth_country, "TheNetherlands", "The Netherlands"),
         birth_country = str_replace(birth_country, "NewSouthWales", "New South Wales"),
  ) %>% 
  mutate(birth_city_state = str_replace(birth_city_state, "NewSouthWales", "New South Wales"),
         birth_city_state = str_replace(birth_city_state, "SouthAustralia", "South Australia")
  ) %>% 
  mutate(birth_city_state = if_else(birth_country %in% c("Queensland", "New South Wales", "Brisbane", "Victoria", "Townsville", "Armidale", "Mackay", "Melbourne", "Miles", "NSW", "Toowoomba"), birth_country, birth_city_state),
         birth_country = if_else(birth_country %in% c("Queensland", "New South Wales", "Brisbane", "Victoria", "Townsville", "Armidale", "Mackay", "Melbourne", "Miles", "NSW", "Toowoomba"), "Australia", birth_country)
  ) %>% 
  mutate(birth_city_state = str_replace(birth_city_state, "QLD", "Queensland"),
         birth_city_state = str_replace(birth_city_state, "NSW", "New South Wales"),
         birth_city_state = str_remove(birth_city_state, "\\,$"),
         )

# Check countries
data_on_all_current_politicians %>% 
  select(birth_country) %>% 
  count(birth_country, sort = TRUE)

# Check towns
towns <- 
  data_on_all_current_politicians %>% 
  select(birth_city_state) %>% 
  count(birth_city_state, sort = TRUE)
rm(towns)

# Check for consistency
data_on_all_current_politicians %>% 
  filter(is.na(birth_country)) %>% 
  select(birth_city_state) %>% 
  distinct()

data_on_all_current_politicians <- 
  data_on_all_current_politicians %>% 
  select(-birth_place)

#### Postscript ####

write_csv(data_on_all_current_politicians, "outputs/data/queensland/03-data_on_all_current_politicians.csv")


