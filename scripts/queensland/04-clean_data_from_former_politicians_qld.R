#### Preamble ####
# Purpose: The purpose of this script is to mash the data into shape.
# Author: Rohan Alexander
# Email: rohan.alexander@utoronto.ca
# Last updated: 19 June 2020
# Prerequisites: Need to have the IDs from 'get_ids.R'
# Misc:
# Todos: Look for TODO: 


#### Set up workspace ####
library(rvest)
library(tidyverse)


# Read in data
data_on_all_former_politicians <- read_csv("outputs/data/queensland/02-raw_data_on_all_politicians.csv")


# Names
data_on_all_former_politicians <- 
  data_on_all_former_politicians %>% 
  arrange(name) %>% 
  mutate(name = str_squish(name)) %>% 
  separate(name, into = c("surname", "other_names"), sep = "\\,", remove = FALSE, extra = "merge")

# Number
data_on_all_former_politicians <- 
  data_on_all_former_politicians %>% 
  mutate(number = str_remove(number, "/"))

# Separate the bits and pieces
data_on_all_former_politicians <- 
  data_on_all_former_politicians %>%
  separate(personal, into = c("personal", "related_to"), sep = "Relationship to Politician: ", remove = FALSE, extra = "merge", fill = "right") %>% 
  separate(personal, into = c("personal", "religion"), sep = "Religion: ", remove = FALSE, extra = "merge", fill = "right") %>% 
  separate(personal, into = c("personal", "education"), sep = "Education: ", remove = FALSE, extra = "merge", fill = "right") %>% 
  separate(personal, into = c("personal", "family"), sep = "Family: ", remove = FALSE, extra = "merge", fill = "right") %>% 
  separate(personal, into = c("personal", "parents"), sep = "Parents: ", remove = FALSE, extra = "merge", fill = "right") %>% 
  separate(personal, into = c("personal", "died"), sep = "Death Date: ", remove = FALSE, extra = "merge", fill = "right") %>% 
  separate(personal, into = c("personal", "born"), sep = "Birth Date: ", remove = FALSE, extra = "merge", fill = "right") %>% 
  select(surname, other_names, name, number, related_to, religion, education, family, parents, born, died, everything) %>% 
  mutate_at(c("surname", "other_names", "name", "number", "related_to", "religion", "education", "family", "parents", "born", "died"),
            ~str_remove_all(., "[\r\n]")) %>% 
  mutate_at(c("surname", "other_names", "name", "number", "related_to", "religion", "education", "family", "parents", "born", "died"),
            ~str_squish(.)) 



#### Names ####
data_on_all_former_politicians <- 
  data_on_all_former_politicians %>%
  mutate(is_doctor = if_else(str_detect(other_names, "Dr "), "Dr", "")) %>% 
  mutate(is_sir = if_else(str_detect(other_names, "Sir "), "Sir", "")) %>%
  mutate(title = paste(is_doctor, is_sir, sep = " "),
         title = str_squish(title)) %>% 
  select(-is_sir, -is_doctor)

get_rid_of_these <- c("Hon", 
                      "KCMG", 
                      "OBE", "CBE", "OAM", "CMG", "BEM", "MBE", "OSJ", "KBE",
                      "AM", "AC", "AO", 
                      "Dr ", "Sir", "Mr ", "Mrs ", "Judge ", "Rev ")


data_on_all_former_politicians <- 
  data_on_all_former_politicians %>%
  mutate(other_names = str_remove_all(other_names, paste(get_rid_of_these, collapse = "|")),
         other_names = str_remove(other_names, "\\,"),
         other_names = str_squish(other_names))

data_on_all_former_politicians <- 
  data_on_all_former_politicians %>%
  mutate(other_names = str_replace(other_names, "De Burgh", "DeBurgh"),
         other_names = str_replace(other_names, "St George", "StGeorge")) %>%
  separate(col = other_names, into = c("first_name", "remove"), sep = " ", extra = "merge", remove = FALSE, fill = "right") %>% 
  select(-remove) %>% 
  separate(col = other_names, into = c("remove", "common_name"), sep = "\\(", extra = "merge", remove = FALSE, fill = "right") %>% 
  mutate(common_name = str_remove(common_name, "\\)")) %>% 
  mutate(first_name = str_replace(first_name, "DeBurgh", "De Burgh"),
         first_name = str_replace(first_name, "StGeorge", "St George")) %>% 
  rename(all_other_names = remove) %>% 
  rename(original_name_col = name) %>% 
  select(-other_names)


data_on_all_former_politicians <- 
  data_on_all_former_politicians %>%
  mutate(display_name = if_else(is.na(common_name), 
                                paste(surname, first_name, sep = ", "), 
                                paste(surname, common_name, sep = ", ")
                                )
         ) %>% 
  select(surname, first_name, all_other_names, common_name, display_name, title, everything())

names <- 
  data_on_all_former_politicians %>%
  select(first_name) %>% 
  count(first_name, sort = TRUE)

rm(data_on_all_former_politicians_test)
rm(names, get_rid_of_these)

urgh <- data_on_all_former_politicians %>%
  select(display_name) %>% 
  count(display_name, sort = TRUE)

# TODO: COME BACK AND MAKE THESE UNIQUE
rm(urgh)

data_on_all_former_politicians <- 
  data_on_all_former_politicians %>% 
  select(-original_name_col)


#### Birth ####
data_on_all_former_politicians <- 
  data_on_all_former_politicians %>% 
  separate(col = born, into = c("kind_of_date", "kind_of_place"), sep = "\\(", remove = FALSE, fill = "right") %>% 
  mutate(kind_of_place = str_remove(kind_of_place, "\\)$"))

data_on_all_former_politicians <- 
  data_on_all_former_politicians %>% 
  select(surname:title, kind_of_date, kind_of_place, everything())

data_on_all_former_politicians <- 
  data_on_all_former_politicians %>% 
  separate(col = kind_of_date, into = c("debris", "birth_year"), sep = "(?=[:digit:]{4})", remove = FALSE, fill = "right") %>% 
  separate(col = debris, into = c("day_of_birth", "month_of_birth"), sep = " ", remove = TRUE, fill = "right") %>% 
  mutate(birth_year = str_squish(birth_year))

days <- 
  data_on_all_former_politicians %>% 
  select(day_of_birth) %>% 
  count(day_of_birth, sort = TRUE)

months_is_wrongs <- c("February", "August", "March", "May", "September", "April", "December", "July", "January", "June", "October")

data_on_all_former_politicians <- 
  data_on_all_former_politicians %>% 
  mutate(month_of_birth = if_else(day_of_birth %in% months_is_wrongs, day_of_birth, month_of_birth),
         day_of_birth = if_else(day_of_birth %in% months_is_wrongs, "", day_of_birth))

months <- 
  data_on_all_former_politicians %>% 
  select(month_of_birth) %>% 
  count(month_of_birth, sort = TRUE)

years <- 
  data_on_all_former_politicians %>% 
  select(birth_year) %>% 
  count(birth_year, sort = TRUE)

data_on_all_former_politicians <- 
  data_on_all_former_politicians %>% 
  mutate(birth_date = paste(birth_year, month_of_birth, day_of_birth, paste = " "),
         birth_date = lubridate::ymd(birth_date))

rm(days, months, years, months_is_wrongs)

data_on_all_former_politicians <- 
  data_on_all_former_politicians %>% 
  select(surname:title, birth_date, birth_year, everything()) %>% 
  select(-kind_of_date, -day_of_birth, -month_of_birth)

#TODO: these don't have a birthdate
data_on_all_former_politicians %>% 
  filter(is.na(kind_of_date))

# Deal with place of birth
data_on_all_former_politicians$kind_of_place %>% head()

# Regex from https://stackoverflow.com/questions/32119963/r-split-string-at-last-whitespace-chars-using-tidyrseparate
data_on_all_former_politicians <- 
  data_on_all_former_politicians %>% 
  mutate(kind_of_place = str_replace(kind_of_place, "New Zealand", "NewZealand"),
         kind_of_place = str_replace(kind_of_place, "Scotland, United Kingdom", "Scotland"),
         kind_of_place = str_replace(kind_of_place, "New South Wales", "NewSouthWales"),
         kind_of_place = str_replace(kind_of_place, "South Australia", "SouthAustralia"),
         kind_of_place = str_replace(kind_of_place, "England, United Kingdom", "England"),
         kind_of_place = str_replace(kind_of_place, "South Africa", "SouthAfrica"),
         kind_of_place = str_replace(kind_of_place, "United States of America", "USA"),
         kind_of_place = str_replace(kind_of_place, "Great Britain", "GreatBritain"),
         kind_of_place = str_replace(kind_of_place, "British West Indies", "BritishWestIndies"),
         kind_of_place = str_replace(kind_of_place, "Channel Islands", "ChannelIslands"),
         kind_of_place = str_replace(kind_of_place, "The Netherlands", "TheNetherlands"),
         kind_of_place = str_replace(kind_of_place, "United Kingdom", "UnitedKingdom"),
         kind_of_place = str_replace(kind_of_place, "British Guiana", "BritishGuiana"),
         kind_of_place = str_replace(kind_of_place, "Papua New Guinea", "PapuaNewGuinea"),
         kind_of_place = str_replace(kind_of_place, "East Africa", "EastAfrica"),
         ) %>% 
  separate(col = kind_of_place, into = c("birth_city_state", "birth_country"), 
           sep = "\\s+(?=\\S*$)", # Looks for white-space that is followed by any number of non-white-space, but works from the end of the string.
           remove = FALSE,
           fill = "left") %>% 
  mutate(birth_country = str_replace(birth_country, "NewZealand", "New Zealand"),
         birth_country = str_replace(birth_country, "SouthAfrica", "South Africa"),
         birth_country = str_replace(birth_country, "NewSouthWales", "New South Wales"),
         birth_country = str_replace(birth_country, "SouthAustralia", "South Australia"),
         birth_country = str_replace(birth_country, "TheNetherlands", "The Netherlands"),
         birth_country = str_replace(birth_country, "UnitedKingdom", "United Kingdom"),
         birth_country = str_replace(birth_country, "GreatBritain", "Great Britain"),
         birth_country = str_replace(birth_country, "BritishWestIndies", "British West Indies"),
         birth_country = str_replace(birth_country, "ChannelIslands", "Channel Islands"),
         birth_country = str_replace(birth_country, "BritishGuiana", "British Guiana"),
         birth_country = str_replace(birth_country, "PapuaNewGuinea", "Papua New Guinea"),
         birth_country = str_replace(birth_country, "EastAfrica", "East	Africa"),
  ) %>% 
  mutate(birth_city_state = str_replace(birth_city_state, "NewSouthWales", "New South Wales"),
         birth_city_state = str_replace(birth_city_state, "SouthAustralia", "South Australia")
  ) %>% 
  mutate(birth_city_state = if_else(birth_country %in% c("South Australia", "New South Wales", "Brisbane", "Ipswich", "Sydney", "NSW"), birth_country, birth_city_state),
         birth_country = if_else(birth_country %in% c("South Australia", "New South Wales", "Brisbane", "Ipswich", "Sydney", "NSW"), "Australia", birth_country)
  ) %>% 
  mutate(birth_city_state = str_remove(birth_city_state, ",$"),
         birth_city_state = str_replace(birth_city_state, "Ashby-de-a-Zouch", "Ashby-de-la-Zouch"),
         birth_city_state = str_replace(birth_city_state, "QLD", "Queensland"),
         birth_city_state = str_replace(birth_city_state, "NSW", "New South Wales"),
         birth_city_state = str_replace(birth_city_state, "Newtown Cook's River, New South Wales", "Newtown, Cook's River, New South Wales"),
         birth_city_state = str_replace(birth_city_state, "Brisbane , Queensland", "Brisbane, Queensland"),
         birth_city_state = str_replace(birth_city_state, "Sydney , New South Wales", "Sydney, New South Wales")
         )

# Check countries
countries <- 
  data_on_all_former_politicians %>% 
  select(birth_country) %>% 
  count(birth_country, sort = TRUE)

rm(countries)

# Check towns
towns <- 
  data_on_all_former_politicians %>% 
  select(birth_city_state) %>% 
  count(birth_city_state, sort = TRUE)

data_on_all_former_politicians %>% 
  filter(is.na(birth_country)) %>% 
  select(birth_city_state) %>% 
  distinct()

data_on_all_former_politicians <- 
  data_on_all_former_politicians %>% 
  select(-kind_of_place)

rm(towns)


#### Death ####
data_on_all_former_politicians <- 
  data_on_all_former_politicians %>% 
  separate(col = died, into = c("kind_of_date", "kind_of_place"), sep = "\\(", remove = FALSE, fill = "right") %>% 
  mutate(kind_of_place = str_remove(kind_of_place, "\\)$"))

data_on_all_former_politicians <- 
  data_on_all_former_politicians %>% 
  select(surname:birth_country, kind_of_date, kind_of_place, everything())

data_on_all_former_politicians <- 
  data_on_all_former_politicians %>% 
  mutate(kind_of_date = str_squish(kind_of_date)) %>% 
  separate(col = kind_of_date, into = c("debris", "death_year"), sep = "(?=[:digit:]{4})", remove = FALSE, fill = "right") %>% 
  mutate(debris = str_squish(debris)) %>% 
  separate(col = debris, into = c("day_of_death", "month_of_death"), sep = " ", remove = TRUE, fill = "right") %>% 
  mutate(death_year = str_squish(death_year))

days <- 
  data_on_all_former_politicians %>% 
  select(day_of_death) %>% 
  count(day_of_death, sort = TRUE)

months_is_wrongs <- c("February")

data_on_all_former_politicians <- 
  data_on_all_former_politicians %>% 
  mutate(month_of_death = if_else(day_of_death %in% months_is_wrongs, day_of_death, month_of_death),
         day_of_death = if_else(day_of_death %in% months_is_wrongs, "", day_of_death))

months <- 
  data_on_all_former_politicians %>% 
  select(month_of_death) %>% 
  count(month_of_death, sort = TRUE)

years <- 
  data_on_all_former_politicians %>% 
  select(death_year) %>% 
  count(death_year, sort = TRUE)

data_on_all_former_politicians <- 
  data_on_all_former_politicians %>% 
  mutate(death_date = paste(death_year, month_of_death, day_of_death, paste = " "),
         death_date = lubridate::ymd(death_date)) %>% 
  select(surname:death_year, death_date, everything()) %>% 
  select(-kind_of_date, -day_of_death, -month_of_death)

rm(days, months, years, months_is_wrongs)

# Deal with place of birth
data_on_all_former_politicians$kind_of_place %>% head()

# Regex from https://stackoverflow.com/questions/32119963/r-split-string-at-last-whitespace-chars-using-tidyrseparate
data_on_all_former_politicians <- 
  data_on_all_former_politicians %>% 
  mutate(kind_of_place = str_replace(kind_of_place, "New Zealand", "NewZealand"),
         kind_of_place = str_replace(kind_of_place, "Scotland, United Kingdom", "Scotland"),
         kind_of_place = str_replace(kind_of_place, "New South Wales", "NewSouthWales"),
         kind_of_place = str_replace(kind_of_place, "South Australia", "SouthAustralia"),
         kind_of_place = str_replace(kind_of_place, "England, United Kingdom", "England"),
         kind_of_place = str_replace(kind_of_place, "South Africa", "SouthAfrica"),
         kind_of_place = str_replace(kind_of_place, "United States of America", "USA"),
         kind_of_place = str_replace(kind_of_place, "Great Britain", "GreatBritain"),
         kind_of_place = str_replace(kind_of_place, "British West Indies", "BritishWestIndies"),
         kind_of_place = str_replace(kind_of_place, "Channel Islands", "ChannelIslands"),
         kind_of_place = str_replace(kind_of_place, "The Netherlands", "TheNetherlands"),
         kind_of_place = str_replace(kind_of_place, "United Kingdom", "UnitedKingdom"),
         kind_of_place = str_replace(kind_of_place, "British Guiana", "BritishGuiana"),
         kind_of_place = str_replace(kind_of_place, "Netherland East Indies", "NetherlandEastIndies"),
         kind_of_place = str_replace(kind_of_place, "Isle of Man", "IsleofMan"),
         kind_of_place = str_replace(kind_of_place, "Papua New Guinea", "PapuaNewGuinea"),
         kind_of_place = str_replace(kind_of_place, "East Africa", "EastAfrica"),
  ) %>% 
  separate(col = kind_of_place, into = c("death_city_state", "death_country"), 
           sep = "\\s+(?=\\S*$)", # Looks for white-space that is followed by any number of non-white-space, but works from the end of the string.
           remove = FALSE,
           fill = "left") %>% 
  mutate(death_country = str_replace(death_country, "NewZealand", "New Zealand"),
         death_country = str_replace(death_country, "SouthAfrica", "South Africa"),
         death_country = str_replace(death_country, "NewSouthWales", "New South Wales"),
         death_country = str_replace(death_country, "SouthAustralia", "South Australia"),
         death_country = str_replace(death_country, "TheNetherlands", "The Netherlands"),
         death_country = str_replace(death_country, "UnitedKingdom", "United Kingdom"),
         death_country = str_replace(death_country, "GreatBritain", "Great Britain"),
         death_country = str_replace(death_country, "BritishWestIndies", "British West Indies"),
         death_country = str_replace(death_country, "ChannelIslands", "Channel Islands"),
         death_country = str_replace(death_country, "BritishGuiana", "British Guiana"),
         death_country = str_replace(death_country, "PapuaNewGuinea", "Papua New Guinea"),
         death_country = str_replace(death_country, "EastAfrica", "East	Africa"),
         death_country = str_replace(death_country, "IsleofMan", "Isle of Man"),
         death_country = str_replace(death_country, "Ausralia", "Australia"),
         death_country = str_replace(death_country, "NetherlandEastIndies", "Netherland East Indies"),
         death_country = str_replace(death_country, "Bokhara", "At sea"),
         death_country = str_replace(death_country, "Sea", "At sea"),
  ) %>% 
  mutate(death_city_state = str_replace(death_city_state, "NewSouthWales", "New South Wales"),
         death_city_state = str_replace(death_city_state, "SouthAustralia", "South Australia")
  ) %>% 
  mutate(death_city_state = if_else(death_country %in% c("South Australia", "New South Wales", "Brisbane", "Ipswich", "Sydney", "NSW", "Queensland", "Victoria"), death_country, death_city_state),
         death_country = if_else(death_country %in% c("South Australia", "New South Wales", "Brisbane", "Ipswich", "Sydney", "NSW", "Queensland", "Victoria"), "Australia", death_country)
  ) %>% 
  mutate(death_city_state = str_remove(death_city_state, ",$"),
         death_city_state = str_replace(death_city_state, "Ashby-de-a-Zouch", "Ashby-de-la-Zouch"),
         death_city_state = str_replace(death_city_state, "QLD", "Queensland"),
         death_city_state = str_replace(death_city_state, "NSW", "New South Wales"),
         death_city_state = str_replace(death_city_state, "Newtown Cook's River, New South Wales", "Newtown, Cook's River, New South Wales"),
         death_city_state = str_replace(death_city_state, "Brisbane , Queensland", "Brisbane, Queensland"),
         death_city_state = str_replace(death_city_state, "Sydney , New South Wales", "Sydney, New South Wales"),
         death_city_state = str_replace(death_city_state, "At sea on S.S.", "At sea on SS Bokhara"),
         death_city_state = str_replace(death_city_state, "SS Rotorua, Tasman", "At sea on SS Rotorua, Tasman Sea"),
         death_city_state = str_replace(death_city_state, "`", "'"),
  )

# Check countries
countries <- 
  data_on_all_former_politicians %>% 
  select(death_country) %>% 
  count(death_country, sort = TRUE)

rm(countries)

# Check towns
towns <- 
  data_on_all_former_politicians %>% 
  select(death_city_state) %>% 
  count(death_city_state, sort = TRUE)

data_on_all_former_politicians %>% 
  filter(is.na(birth_country)) %>% 
  select(birth_city_state) %>% 
  distinct()

data_on_all_former_politicians <- 
  data_on_all_former_politicians %>% 
  select(-kind_of_place)


data_on_all_former_politicians$everything %>% head()



#

read_csv("outputs/data/queensland/01-list_of_all_former_politicians.csv")
read_csv("outputs/data/queensland/02-raw_data_on_all_politicians.csv")




