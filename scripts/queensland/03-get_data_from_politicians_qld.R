#### Preamble ####
# Purpose: The purpose of this script is to download all of the pages for Queensland politicians.
# Author: Rohan Alexander
# Email: rohan.alexander@utoronto.ca
# Last updated: 12 June 2020
# Prerequisites: Need to have the IDs from 'get_ids.R'
# Misc:


#### Set up workspace ####
library(rvest)
library(tidyverse)


#### Write the function ####
get_politicians_data <- function(page_of_interest){
  
  # For testing: 
  # page_of_interest <- "inputs/data/queensland/politicians/2719334794.html"
  
  politicians_page <- read_html(page_of_interest)
  
  name <- politicians_page %>% 
    html_nodes(".left h1") %>% 
    html_text()
  
  personal <- politicians_page %>% 
    html_nodes(".left:nth-child(3)") %>% 
    html_text()
  
  parliamentary <- politicians_page %>% 
    html_nodes(".BottomDottedBorder") %>% 
    html_text()
  
  everything <- politicians_page %>% 
    html_nodes(".member-bio") %>% 
    html_text()
  
  politician_data <- tibble(
    name = name,
    number = page_of_interest %>% str_remove("inputs/data/queensland/politicians/") %>% str_remove(".html"),
    personal = personal,
    everything = everything
  )
  
  return(politician_data)
}


#### Use the function ####
all_former_politicians <- list.files(path = "inputs/data/queensland/politicians/", 
                                     pattern = "*.html",
                                     full.names = TRUE)

# For testing: 
# all_former_politicians <- all_former_politicians[1:2]

data_on_all_former_politicians <- purrr::map_dfr(all_former_politicians, get_politicians_data)


write_csv(data_on_all_former_politicians, "outputs/data/queensland/02-raw_data_on_all_former_politicians.csv")
