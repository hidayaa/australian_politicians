#### Preamble ####
# Purpose: The purpose of this script is to get all of the IDs for NSW politicians.
# The html files were downloaded from: https://www.parliament.nsw.gov.au/members/formermembers/pages/former-members-index.aspx
# We now need to go through each of them and grab the link to the person.
# Author: Rohan Alexander
# Email: rohan.alexander@utoronto.ca
# Last updated: 8 June 2020
# Prerequisites:
# Misc:


#### Set up workspace ####
library(rvest)
library(tidyverse)


#### Write the function ####
get_links <- function(file){
  files_html <- read_html(file)
  
  files_data <- 
    tibble(
      name = files_html %>% 
        html_nodes("td:nth-child(1)") %>% 
        html_text() %>% 
        str_squish(),
      page = files_html %>% 
        html_nodes("td:nth-child(1)") %>%
        html_nodes("a") %>% 
        html_attr('href'),
      dob = files_html %>% 
        html_nodes("td:nth-child(2)") %>% 
        html_text(),
      status = files_html %>% 
        html_nodes("td:nth-child(3)") %>% 
        html_text(),
      gender = files_html %>% 
        html_nodes("td:nth-child(4)") %>% 
        html_text()
    )
  
  print(file)
  return(files_data)
  
}


#### Use the function ####
all_letters <- list.files(path = "inputs/data/new_south_wales/lists", 
                          pattern = "*.htm",
                          full.names = TRUE)

all_former_politicians <- purrr::map_dfr(all_letters, get_links)


#### Combine everything and save ####
write_csv(all_former_politicians, "outputs/data/new_south_wales/list_of_all_former_politicians.csv")


rm(all_former_politicians, files_html, all_letters, files, get_links, file)
