#### Preamble ####
# Purpose: The purpose of this script is to get all of the IDs for Queensland politicians.
# The html files were downloaded from: https://www.parliament.qld.gov.au/members/former/member-register
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
  # file <- "inputs/data/queensland/A.htm" # Just for testing.
  
  files_html <- read_html(file)
  
  files_data <- 
    tibble(
    name = files_html %>% 
      html_nodes("a") %>% 
      html_text() %>% 
      str_squish(),
    page = files_html %>% 
      html_nodes("a") %>% 
      html_attr('href'),
    electorate = files_html %>% 
      html_nodes("td:nth-child(2)") %>% 
      html_text(),
    from_to = files_html %>% 
      html_nodes("td:nth-child(3)") %>% 
      html_text(),
    died = files_html %>% 
      html_nodes("td:nth-child(4)") %>% 
      html_text()
    )
  
  print(file)
  return(files_data)
  
  }


#### Use the function ####
all_letters <- list.files(path = "inputs/data/queensland/lists/", 
                          pattern = "*.htm",
                          full.names = TRUE)

all_former_politicians <- purrr::map_dfr(all_letters, get_links)


#### Combine everything and save ####
write_csv(all_former_politicians, "outputs/data/queensland/list_of_all_former_politicians.csv")


rm(all_former_politicians, files_html, all_letters, files, get_links, file)
