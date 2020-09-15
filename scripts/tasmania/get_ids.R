#### Preamble ####
# Purpose: The purpose of this script is to get all of the IDs for Tasmanian politicians.
# The html files were downloaded from: https://www.parliament.tas.gov.au/history/tasparl/a.htm
# We now need to go through each of them and grab the link to the person.
# Author: Rohan Alexander
# Email: rohan.alexander@utoronto.ca
# Last updated: 9 June 2020
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
        html_nodes("tr:nth-child(3) a") %>% 
        html_text() %>% 
        str_squish(),
      page = files_html %>% 
        html_nodes("tr:nth-child(3) a") %>%
        html_attr('href')
    )

  print(file)
  return(files_data)
  
}


#### Use the function ####
all_letters <- list.files(path = "inputs/data/tasmania/lists", 
                          pattern = "*.htm",
                          full.names = TRUE)

all_former_politicians <- purrr::map_dfr(all_letters, get_links)


#### Combine everything and save ####
write_csv(all_former_politicians, "outputs/data/tasmania/list_of_all_former_politicians.csv")


rm(all_former_politicians, files_html, all_letters, files, get_links, file)
