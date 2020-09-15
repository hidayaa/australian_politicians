#### Preamble ####
# Purpose: The purpose of this script is to get all of the IDs for the current Queensland politicians.
# The html files were downloaded from: https://www.parliament.qld.gov.au/members/current/list
# We now need to go through each of them and grab the link to the person.
# Author: Rohan Alexander
# Email: rohan.alexander@utoronto.ca
# Last updated: 20 June 2020
# Prerequisites:
# Misc:


#### Set up workspace ####
library(rvest)
library(tidyverse)


#### Write the function ####
file <- "inputs/data/queensland/current.html"
  
files_html <- read_html(file)
  
files_data <- 
  tibble(
    name = files_html %>% 
      html_nodes(".right-thumbnail a") %>% 
      html_text() %>% 
      str_squish(),
    page = files_html %>% 
      html_nodes(".right-thumbnail a") %>% 
      html_attr('href'),
    electorate = files_html %>% 
      html_nodes(".electorate") %>% 
      html_text()
    )
  


#### Combine everything and save ####
write_csv(files_data, "outputs/data/queensland/list_of_all_current_politicians.csv")


rm(files_html, files_data)

