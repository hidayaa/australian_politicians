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
files_html <- read_html("inputs/data/western_australia/lists/all.htm")
  
files_data <- 
  tibble(
    name = files_html %>% 
      html_nodes(".tdBordB:nth-child(1)") %>% 
      html_text() %>% 
      str_squish(),
    page = files_html %>% 
      html_nodes(".tdBordB:nth-child(1)") %>%
      html_nodes("a") %>% 
      html_attr('href'),
    house = files_html %>% 
      html_nodes(".tdBordB:nth-child(2)") %>% 
      html_text(),
    elected = files_html %>% 
      html_nodes(".tdBordB:nth-child(3)") %>% 
      html_text(),
    electorate = files_html %>% 
      html_nodes(".tdBordB:nth-child(4)") %>% 
      html_text(),
    retired = files_html %>% 
      html_nodes(".tdBordB:nth-child(5)") %>% 
      html_text()
    )


#### Combine everything and save ####
write_csv(files_data, "outputs/data/western_australia/list_of_all_former_politicians.csv")


rm(all_former_politicians, files_html, all_letters, files, get_links, file)
