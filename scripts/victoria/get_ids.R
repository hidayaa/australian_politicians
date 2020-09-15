#### Preamble ####
# Purpose: The purpose of this script is to get all of the IDs for VIC politicians.
# The html files were downloaded from: https://www.parliament.vic.gov.au/about/people-in-parliament/re-member
# We now need to go through and grab the link to the person.
# Author: Rohan Alexander
# Email: rohan.alexander@utoronto.ca
# Last updated: 8 June 2020
# Prerequisites:
# Misc:


#### Set up workspace ####
library(rvest)
library(tidyverse)


files_html <- read_html("inputs/data/victoria/all.htm")
  

files_data <- 
  tibble(
    name = files_html %>% 
      html_nodes("#pov_remembersdb") %>% 
      html_nodes("a") %>% 
      html_text(),
    page = files_html %>% 
      html_nodes("#pov_remembersdb") %>% 
      html_nodes("a") %>% 
      html_attr('href')
    )

files_data <- 
  files_data %>% 
  separate(col = name, into = c("name", "birth"), sep = "\\(") %>% 
  separate(col = birth, into = c("born", "died"), sep = " - ") 



#### Combine everything and save ####
write_csv(files_data, "outputs/data/victoria/list_of_all_former_politicians.csv")

