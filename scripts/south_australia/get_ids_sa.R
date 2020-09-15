#### Preamble ####
# Purpose: The purpose of this script is to get all of the IDs for South Australian politicians.
# The html files were downloaded from: https://www.parliament.sa.gov.au/en/Members/All-Former-Members
# We now need to go through each of them and grab the link to the person.
# Author: Rohan Alexander
# Email: rohan.alexander@utoronto.ca
# Last updated: 9 June 2020
# Prerequisites:
# Misc:


#### Set up workspace ####
library(rvest)
library(tidyverse)


# Get the data
files_html <- read_html("inputs/data/south_australia/lists/all.html")
  

names <- 
  files_html %>% 
  html_nodes("#ctl00_ContentPlaceHolder1_grdResults td:nth-child(1)") %>% 
  html_text() %>% 
  str_squish()

names <- names[-length(names)]

page <- 
  files_html %>% 
  html_nodes("#ctl00_ContentPlaceHolder1_grdResults td:nth-child(1)") %>% 
  html_nodes("a") %>% 
  html_attr("href")

first_name <- 
  files_html %>% 
  html_nodes("#ctl00_ContentPlaceHolder1_grdResults td:nth-child(2)") %>% 
  html_text()

title <- 
  files_html %>% 
  html_nodes("#ctl00_ContentPlaceHolder1_grdResults td:nth-child(3)") %>% 
  html_text()

electorate <- 
  files_html %>% 
  html_nodes("#ctl00_ContentPlaceHolder1_grdResults td:nth-child(4)") %>% 
  html_text()

house <- 
  files_html %>% 
  html_nodes("#ctl00_ContentPlaceHolder1_grdResults td:nth-child(5)") %>% 
  html_text()

party <- 
  files_html %>% 
  html_nodes("#ctl00_ContentPlaceHolder1_grdResults td:nth-child(6)") %>% 
  html_text()

all <- tibble(
  names = names,
  page = page,
  first_name = first_name,
  title = title,
  electorate = electorate,
  house = house,
  party = party
)

all <- 
  all %>% 
  slice(-1)

all <- 
  all %>% 
  mutate(page = paste0("https://members.parliament.sa.gov.au/", page))




#### Combine everything and save ####
write_csv(all, "outputs/data/south_australia/list_of_all_former_politicians.csv")


rm(all_former_politicians, files_html, all_letters, files, get_links, file,
   electorate, first_name, house, names, page, party, title)
