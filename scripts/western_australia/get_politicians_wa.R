#### Preamble ####
# Purpose: The purpose of this script is to download all of the pages for Wesnter Australian politicians.
# Author: Rohan Alexander
# Email: rohan.alexander@utoronto.ca
# Last updated: 9 June 2020
# Prerequisites: Need to have the IDs from 'get_ids.R'
# Misc:


#### Set up workspace ####
library(rvest)
library(tidyverse)


#### Write the function ####
get_bio <- function(address){
  
  # address <- all_former_politicians$page[1]
  
  politicians_page <- read_html(address)
  
  save_name <- 
    str_remove(address, "https://www.parliament.wa.gov.au/parliament/library/MPHistoricalData.nsf/\\(Lookup\\)/") %>% 
    str_remove("#") %>% 
    str_remove("\\?OpenDocumentcurrent") %>% 
    paste0("inputs/data/western_australia/politicians/", ., ".html")
  
  write_html(politicians_page, file = save_name)
  
  message <- paste0("Done with ", address, " at ", Sys.time())
  print(message)
  Sys.sleep(sample(x = c(5:15), size = 1))
}


#### Use the function ####
all_former_politicians <- read_csv("outputs/data/western_australia/list_of_all_former_politicians.csv")


# For testing: 
all_former_politicians <- all_former_politicians[1:2,]

safely_get_bio <- purrr::safely(get_bio)

all_former_politicians <- purrr::walk(all_former_politicians$page, safely_get_bio)


