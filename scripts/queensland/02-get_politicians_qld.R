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
get_bio <- function(page_of_interest){
  
  # For testing: 
  # page_of_interest <- "inputs/data/queensland/politicians/2719334794.html"
  
  politicians_page <- read_html(address)
  
  save_name <- str_remove(address, "https://www.parliament.qld.gov.au/members/former/bio\\?id=") %>% 
    paste0("inputs/data/queensland/politicians/", ., ".html")
  
  write_html(politicians_page, file = save_name)

  message <- paste0("Done with ", address, " at ", Sys.time())
  print(message)
  Sys.sleep(sample(x = c(5:15), size = 1))
}


#### Use the function ####
all_former_politicians <- read_csv("outputs/data/queensland/list_of_all_former_politicians.csv")

# For testing: 
# all_former_politicians <- all_former_politicians[1:2,]

safely_get_bio <- purrr::safely(get_bio)

all_former_politicians <- purrr::walk(all_former_politicians$page, safely_get_bio)





#### Current ####
get_bio_current <- function(page_of_interest){
  
  # For testing: 
  # page_of_interest <- all_current_politicians$page[1]
  
  id <- str_remove(page_of_interest, "/members/current/list/MemberDetails\\?ID=")
  
  address <- paste0("https://www.parliament.qld.gov.au/members/current/list/MemberDetails?ID=", id)
  
  politicians_page <- read_html(address)
  
  save_name <- paste0("inputs/data/queensland/politicians/", id, ".html")
  
  write_html(politicians_page, file = save_name)
  
  message <- paste0("Done with ", id, " at ", Sys.time())
  print(message)
  Sys.sleep(sample(x = c(5:15), size = 1))
}


#### Use the function ####
all_current_politicians <- read_csv("outputs/data/queensland/list_of_all_current_politicians.csv")

# For testing: 
# all_current_politicians <- all_current_politicians[1:2,]

safely_get_current_bio <- purrr::safely(get_bio_current)

all_politicians <- purrr::walk(all_current_politicians$page, safely_get_current_bio)




