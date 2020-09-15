#### Preamble ####
# Purpose: The purpose of this script is to download all of the pages for Queensland politicians.
# Author: Rohan Alexander
# Email: rohan.alexander@utoronto.ca
# Last updated: 21 June 2020
# Prerequisites: Need to have the IDs from 'get_ids.R'
# Misc:


#### Set up workspace ####
library(rvest)
library(tidyverse)


#### Write the function ####
get_politicians_data <- function(page_of_interest){
  
  # For testing: 
  # page_of_interest <- "inputs/data/queensland/politicians/812306558.html"
  
  politicians_page <- read_html(page_of_interest)
  
  name <- politicians_page %>% 
    html_nodes(".left h1") %>% 
    html_text()
  
  bio <- politicians_page %>% 
    html_nodes(".member-bio") %>% 
    html_text()
  
  politician_data <- tibble(
    name = name,
    number = page_of_interest %>% str_remove("inputs/data/queensland/politicians/") %>% str_remove(".html"),
    bio = bio
  )
  
  return(politician_data)
}


#### Use the function ####
all_current_politicians <- list.files(path = "inputs/data/queensland/politicians/", 
                                     pattern = "*.html",
                                     full.names = TRUE)

numbers <- c("812306558.html", "2917049458.html", "4154112788.html", "1634313414.html", "2909040466.html", "1560887617.html", "38139274.html", "1339591927.html", 
  "270271732.html", "4264547649.html", "1753153258.html", "714866259.html", "2411213250.html", "1956503169.html", "2889146732.html", "2685143056.html", 
  "3778366749.html", "977021911.html", "752580610.html", "194565277.html", "780236409.html", "1171511689.html", "425411558.html", "3080924249.html", 
  "2980517905.html", "2540388086.html", "1115979287.html", "2704686237.html", "407905322.html", "2781841870.html", "3291126039.html", "3592056585.html", 
  "3942844115.html", "9522482.html", "3779160121.html", "895860336.html", "1649329815.html", "1749439388.html", "551359571.html", "3921191401.html", 
  "3185374871.html", "3098878649.html", "405444814.html", "1637065151.html", "84787299.html", "2975500316.html", "2681648574.html", "40491599.html", 
  "1140473162.html", "3933801190.html", "3319466242.html", "1642712805.html", "3780599856.html", "3441651661.html", "1230118149.html", "3968359730.html", 
  "4209461404.html", "1452802487.html", "1226169161.html", "3895516698.html", "1236262867.html", "566313827.html", "3589307611.html", "1311447998.html", 
  "4169613409.html", "343413384.html", "1256874142.html", "3394533373.html", "3817157555.html", "2169129767.html", "3172667384.html", "3451737636.html", 
  "3608468635.html", "2735111125.html", "3295683648.html", "292739608.html", "1630339737.html", "890183913.html", "259257624.html", "3824095407.html", 
  "915661556.html", "2877371171.html", "3280563879.html", "819683359.html", "1164255125.html", "332322051.html", "2703195545.html", "2155251911.html", 
  "2300871827.html", "1138671533.html", "2319326882.html", "93042746.html", "1217785207.html")

hands <- paste0("inputs/data/queensland/politicians/", numbers)

# For testing: 
# hands <- hands[1:2]

data_on_all_current_politicians <- purrr::map_dfr(hands, get_politicians_data)


write_csv(data_on_all_current_politicians, "outputs/data/queensland/02-raw_data_on_all_current_politicians.csv")


