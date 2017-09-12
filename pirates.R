rm(list = ls()); gc(); gc()

options(bitmapType='cairo')
options(scipen = 999)

library(ggplot2)
library(rvest)
library(dplyr)
library(purrr)
library(tidyr)

# Define your workspace: "X:/xxx/"
wd <- "c:/github/pirates/"
setwd(wd)



## retrieve wikipedia list of pirates page
#pirates.url <- "https://en.wikipedia.org/w/index.php?title=List_of_pirates&oldid=799791558"
pirates.url <- "C:/github/pirates/List of pirates - Wikipedia.htm"


## read the page into R
pirates.wiki <- read_html(pirates.url)


# 3: Rise of the English Sea Dogs and Dutch Corsairs: 1560–1650
# 4: Age of the Buccaneers: 1650–1690
# 5: Golden Age of Piracy: 1690–1730
pirates.raw <- pirates.wiki %>% 
  # grab all data tables; they all have the class "wikitable"
  html_nodes(".wikitable") %>% 
  # extract all tables and put them into one dataframe
  map_dfr(html_table, fill = TRUE) %>% 
  # clean column names
  setNames(make.names(names(.)))

pirates.raw %>% 
  group_by(Country.of.origin) %>% 
  tally(sort = TRUE)














