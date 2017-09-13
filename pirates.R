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


# 3: Rise of the English Sea Dogs and Dutch Corsairs: 1560-1650
# 4: Age of the Buccaneers: 1650-1690
# 5: Golden Age of Piracy: 1690-1730
pirates.raw <- pirates.wiki %>% 
  # grab all data tables; they all have the class "wikitable"
  html_nodes(".wikitable") %>% 
  # extract all tables and put them into one dataframe
  purrr::map_dfr(html_table, fill = TRUE) %>% 
  # clean column names
  setNames(make.names(names(.))) %>% 
  mutate(
    # stupid wiki has capital and non-capital letters in headers...
    Years.active = if_else(is.na(Years.active), Years.Active, Years.active),
    # empty table cells should be NA
    Years.active = if_else(Years.active == "", as.character(NA), Years.active),
    Life = if_else(Life == "", as.character(NA), Life),
    # god, I hate windows... R on Win will chocke on ‒ and – characters, so replace them with ASCII
    # http://unicode-search.net/unicode-namesearch.pl?term=dash
    # U+2012 to U+2015 are dashes
    Life = gsub(pattern = "(\u2012|\u2013)", replacement = "-", x = Life),
    Years.active = gsub(pattern = "(\u2012|\u2013)", replacement = "-", x = Years.active)
  ) %>% 
  # keep columns we want and re-order
  select(Name, Life, Country.of.origin, Years.active, Comments)

#write.csv(pirates.raw, file = "pirates.csv", row.names = FALSE)

## functions used for cleanup
f.keep.only.digits <- function(x) {
  gsub("[[:alpha:][:blank:][:punct:]]", "", x)
}



pirates.dta <- pirates.raw %>% 
  mutate(
    # born and died forced into the "yyyy-yyyy" schedule for later tidyr::separate()
    Life.parsed = gsub("(b.|born) *([[:digit:]]+)", "\\2-", x = Life),
    Life.parsed = gsub("(d.|died) *([[:digit:]]+)", "-\\2", x = Life.parsed),
    # move the "flourished" dates to its own column
    flourished = if_else(grepl("fl.", Life.parsed), gsub("fl. *", "", Life.parsed), as.character(NA)),
    # remove "flourished" dates from life
    Life.parsed = if_else(grepl("fl.", Life.parsed), as.character(NA), Life.parsed)
  ) %>% 
  # split the life, active, and flourished years into 2 columns (start/stop), each
  separate(Life.parsed, c("born", "died"), sep = "-", extra = "merge") %>% 
  separate(Years.active, c("active.start", "active.stop"), sep = "-", extra = "merge", remove = FALSE) %>% 
  separate(flourished, c("fl.start", "fl.stop"), sep = "-", extra = "merge", remove = FALSE) %>% 
  # 
  mutate_at(
    vars(born, died, active.start, active.stop, fl.start, fl.stop), 
    f.keep.only.digits
  ) %>% 
  mutate_at(
    vars(born, died, active.start, active.stop, fl.start, fl.stop), 
    as.integer
  ) %>% 
  # get the best guess of piracing time
  mutate(
    piracing.start = if_else(is.na(active.start), fl.start, active.start),
    piracing.stop = 1
  ) %>% 
  # reorder
  select(Life, piracing.start, piracing.stop, everything()) %>% 
  #select(Life, born, died, Years.active, active.start, active.stop, flourished, fl.start, fl.stop, everything()) %>% 
  View()








# pirates by country
pirates.raw %>% 
  group_by(Country.of.origin) %>% 
  tally(sort = TRUE)





# skull+crossbones
# U+2620
# https://emojipedia.org/emoji/%E2%98%A0/









