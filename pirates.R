rm(list = ls()); gc(); gc()

options(bitmapType='cairo')
options(scipen = 999)

library(dplyr)
library(purrr)
library(tidyr)
library(readr)
library(stringr)
library(ggplot2)
library(rvest)

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
    Years.active = parse_character(Years.active),
    Life = parse_character(Life), 
    # god, I hate windows... R on Win will chocke on ‒ and – characters, so replace them with ASCII
    # http://unicode-search.net/unicode-namesearch.pl?term=dash
    # U+2012 to U+2015 are dashes
    Life = stringr::str_replace_all(Life, "(\u2012|\u2013)", "-"),
    Years.active = stringr::str_replace_all(Years.active, "(\u2012|\u2013)", "-")
  ) %>% 
  # keep columns we want and re-order
  select(Name, Life, Country.of.origin, Years.active, Comments)

#write.csv(pirates.raw, file = "pirates.csv", row.names = FALSE)

## functions used for cleanup
f.keep.last.four.digits <- function(x) {
  gsub("[[:alpha:][:blank:][:punct:]]", "", x) %>% 
    str_sub(-4, -1)
}


f.activity.from.life <- function(b, d, return.b = TRUE) {
  # four different NA-permutations:
  # we assume that if
  #   - we only know birth, piracy was active at birth + 25 years
  #   - we only know death, piracy was active at death
  #   - we know both birth&death, piracy was active at his middle age: (b+d)/2
  #
  # we return the corresponding value, and make sure everything is integer()
  case_when(
    is.na(b)&is.na(d) ~ as.integer(NA),
    is.na(b)          ~ if(return.b){as.integer(NA)} else {d},
    is.na(d)          ~ if(return.b){as.integer(b+25L)} else {as.integer(NA)},
    TRUE              ~ if(return.b){as.integer((b+d)/2)} else {as.integer(NA)}
  )
}


## generate our pirating times dataset
pirates.dta <- pirates.raw %>% 
  mutate(
    # born and died forced into the "yyyy-yyyy" schedule for later tidyr::separate()
    Life.parsed = gsub("(b.|born) *([[:digit:]]+)", "\\2-", x = Life),
    Life.parsed = gsub("(d.|died) *([[:digit:]]+)", "-\\2", x = Life.parsed),
    Years.active = gsub("(to) *([[:digit:]]+)", "-\\2", x = Years.active),
    # move the "flourished" dates to its own column
    flourished = if_else(grepl("fl.", Life.parsed), gsub("fl. *", "", Life.parsed), as.character(NA)),
    # remove "flourished" dates from life
    Life.parsed = if_else(grepl("fl.", Life.parsed), as.character(NA), Life.parsed)
  ) %>% 
  # split the life, active, and flourished years into 2 columns (start/stop), each
  separate(Life.parsed, c("born", "died"), sep = "-", extra = "merge", fill = "right") %>% 
  separate(Years.active, c("active.start", "active.stop"), sep = "-", extra = "merge", fill = "right", remove = FALSE) %>% 
  separate(flourished, c("fl.start", "fl.stop"), sep = "-", extra = "merge", fill = "right", remove = FALSE) %>% 
  # 
  mutate_at(
    vars(born, died, active.start, active.stop, fl.start, fl.stop), 
    f.keep.last.four.digits
  ) %>% 
  mutate_at(
    vars(born, died, active.start, active.stop, fl.start, fl.stop),
    #as.integer
    parse_integer
  ) %>%
  # get the best guess of piracing time: active > flourished > estimated from life
  mutate(
    # take active if available, if not take flourished
    piracing.start = if_else(is.na(active.start), fl.start, active.start),
    piracing.stop = if_else(is.na(active.stop), fl.stop, active.stop),
    # neither active, nor flourished date, estimate from life
    piracing.start = if_else(is.na(piracing.start)&is.na(piracing.stop), f.activity.from.life(b = born, d = died), piracing.start),
    piracing.stop = if_else(is.na(piracing.start)&is.na(piracing.stop), f.activity.from.life(b = born, d = died, return.b = FALSE), piracing.stop),
    # finally, let's get single-year start/stops cleaned up
    piracing.start = if_else(is.na(piracing.start), piracing.stop, piracing.start), 
    piracing.stop = if_else(is.na(piracing.stop), piracing.start, piracing.stop)
  ) %>% 
  # reorder
  select(Name, Life, piracing.start, piracing.stop, Country.of.origin, everything()) 


# golden age of piracy
pirates.dta %>% 
  filter(piracing.start > 1000) %>% 
  ggplot()+
  geom_density(aes(x=piracing.start), colour = "red")+
  geom_density(aes(x=piracing.stop), colour = "blue")+
  geom_text(aes(x = 1300, y = 0.002, label = "yarrr! \u2620"), size = 12)

# piracing tenure
pirates.dta %>% 
  filter(piracing.start > 1000) %>% 
  ggplot()+
  geom_density(aes(x=piracing.stop-piracing.start), colour = "green")



# http://qi.com/infocloud/pirates
# 17th century: barbary pirates, nothing on them here?
# Robert Newton, the actor who played Long John Silver in the first sound production of Treasure Island
# https://www.theguardian.com/theguardian/2010/mar/10/pirates-notes-and-queries
# life expectancy after birth ~50: https://ourworldindata.org/life-expectancy/


# pirates by country
pirates.raw %>% 
  group_by(Country.of.origin) %>% 
  tally(sort = TRUE)

tt <- pirates.dta %>% 
  mutate(
    arrr = piracing.stop - piracing.start
  ) %>% hist()

density(tt$arrr, na.rm = TRUE)




# skull+crossbones
# U+2620
# https://emojipedia.org/emoji/%E2%98%A0/
# "\u2620"









