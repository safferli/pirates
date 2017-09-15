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

# pirates by country
pirates.raw %>% 
  group_by(Country.of.origin) %>% 
  tally(sort = TRUE)

top.n.countries <- function(n = 10) {
  pirates.dta %>% 
  group_by(Country.of.origin) %>% 
  tally(sort = TRUE) %>% 
  head(n) %>% 
  .$Country.of.origin
}

pirates.by.country <- pirates.dta %>% 
  mutate(
    piracing.start = piracing.start %/%10L*10L,
    piracing.stop = piracing.stop %/%10L*10L,
    country = if_else(Country.of.origin %in% top.n.countries(10), Country.of.origin, "other")
  ) %>% 
  group_by(piracing.start, country) %>% 
  tally() 


# hack some country colours, based off this scheme: http://colorbrewer2.org/?type=qualitative&scheme=Paired&n=11
my.colours <- c('#a6cee3', # China
                '#33a02c', # Colonial America
                '#e31a1c', # England
                '#1f78b4', # France
                '#fb9a99', # Germany
                '#fdbf6f', # Netherlands
                '#b2df8a', # other
                '#cab2d6', # Spain
                '#6a3d9a', # United States
                '#ffff99', # Unknown
                '#ff7f00') # Venezuela


pirates.by.country %>% 
  filter(piracing.start > 1000) %>% 
  ggplot(aes(x=piracing.start, y=n))+
  geom_bar(aes(fill=country, colour=country), stat = "identity", position = "stack")+
  # scale_fill_brewer(type = "qual", palette = 3)+
  # scale_colour_brewer(type = "qual", palette = 3)+
  scale_fill_manual(values = my.colours)+
  scale_colour_manual(values = my.colours)+
  theme_bw()+
  theme(plot.title = element_text(lineheight=.8, face="bold"))+
  labs(
    title = "Arrr!tivity of famous pirates per decade\n \u2620\u2620\u2620", 
    x = "", 
    y = "number of pirates active"
  )
# export to size that fits everything into graph, use golden ratio
ggsave(file="piracy-country-time.png", width = 30, height = 30/((1+sqrt(5))/2), units = "cm")







# skull+crossbones
# U+2620
# https://emojipedia.org/emoji/%E2%98%A0/
# "\u2620"



pirates.fit <- pirates.dta %>% 
  filter(piracing.start > 1500) %>% 
  filter(piracing.start < 1750) %>% 
  mutate(
    arrr = piracing.stop - piracing.start
  )
  
pirates.fit %>% 
  ggplot()+
  geom_density(aes(x=piracing.start), colour = "red")


#library(fitdistrplus)

fitdistrplus::descdist(pirates.fit$arrr, boot = 1000)

# negative binomial
f1 <- fitdistrplus::fitdist(pirates.fit$arrr, "nbinom")
f1
fitdistrplus::plotdist(pirates.fit$arr, "nbinom", para = list(size=f1$estimate[1], mu=f1$estimate[2]))


# 
# f2 <- fitdistrplus::fitdist(pirates.fit$piracing.start, "sn")
# f2
# fitdistrplus::plotdist(pirates.fit$piracing.start, "nbinom", para = list(size = f1$estimate[1], mu = f1$estimate[2]))

# f3 <- fGarch::snormFit(pirates.fit$arrr)
# f3

# dsnorm(1:100, mean = 9.3899, sd = 7.0945, xi = 180.3135) %>% as.data.frame()
# 
# 
# library(actuar)
# 
# my_data <- pirates.fit$arrr
# 
# fit_ll <- fitdist(my_data, "llogis", start = list(shape = 1, scale = 500), lower = c(0,0))
# fit_P  <- fitdist(my_data, "pareto", start = list(shape = 1, scale = 500))
# fit_B  <- fitdist(my_data, "burr",   start = list(shape1 = 0.3, shape2 = 1, rate = 1))
# cdfcomp(list(fit_ln, fit_ll, fit_P, fit_B), xlogscale = TRUE, ylogscale = TRUE,
#         legendtext = c("lognormal", "loglogistic", "Pareto", "Burr"), lwd=2)
# 
# 
# 
# fit_ll <- fitdist(my_data, "llogis")
# 
# fit0.start <- fitdist(my_data, "truncnorm", fix.arg=list(a=0),
#                       start = as.list(0))



## pirate name generator
# http://www.fantasynamegenerators.com/pirate-names.php

source("pirate-names.R")

f.gen.pirate.name <- function(female = FALSE) {
  # generate a pirate name from a list of 
  # - first names (male/female)
  # - nick names (male/female)
  # - last names
  if(female) {
    paste(
      base::sample(names.female, 1), 
      base::sample(names.nickf, 1), 
      base::sample(names.last, 1)
    )
  } else {
    paste(
      base::sample(names.male, 1), 
      base::sample(names.nickm, 1), 
      base::sample(names.last, 1)
    )
  }
}

f.gen.pirating.tenure <- function() {
  # generate a tenure time from the estimated theoretical neg binomial distribution (pirates.fit$arrr)
  rnbinom(1, size=f1$estimate[1], mu=f1$estimate[2])
}

f.gen.pirating.startyear <- function() {
  # generate a startyear by pulling from the empirical distribution (pirates.fit$piracing.start)
  base::sample(pirates.fit$piracing.start, 1)
}

f.pirate.story <- function(female = FALSE) {
  # build the pirate story by joining name, startyear, and tenure time
  yy <- f.gen.pirating.startyear()
  rr <- paste(
    "Your name is", 
    f.gen.pirate.name(female), 
    "and you roamed the caribbean from", 
    yy, "to", yy+f.gen.pirating.tenure()
  )
  return(rr)
}


# test the story!
f.pirate.story()
f.pirate.story(female = TRUE)




