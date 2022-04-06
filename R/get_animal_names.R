# animal names
library(tidyverse)
library(rvest)

animals_raw <- read_html("https://www.enchantedlearning.com/wordlist/animal.shtml")
animals <- animals_raw %>%  html_element(css = "#main-content > div > div > div.wordlist-wrapper") %>%
  html_text2() %>%
  str_split("\n") %>%
  unlist() %>%
  enframe(name=NULL,value="animal") %>%
  filter(!(animal %in% LETTERS))

birds_raw <- read_html("https://www.enchantedlearning.com/wordlist/birds.shtml")
birds <- birds_raw %>%  html_element(css = "#main-content > div > div > div.wordlist-wrapper") %>%
  html_text2() %>%
  str_split("\n") %>%
  unlist() %>%
  enframe(name=NULL,value="animal") %>%
  filter(!(animal %in% LETTERS))


