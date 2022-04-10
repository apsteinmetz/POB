# animal names
library(tidyverse)
library(rvest)

letter_pairs <- paste(LETTERS[1:25],LETTERS[2:26],sep="-")

mammals_raw <- read_html("https://www.enchantedlearning.com/wordlist/mammal.shtml")
mammals <- mammals_raw %>%  html_element(css = "#main-content > div > div > div.wordlist-wrapper") %>%
  html_text2() %>%
  str_split("\n") %>%
  unlist() %>%
  enframe(name=NULL,value="animal") %>%
  filter(!(animal %in% LETTERS)) %>%
  filter(!(animal %in% letter_pairs))  %>%
  mutate(class = "Mammal")

birds_raw <- read_html("https://www.enchantedlearning.com/wordlist/birds.shtml")
birds <- birds_raw %>%  html_element(css = "#main-content > div > div > div.wordlist-wrapper") %>%
  html_text2() %>%
  str_split("\n") %>%
  unlist() %>%
  enframe(name=NULL,value="animal") %>%
  filter(!(animal %in% LETTERS)) %>%
  filter(!(animal %in% letter_pairs)) %>%
  mutate(class = "Bird")

fish_raw <- read_html("https://www.enchantedlearning.com/wordlist/fish.shtml")
fish <- fish_raw %>%  html_element(css = "#main-content > div > div > div.wordlist-wrapper") %>%
  html_text2() %>%
  str_split("\n") %>%
  unlist() %>%
  enframe(name=NULL,value="animal") %>%
  filter(!(animal %in% LETTERS)) %>%
  filter(!(animal %in% letter_pairs)) %>%
  mutate(class = "Fish")

reptiles_raw <- read_html("https://www.enchantedlearning.com/wordlist/reptiles.shtml")
reptiles <- reptiles_raw %>%  html_element(css = "#main-content > div > div > div.wordlist-wrapper") %>%
  html_text2() %>%
  str_split("\n") %>%
  unlist() %>%
  enframe(name=NULL,value="animal") %>%
  filter(!(animal %in% LETTERS)) %>%
  filter(!(animal %in% letter_pairs)) %>%
  mutate(class = "Reptile")

insects_raw <- read_html("https://www.enchantedlearning.com/wordlist/insect.shtml")
insects <- insects_raw %>%  html_element(css = "#main-content > div > div > div.wordlist-wrapper") %>%
  html_text2() %>%
  str_split("\n") %>%
  unlist() %>%
  enframe(name=NULL,value="animal") %>%
  filter(!(animal %in% LETTERS)) %>%
  filter(!(animal %in% letter_pairs))  %>%
  mutate(class = "Insect")

amphibians_raw <- read_html("https://www.enchantedlearning.com/wordlist/amphibian.shtml")
amphibians <- amphibians_raw %>%  html_element(css = "#main-content > div > div > div.wordlist-wrapper") %>%
  html_text2() %>%
  str_split("\n") %>%
  unlist() %>%
  enframe(name=NULL,value="animal") %>%
  filter(!(animal %in% LETTERS)) %>%
  filter(!(animal %in% letter_pairs)) %>%
  mutate(class = "Amphibian")

animals_full <- bind_rows(mammals,birds,fish,insects,reptiles,amphibians)

animals_full$class <- as_factor(animals_full$class)

save(animals_full,file = "./data/animals_full.rdata")

# now take just last word in name as the general name

animals <- animals_full %>% transmute(animal = word(animal,-1),class) %>%
  unique() %>%
  arrange(animal)

save(animals,file = "./data/animals.rdata")

