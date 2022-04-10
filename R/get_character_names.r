# get character names

library(tidyverse)
library(rvest)
library(googlesheets4)

pob_names_raw <- read_html("https://en.wikipedia.org/wiki/Recurring_characters_in_the_Aubrey%E2%80%93Maturin_series")


pob_names <- pob_names_raw %>%
  html_elements("b") %>%
  html_text() %>%
  enframe(name="id",value="character") %>%
  mutate(alias1 = character)

#googlesheets4::gs4_create(name="pob_names",sheets = pob_names)

pob_sheet <- gs4_get("https://docs.google.com/spreadsheets/d/1P4GBk-xfCpHVOgh-DhiQDFNYAzNx4sENpbTR3HDmejY/edit#gid=68927014")
pob_names <- read_sheet(pob_sheet)

save(pob_names,file="./data/pob_names.rdata")
