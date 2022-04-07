# find animal names in text
library(tidyverse)
library(tidytext)

load("./data/pob_books.rdata")
load("./data/animals.rdata")


tkns <- tibble()
for (n in 1:nrow(pob_books)) {
  tkn <- pob_books[n,] %>% unnest_tokens(word, text)
  tkns <- bind_rows(tkns, tkn)
}



false_positives =c("martin","leopard","dog","fly","cat",
                   "nest","animal","egg","congregation",
                   "bird","fox","fish","rail","mole",
                   "swift","tail","bill","flight",
                   "swallow","wing","skipper","crow",
                   "feather","lobster","coral","fowl",
                   "sponge","flock","brood","bass")

animals <- animals %>%
  filter(!(animal %in% false_positives))

pob_animals <- animals %>%
  rename(word = animal) %>%
  inner_join(tkns) %>%
  count(word)
