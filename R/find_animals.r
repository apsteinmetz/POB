# find animal names in text
library(tidyverse)
library(tidytext)
library(wordcloud)
library(pluralize)

load("./data/pob_books.rdata")


tkns <- tibble()
for (n in 1:nrow(pob_books)) {
  tkn <- pob_books[n,] %>% unnest_tokens(word, text)
  tkns <- bind_rows(tkns, tkn)
}



false_positives =c("martin","leopard","dog","fly","cat",
                   "nest","animal","egg","congregation",
                   "fox","rail","mole","cricket",
                   "swift","tail","bill","flight",
                   "duck",
                   "swallow","wing","skipper","crow",
                   "feather","coral","fowl","crest",
                   "sponge","flock","brood","bass",
                   "horse","fish","bird","flightless",
                   "bear","ornithologist","ornithology")

added_animals <- c("buzzard")

make_singular <- c("primates","vertebrates","invertebrates","mussels",
                   "insectivores","echinoderms")



load("./data/animals.rdata")

animals <- animals %>%
  filter(!(animal %in% false_positives))

animals <- enframe(added_animals,name=NULL,value="animal") %>%
  bind_rows(animals) %>%
  mutate(animal = singularize(animal))

# by books
pob_animals_by_book <-
  animals %>%
  mutate(plural = pluralize(animal)) %>%
  pivot_longer(cols = c(animal,plural)) %>%
  select(value) %>%
  # eliminate double counting where plural = singular word, e.g. "sheep"
  unique() %>%
  rename(word=value) %>%
  inner_join(tkns,by="word") %>%
  group_by(title) %>%
  count(word) %>%
  # revert plurals to singular
  mutate(word = singularize(word)) %>%
  group_by(title,word) %>%
  tally(n) %>%
  {.}

# overall
pob_animals <- pob_animals_by_book %>%
  group_by(word) %>%
  tally(n)

if(require(RColorBrewer)){

  pal <- brewer.pal(9,"BuGn")
  pal <- pal[-(1:4)]
}

wordcloud(pob_animals$word,freq = pob_animals$n,colors = pal)

