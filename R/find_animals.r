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
  mutate(animal = if_else(animal %in% make_singular,
                          str_remove(animal,"s$"),
                          animal))

# singular animal
pob_animal <- animals %>%
  rename(word = animal) %>%
  inner_join(tkns,by="word") %>%
  count(word)

#pluralize
consonants <- letters[!(letters %in% c("a","e","i","o","u","y"))] %>%
  paste(collapse = "")

y_regex <- paste0("[",consonants,"]y$")

animals <- animals %>%
  mutate(plural = animal) %>%
  mutate(plural = if_else(str_detect(plural,"s$"),
                           paste0(plural,"es"),
                           paste0(plural,"s"))) %>%
  mutate(plural = if_else(str_detect(animal,y_regex),
                          str_replace(animal,"y$","ies"),
                          plural)) %>%

  {.}
animals %>% filter(str_detect(animal,y_regex))

# what words will be misspelled after removing "s"
odd_plurals <- pob_animal %>% filter(str_detect(word,"s$|y$")) %>%
  mutate(word = paste0(word,"e")) %>%
  mutate(word = str_replace(word,"ye$","ie")) %>%
  pull(word)


pob_animals <- animals %>%
  rename(word = plural) %>%
  inner_join(tkns,by="word") %>%
  count(word) %>%
  # revert plurals to singular
  mutate(word = str_remove(word,"s$")) %>%
  mutate(word = if_else(word %in% odd_plurals,
                         str_remove(word,"e$"),
                         word)) %>%
  mutate(word = str_replace(word,"i$","y")) %>%
  # combine with original singular
  bind_rows(pob_animal) %>%
  group_by(word) %>%
  tally(n)


if(require(RColorBrewer)){

  pal <- brewer.pal(9,"BuGn")
  pal <- pal[-(1:4)]
}

wordcloud(pob_animals$word,freq = pob_animals$n,colors = pal)

# how do rankings change when we include plurals?
ranks <- pob_animal %>% rename(singular = n) %>%
  full_join(pob_animals) %>%
  rename(plural = n) %>%
  mutate(across(.fns= ~replace(., is.na(.), 0))) %>%
  mutate(rank_s = rank(singular)) %>%
  mutate(rank_p = rank(plural)) %>%
  mutate(rank_diff= rank_s-rank_p)




# by books
pob_animals_by_book <- tkns %>%
  rename(animal =  word) %>%
  group_by(title) %>%
  inner_join(animals,by="animal") %>%
  count(animal) %>%
  pivot_wider(id_cols = animal,values_from = n,names_from = title) %>%
  mutate(across(.fns= ~replace(., is.na(.), 0)))



