# find animal names in text
library(tidyverse)
library(tidytext)
library(wordcloud)
library(pluralize)
library(wesanderson)

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
                   "duck","jack", "being","red",
                   "paradise","boatman","roost",
                   "migration",
                   "watching","feather","devil",
                   "swallow","wing","skipper","crow",
                   "feather","feathers","coral","fowl","crest",
                   "sponge","flock","brood","bass",
                   "horse","fish","bird","flightless",
                   "bear","ornithologist","ornithology")

added_animals <- c(animal="buzzard",class="Bird") %>%
  t() %>%
  as_tibble() %>%
  mutate(class=as_factor(class))

load("./data/animals.rdata")

animals <- animals %>%
  filter(!(animal %in% false_positives))

animals <- bind_rows(added_animals,animals) %>%
  mutate(animal = singularize(animal)) %>%
  unique()

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
  rename(animal=word)
  {.}

# overall
pob_animals <- pob_animals_by_book %>%
  group_by(animal) %>%
  tally(n)

if(require(RColorBrewer)){

  pal <- brewer.pal(9,"BuGn")
  pal <- pal[-(1:4)]
}

wordcloud(pob_animals$animal,freq = pob_animals$n,colors = pal)

pob_animals_by_book %>%
  left_join(animals) %>%
  group_by(title,class) %>%
  tally(n) %>%
  ggplot(aes(title,n,fill=forcats::fct_rev(class))) + geom_col()+
  coord_flip() +
  theme_minimal() +
  # scale_fill_brewer(palette = "Set3") +
  scale_fill_viridis_d(name = "Class") +
  labs(title = "Animal Mentions",
       subtitle = "Aubrey/Maturin Series",
       x = "",
       y = "Number of Mentions")

