# calc character network
library(tidyverse)
library(tidytext)

# if any two characters names appear within this proximity,
# they are deemed to be associated
WORD_PROXIMITY = 200


load(file="./data/pob_names.rdata")
load("./data/pob_books.rdata")


# look at term frequency
# see: https://www.tidytextmining.com/tfidf.html

book_words <- pob_books %>%
  group_by(title) %>%
  unnest_tokens(word,text) %>%
  count(title,word,sort=TRUE) %>%
  #strip possessive
  mutate(word = str_remove(word,"'s"))


total_words <- book_words %>%
  group_by(title) %>%
  summarize(total = sum(n))

book_words <- left_join(book_words, total_words)

ggplot(book_words, aes(n/total, fill = title)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~title, ncol = 2, scales = "free_y")

freq_by_rank <- book_words %>%
  group_by(title) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total) %>%
  ungroup()

# zipf's law
freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color = title)) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10()

book_tf_idf <- book_words %>%
  bind_tf_idf(word,title, n)

book_tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))

library(forcats)

book_tf_idf %>%
  group_by(title) %>%
  slice_max(tf_idf, n = 5) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = title)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~title, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)

temp <- book_tf_idf %>%
  group_by(title) %>%
  slice_max(tf_idf, n = 5) %>%
  select(title,word)

# change instances of first and last names to just first
# where the is ambiguity with a family member
pob_books <- pob_books %>%
  mutate(text = str_replace_all(text,"Molly Harte","Molly")) %>%
  mutate(text = str_replace_all(text,"Mrs. Oakes","Clarissa")) %>%
  mutate(text = str_replace_all(text,"Queeney","Queenie")) %>%
  mutate(text = str_replace_all(text,"Clarissa Oakes","Clarissa"))

# where characters are alternately refered to by first or last
# choose one
pob_books <- pob_books %>%
  mutate(text = str_replace_all(text,"Heneage","Dundas")) %>%
  mutate(text = str_replace_all(text,"Maturin","Stephen")) %>%
  mutate(text = str_replace_all(text,"Aubrey","Jack"))


# proximity measure of character mentions
book_words <- pob_books %>%
  group_by(title) %>%
  unnest_tokens(word,text) %>%
  #strip possessive
  mutate(word = str_remove(word,"'s")) %>%
  rownames_to_column("position") %>%
  mutate(position = as.numeric(position))

#get rid of sophie in the first book since it refers to the ship
book_words <- book_words %>%
  mutate(word = if_else(word =="sophie" & title == "Master and Commander",
                        "hms_sophie",word))

character_mentions <- book_words %>%
  filter(word %in% tolower(as.vector(pob_names$id)))

character_proximity <- character_mentions %>%
  mutate(person2 = lag(word),pos_2=lag(position)) %>%
  mutate(distance = position - lag(pos_2)) %>%
  rename(person1 = word) %>%
  filter(!(person1 == person2)) %>%
  na.omit()

# character_summary <- character_proximity %>%
#   group_by(title,person1,person2) %>%
#   summarise(n = n(),avg_dist = mean(distance)) %>%
#   ungroup() %>%
#   # group_by(title) %>%
#   arrange(n)

character_summary <- character_proximity %>%
  group_by(person1,person2) %>%
  summarise(n = n(),avg_dist = mean(distance),.groups="drop") %>%
  ungroup() %>%
  # group_by(title) %>%
  arrange(desc(n))

# combine reciprocal pairs
character_summary <- character_summary %>%
  rowwise() %>%
  mutate(person12 = list(sort(c(person1,person2)))) %>%
  mutate(person1 = person12[1]) %>%
  mutate(person2 = person12[2]) %>%
  select(-person12) %>%
  group_by(person1,person2) %>%
  summarise(n = sum(n),avg_dist=mean(avg_dist),.groups = "drop")




