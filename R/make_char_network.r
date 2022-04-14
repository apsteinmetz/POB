# calc character network
library(tidyverse)
library(tidytext)
library(forcats)
library(igraph)
library(rgexf)

load(file="./data/pob_names.rdata")
load("./data/pob_books.rdata")

# ------------------------------------------------------
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


book_tf_idf %>%
  group_by(title) %>%
  slice_max(tf_idf, n = 5) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = title)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~title, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)


# ------------------------------------------------------
# CHARACTER NETWORK
# proximity measure of character mentions
pob_sheet <- gs4_get("https://docs.google.com/spreadsheets/d/1P4GBk-xfCpHVOgh-DhiQDFNYAzNx4sENpbTR3HDmejY/edit#gid=68927014")
pob_names <- read_sheet(pob_sheet)
save(pob_names,file="./data/pob_names.rdata")


load(file="./data/pob_names.rdata")
load("./data/pob_books.rdata")

#strip possessive
pob_books <- pob_books %>%
  mutate(book_num = as.integer(book_num)) %>%
  mutate(text = str_remove(text,"'s"))

# change instances of first and last names to just first
# where the is ambiguity with a family member
pob_books <- pob_books %>%
  mutate(text = str_replace_all(text,"Molly Harte","Molly")) %>%
  mutate(text = str_replace_all(text,"Mrs(\\.)?( )+Oakes","Clarissa")) %>%
  mutate(text = str_replace_all(text,"Mrs(\\.)?( )+Oakes","Clarissa")) %>%
  mutate(text = str_replace_all(text,"Lady Keith","Queenie")) %>%
  mutate(text = str_replace_all(text,"Clarissa Oakes","Clarissa"))

# normalize spelling where POB alters it across volumes
pob_books <- pob_books %>%
  mutate(text = str_replace_all(text,"Queeney","Queenie")) %>%
  mutate(text = str_replace_all(text,"Davis","Davies"))


# enforce some two-word names
pob_books <- pob_books %>%
  mutate(text = str_replace_all(text,"General Aubrey","General_Aubrey")) %>%
  mutate(text = str_replace_all(text,"Philip Aubrey","Philip_Aubrey")) %>%
  mutate(text = str_replace_all(text,"King George","King_George")) %>%
  mutate(text = str_replace_all(text,"Miss Smith","Miss_Smith")) %>%
  mutate(text = str_replace_all(text,"Amanda Smith","Miss_Smith")) %>%
  mutate(text = str_replace_all(text,"Christine Wood","Christine_Wood")) %>%
  mutate(text = str_replace_all(text,"Mrs(\\.)?( )+Fielding","Mrs_Fielding")) %>%
  mutate(text = str_replace_all(text,"Laura Fielding","Mrs_Fielding")) %>%
  mutate(text = str_replace_all(text,"Mrs(\\.)?( )+Williams","Mrs_Williams")) %>%
  mutate(text = str_replace_all(text,"Mrs(\\.)?( )+Broad","Mrs_Broad"))

# where characters are alternately refered to by first or last
# choose one
pob_books <- pob_books %>%
  mutate(text = str_replace_all(text,"Heneage Dundas","Dundas")) %>%
  mutate(text = str_replace_all(text,"Heneage","Dundas")) %>%
  mutate(text = str_replace_all(text,"Stephen Maturin","Stephen")) %>%
  mutate(text = str_replace_all(text,"Maturin","Stephen")) %>%
  mutate(text = str_replace_all(text,"Diana Villiers","Diana")) %>%
  mutate(text = str_replace_all(text,"Villiers","Diana")) %>%
  mutate(text = str_replace_all(text,"Jack Aubrey","Jack")) %>%
  mutate(text = str_replace_all(text,"Aubrey","Jack")) %>%
  mutate(text = str_replace_all(text,"General_Jack","General_Aubrey")) %>%
  # avoid lower case "jack"
  mutate(text = str_replace_all(text,"jack","mast_jack"))

#get rid of sophie in the first book since it refers to the ship
pob_books <- pob_books %>%
  mutate(text = if_else(title == "Master and Commander",
                        str_replace_all(text,"Sophie","hms_sophie"),text))




book_words <- pob_books %>%
  group_by(book_num,title) %>%
  unnest_tokens(word,text) %>%
  rownames_to_column("position") %>%
  mutate(position = as.numeric(position))

character_mentions <- book_words %>%
  filter(word %in% tolower(as.vector(pob_names$id))) %>%
  mutate(word = str_to_title(word))

characters <- character_mentions %>%
  group_by(word) %>%
  count(word,name="mentions") %>%
  rename(person = word)

PROXIMITY_LIMIT = 200

character_proximity <- character_mentions %>%
  mutate(person2 = lag(word),pos_2=lag(position)) %>%
  mutate(distance = position - lag(pos_2)) %>%
  filter(distance < PROXIMITY_LIMIT) %>%
  rename(person1 = word) %>%
  filter(!(person1 == person2)) %>%
  na.omit()


character_summary <- character_proximity %>%
  group_by(book_num,title,person1,person2) %>%
  summarise(interactions = n(),avg_dist = mean(distance),.groups="drop") %>%
  ungroup() %>%
  # group_by(title) %>%
  arrange(desc(interactions))

# combine reciprocal pairs
relations_agg <- character_summary %>%
  ungroup() %>%
  select(person1, person2, book_num, title, avg_dist, interactions) %>%
  rowwise() %>%
  mutate(person12 = list(sort(c(person1, person2)))) %>%
  mutate(person1 = person12[1]) %>%
  mutate(person2 = person12[2]) %>%
  select(-person12) %>%
  group_by(person1, person2, book_num) %>%
  summarise(
    interactions = sum(interactions),
    log_interactions = log(sum(interactions)),
    avg_dist = mean(avg_dist),
    .groups = "drop"
  ) %>%
  {.}


# graph from complete interactions
relations <- character_summary %>%
  ungroup() %>%
  select(person1, person2, title, book_num, interactions, avg_dist)


g <- graph_from_data_frame(relations_agg,
                           vertices = characters,
                           directed=FALSE)
print(g, e=TRUE, v=TRUE)
plot(g)


wc <- cluster_walktrap(g)
modularity(wc)
membership(wc)
plot(wc, g)

gexg <- rgexf::igraph.to.gexf(g)

plot(gexg)




# combine reciprocal pairs
relations <- character_proximity %>%
  ungroup() %>%
  select(person1, person2, distance) %>%
  group_by(person1, person2) %>%
  summarise(
    avg_dist = mean(distance),
    .groups = "drop"
  ) %>%
  {.}


# prune network to top percent
relations_subset <- relations_agg %>%
  slice_max(interactions,prop=0.25)

characters_subset <- relations_subset %>%
  pivot_longer(c(person1,person2),values_to = "person") %>%
  select(person) %>%
  unique %>%
  left_join(characters)

g <- graph_from_data_frame(relations_subset,
                           vertices = characters_subset,
                           directed=FALSE)
print(g, e=TRUE, v=TRUE)
plot(g)


wc <- cluster_walktrap(g)
modularity(wc)
membership(wc)
plot(wc, g)

gexg <- rgexf::igraph.to.gexf(g)

plot(gexg)

# create edges and nodes spreadsheet for gephi import
relations_subset %>%
  rename(Source=person1,Target=person2) %>%
  mutate(Type = "Undirected",Weight=interactions,.after = Target) %>%
  write_csv(file="./data/edges_summary.csv")

characters_subset %>%
  rename(id=person) %>%
  mutate(label = id,.after = id) %>%
  write_csv(file="./data/nodes_summary.csv")
