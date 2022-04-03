# stylo analysis
library(tidyverse)
library(tidytext)
library(stylo)
library(ggdendro)


load("./data/austen_books.rdata")
load("./data/pob_books.rdata")
load("./data/forester_books.rdata")
load("./data/gutenberg_books.rdata")

all_books <- pob_books[1:6,] %>%
  full_join(austen_books) %>%
  full_join(forester_books) %>%
  full_join(gutenberg_books)


#
# # batch mode, character 3-grams requested:
# style_dendro <- stylo(gui = FALSE,
# #      analyzed.features = "c",
#       ngram.size = 3,
#       corpus.dir = "stylo/corpus")
#
# hclust(dist(lee),
#        method = "ward.D2") %>%
#   plot()
#
#


make_token_list <- function(label1){
  tokenized <- all_books %>%
    filter(label == label1) %>%
    pull(text[1]) %>%
    txt.to.words() %>%
    {.}

  return(tokenized)
}


make_freq_table <- function(title1,rowprop=.01){
  freq_table <- all_books %>%
    filter(title == title1) %>%
    pull(text[1]) %>%
    txt.to.words() %>%
    txt.to.features(ngram.size = 3) %>%
    enframe(value = "ngram_3",name = NULL) %>%
    group_by(ngram_3) %>%
    summarise(freq = n()/nrow(.)) %>%
    arrange(desc(freq)) %>%
    slice_max(order_by=freq,prop=rowprop,with_ties = FALSE) %>%
    {.}

  names(freq_table)[2] <-   all_books %>%
    filter(title == title1) %>%
    pull(label)

  return(freq_table)
}

# make frequency table of all trigrams in all books by hand
books <- all_books$title %>%
  set_names() %>%
  map(make_freq_table) %>%
  reduce(full_join,by="ngram_3") %>%
  mutate(across(.fns= ~ ifelse(is.na(.x),0,.x))) %>%
  as.data.frame()

token_corpus <- all_books$label %>%
  set_names() %>%
  map(make_token_list)



# filter out some nautical phrases
nautical_terms <- "sophie|surprise|frigate|quaterdeck|starboard|larboard|ship|captain|midshipman"

books2 <-books %>%
  filter(!str_detect(ngram_3,nautical_terms))

rownames(books2) <- books2$ngram_3
books <- select(books,-ngram_3) %>% t()

# analyze
stylo(
  parsed.corpus = token_corpus,
  analysis.type = "BCT",
  ngram.size = 3,
  culling.min = 50,
  cullin.max = 50,
  mfw.min = 100,
  mfw.max = 3000,
  custom.graph.title = "Patrick O'Brian",
  write.png.file = FALSE,
  gui = FALSE
)

stylo(
  parsed.corpus = token_corpus,
  analysis.type = "CA",
  ngram.size = 3,
  culling.min = 50,
  cullin.max = 50,
  mfw.min = 100,
  mfw.max = 3000,
  custom.graph.title = "O'Brian, Austen & Co.",
  write.png.file = FALSE,
  gui = FALSE
)

freq_table <- book_style$table.with.all.freqs %>%
  as_tibble(rownames = "book") %>%
  pivot_longer(cols = 1:ncol(book_style$table.with.all.freqs)+1,
               names_to = "phrase",values_to = "freq") %>%
  mutate(freq = as.numeric(freq))

phrases <- c("i dare say","i assure you","if you please","not a moment")
phrases <- c("i dare say","i assure you")

interesting_freq <- freq_table %>%
  filter(phrase %in% phrases)

freq_table %>%
  filter(phrase == "i dare say") %>%
  ggplot(aes(book,freq)) + geom_col() +
  labs(y = "Relative Use",
       subtitle = "Who Uses Common Regency Vernacular?",
       title = '"I Dare Say"') +
  coord_flip()

freq_table %>%
  filter(phrase == "i assure you") %>%
  ggplot(aes(book,freq)) + geom_col() +
  labs(y = "Relative Use",
       subtitle = "Who Uses Common Regency Vernacular",
       title = '"I Assure You"') +
  coord_flip()

freq_table %>% ggplot(aes(book,`i dare say`))+geom_col() +
  coord_flip()

freq_table %>% ggplot(aes(book,`if you please`))+geom_col() +
  coord_flip()
freq_table %>% ggplot(aes(book,`not a moment`))+geom_col() +
  coord_flip()
