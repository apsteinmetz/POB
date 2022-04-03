# import gutenberg austin and sabatini
library(tidyverse)
library(tidytext)
library(janeaustenr)
library(gutenbergr)

# relies on order
# sense, pride, mansfield, emma, northanger,persuasion
years = c(1811,1813,1814,1816,1803,1818)
austen_books <- janeaustenr::austen_books() %>%
  rename(title=book) %>%
  group_by(title) %>%
  summarise(text = str_c(text,collapse = " ")) %>%
  mutate(author = "Jane Austen",.before=text) %>%
  mutate(year = years,.before=text) %>%
  mutate(label = paste("Austen",title,year,sep="_"),.before=text) %>%
  arrange(year)

save(austen_books,file="./data/austen_books.rdata")

# --- Gutenberg ---------------------
metadata <- gutenberg_metadata


# Nautical adventure novels
# Rafael Sabatini. The Sea Hawk and Captain Blood
sabatini_book_ids <- c(1965,3294)

# Female novelists, contemporaries of Austen
# http://femalescriblerian.com/2016/03/21/5-authors-to-read-if-you-love-jane-austen/
# Maria Edgeworth. Belinda and Tales of a fashionable life
edgeworth_book_ids <- c(9455,9414)
# Francis Burney. Camilla
burney_book_ids <- 40619
# Charlotte Brontë. Jane Eyre
bronte_book_ids <- 1260
# 20th century Austen heirs
# Georgette Heyer. The Black Moth
heyer_book_ids <- 38703

book_ids <- c(edgeworth_book_ids,
              burney_book_ids,
              bronte_book_ids,
              sabatini_book_ids,
              heyer_book_ids)

gutenberg_books <- gutenberg_download(book_ids,
                                     meta_fields = c("title","author"))

better_titles = c("Jane Eyre",
                  "Captain Blood",
                  "The Sea Hawk",
                  "Fashionable Life",
                  "Belinda",
                  "The Black Moth",
                  "Camilla")

book_year <- c(1847,
               1922,
               1915,
               1809,
               1801,
               1921,
               1796)

gutenberg_books <- gutenberg_books %>%
  group_by(gutenberg_id,title,author) %>%
  summarise(text = str_c(text,collapse =  " ")) %>%
  ungroup %>%
  mutate(title = better_titles,.before=text) %>%
  mutate(year = book_year,.before=text) %>%
  mutate(label = paste(str_extract(author,".+(?=,)"),
                        title,
                        year,
                        sep = "_"),
         .before=text) %>%
  {.}


save(gutenberg_books,file="./data/gutenberg_books.rdata")


# --- Utilities ------------------------------------
make_plain_text_file <- function(corpus) {
  print(title1)
  corpus %>%
    filter(title == title1) %>%
    unnest(cols = data) %>%
    #append space at end of line
    mutate(text = paste0(text, " ")) %>%
    pull(text) %>%
    str_flatten() %>%
    write_file(paste0("./txt/", title1, ".txt"))
}


gutenberg_books$title %>% walk(make_plain_text_file,gutenberg_books)
austen_books$title %>% walk(make_plain_text_file,austen_books)

austen_tkn <-austen_books %>%
  unnest(cols=data) %>%
  group_by(title) %>%
  mutate(line = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()



gutenberg_tkn <- gutenberg_books %>%
  unnest(cols=data) %>%
  group_by(title) %>%
  mutate(line = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()


