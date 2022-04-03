# make corpus of files

load("./data/austen_books.rdata")
load("./data/pob_books.rdata")
load("./data/forester_books.rdata")
load("./data/gutenberg_books.rdata")

all_books <- pob_books[1:6,] %>%
  full_join(austen_books) %>%
  full_join(forester_books) %>%
  full_join(gutenberg_books)

all_books$label %>%
  paste0("./stylo/corpus/",.,".txt")


  walk(write_file)
