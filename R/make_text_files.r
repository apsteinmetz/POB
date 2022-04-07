# make corpus of files

load("./data/austen_books.rdata")
load("./data/pob_books.rdata")
load("./data/forester_books.rdata")
load("./data/gutenberg_books.rdata")

all_books <- pob_books[1:6,] %>%
  full_join(austen_books) %>%
  full_join(forester_books) %>%
  full_join(gutenberg_books)

for (n in 1:nrow(all_books)){
  print(paste0("./stylo/corpus/",all_books$label[n],".txt"))
  write_file(all_books$text[n],
             file = paste0("./stylo/corpus/",all_books$label[n],".txt"))

}




write_file(all_books$text[n],
           file = "./stylo/corpus/temp.txt")
