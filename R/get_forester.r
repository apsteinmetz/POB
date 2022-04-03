# ------- GET FORESTER -----------------
library(epubr)
book_dir <- "./txt/Forester"
book_names <- dir(book_dir) %>% str_remove("\\.txt$")
txt_files <- paste(book_dir,dir(book_dir),sep="/")

# relies on order
# commodore, atropos, hotspur, lord, lt., midshipman
years <- c(1945,1953,1962,1946,1952,1950)

forester_books <- txt_files %>%
  map(read_file) %>% enframe() %>%
  transmute(title=book_names,text = unlist(value)) %>%
  # strip header info - approx.
  mutate(text = str_sub(text,1400)) %>%
  mutate(text=str_replace_all(text,"\r"," ")) %>%
  mutate(text=str_remove_all(text,"\n")) %>%
  mutate(author = "C.S. Forester",.before=text) %>%
  mutate(year = years,.before=text) %>%
  mutate(label = paste("Forester",title,year,sep = "_"),.before=text) %>%
  arrange(year)



save(forester_books,file="data/forester_books.rdata")

