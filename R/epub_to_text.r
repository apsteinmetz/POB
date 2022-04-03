# convert epub to text
library(tidyverse)
library(epubr)


chapter_names <- read_csv(file="../number_names1to50.csv") %>%
  mutate(name = paste("CHAPTER",name)) %>%
  mutate(len = str_length(name))

# ----------- GET POB -------------------
book_dir <- "./epub/POB"
book_dirs <- dir(book_dir)

epub_files <- paste(book_dir,book_dirs,sep="/") %>%
  map(list.files,pattern="epub$",full.names = TRUE) %>%
  unlist

books  <- epub_files %>%
  map(epub) %>%
  bind_rows()  %>%
  select(title,date,data) %>%
  transmute(title,date = as.Date(substr(date,1,10)),text=data) %>%
  arrange(date) %>%
  rownames_to_column(var="book_num")

all_text <- unnest(books,text,names_sep = ) %>%
  filter(str_detect(text,"(^CHAPTER)|(^Chapter)")) %>%
  mutate(across(.fns = unname))

get_chapter_name <- function(chapter) {
  for (cn in nrow(chapter_names):1) {
    # search in  descending so longer names are searched first
    # we don't want to select chapter four when it's fourteen
    search_str <- paste0("^",chapter_names$name[cn],"|^",toupper(chapter_names$name[cn]))
    if(str_detect(chapter$text,search_str)){
      chapter$section <- as.character(chapter_names$number[cn])
      chapter$text <- str_remove(chapter$text,search_str)
      chapter$nword <- chapter$nword - 2
      chapter$nchar <- chapter$nchar - str_length(chapter_names$name[cn])
      return(chapter)
    }

  }
  return("Chapter 00")
}


for (n in 1:nrow(all_text)){
  chapter <- get_chapter_name(all_text[n,])
  all_text[n,] <- chapter[1,]
}
all_pob_text <- all_text %>% rename(chapter = section)

save(all_pob_text,file="data/all_pob_text.rdata")

pob_books <- all_pob_text %>%
  group_by(book_num,title,date) %>%
  summarise(text = str_c(text,collapse =  " ")) %>%
  mutate(year = lubridate::year(date)) %>%
  mutate(author = "O'Brian") %>%
  mutate(label = paste(author,title,year,sep = "-")) %>%
  arrange(year)

save(pob_books,file="data/pob_books.rdata")


