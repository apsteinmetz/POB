# convert epub to text
library(tidyverse)
library(epubr)


chapter_names <- read_csv(file="../number_names1to50.csv") %>%
  mutate(name = paste("CHAPTER",name)) %>%
  mutate(len = str_length(name))

book_dirs <- dir("./epub")

epub_files <- paste0("./epub/",book_dirs) %>%
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

make_plain_text_file <- function(title1,corpus) {
  print(title1)
  corpus %>%
    filter(title == title1) %>%
    # unnest(cols = data) %>%
    # append space at end of line
    mutate(text = paste0(text, " ")) %>%
    pull(text) %>%
    str_flatten() %>%
    write_file(paste0("./txt/", title1, ".txt"))
}


all_pob_text$title %>% walk(make_plain_text_file,all_pob_text)
