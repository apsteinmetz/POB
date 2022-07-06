library(tidyverse)
library(lubridate)
library(tidytuesdayR)
library(jpeg)
library(png)
library(grid)
library(extrafont)
library(ggmulti)

# Get the Data

# Read in with tidytuesdayR package
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

#nyt_titles <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/#2022-05-10/nyt_titles.tsv')

nyt_full <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_full.tsv')

# fix titles table
nyt_titles_b <- nyt_full %>%
  group_by(title_id,author,title) %>%
  transmute(author,title = str_to_title(title),
            first_week = min(week),
            best_rank = min(rank),
            total_weeks=n())

nyt_titles <- nyt_full %>%
  rename(debut_rank = rank,first_week = week) %>%
  right_join(nyt_titles_b) %>%
  unique() %>%
  mutate(year = year(first_week)) %>%
  select(title_id,title,author,year,total_weeks,first_week,debut_rank,best_rank)

nyt_titles %>%
  filter(author == "Patrick O'Brian") %>%
  select(title,first_week,debut_rank,best_rank,total_weeks) %>%
  knitr::kable()

img_comm <- rasterGrob(readJPEG("img/commodore.jpg"))
img_yellow <- rasterGrob(readJPEG("img/yellow.jpg"))
img_blue <- rasterGrob(readJPEG("img/blue.jpg"))
img_hundred <- rasterGrob(readJPEG("img/hundred.jpg"))
img_bg <- rasterGrob(readJPEG("img/surf_bg.jpg"))
img_glyph <- png::readPNG("img/boat_icon.png")


nyt_full %>%
  filter(author == "Patrick O'Brian") %>%
  mutate(title = as_factor(str_to_title(title))) %>%
  group_by(title) %>%
  ggplot(aes(week,rank,fill=title)) +
  annotation_custom(img_bg,xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  geom_hline(yintercept = c(15,10,5),color="darkgreen") +
  annotation_custom(img_comm,xmin = as.Date("1994-12-01"),xmax = as.Date("1996-04-01"),ymax=-1,ymin = -8) +
  annotation_custom(img_yellow,xmin = as.Date("1996-04-01"),xmax = as.Date("1997-08-01"),ymax=-1,ymin = -8) +
  annotation_custom(img_hundred,xmin = as.Date("1997-09-01"),xmax = as.Date("1999-03-01"),ymax=-1,ymin = -8) +
  annotation_custom(img_blue,xmin = as.Date("1999-04-01"),xmax = as.Date("2000-08-01"),ymax=-1,ymin = -8) +
  geom_line(size = 1,color="blue") +
  geom_image_glyph(images = img_glyph,size = 0.5,fill="grey") +
#  geom_point(color = "darkblue",size = 4) +
  scale_y_reverse(limits = c(16,1),breaks=1:16) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(family = "Garamond",face = "italic",size=20)) +
  scale_x_date(date_breaks = "1 year",date_labels = "%Y",expand = expansion(mult = .1)) +
  labs(title = "Jack and Stephen Conquer America",
       subtitle = "Patrick O'Brian Positions on New York Times Bestseller List",
       y = "Ordo",x  = "Annus")
