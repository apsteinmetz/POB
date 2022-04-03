# stylo analysis
library(tidyverse)
library(tidytext)
library(stylo)
library(ggdendro)



my.test = stylo(gui = FALSE, corpus.dir = "ShakespeareCanon")
my.test = stylo(gui = FALSE, corpus.dir = "ShakespeareCanon")
summary(my.test)

# batch mode, character 3-grams requested:
style_dendro <- stylo(gui = FALSE,
#      analyzed.features = "c",
      ngram.size = 3,
      corpus.dir = "stylo/corpus")

hclust(dist(style_dendro$distance.table),
       method = "ward.D2") %>%
  plot()


