# load required packages
library(rvest)
library(tidyverse)
library(tidytext)
library(patchwork)
library(stringr)
library(ggplot2)
library(widyr)
library(wordcloud2) 
library(wordcloud)
library(igraph)
library(ggraph)

# get first text off the webpage
biden <- read_html('https://www.presidency.ucsb.edu/documents/address-before-joint-session-the-congress-the-state-the-union-28')

biden_nodes <- biden %>%
  html_nodes("p")

#inspect text
length(biden_nodes)

head(biden_nodes)

#create tibble from text
biden_text <- tibble(source='bid',text = biden %>%
                         html_nodes("p") %>%
                         html_text())

#inspect tibble
head(biden_text)

tail(biden_text, 10)
