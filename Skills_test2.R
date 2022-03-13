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

# The text chosen for this analysis was the first State of the Union address from 
# each of the five most recent American presidents; Biden, Trump, Obama, Bush Jr
# and Clinton.

# FIRST TEXT
biden <- read_html('https://www.presidency.ucsb.edu/documents/address-before-joint-session-the-congress-the-state-the-union-28')

biden_nodes <- biden %>%
  html_nodes("p")

# inspect text
length(biden_nodes)

head(biden_nodes)

# create tibble from text
biden_text <- tibble(source='bid',text = biden %>%
                         html_nodes("p") %>%
                         html_text())

# inspect tibble
head(biden_text)

tail(biden_text, 10)

# remove unecessary introduction/ending lines and brackets of applause etc.
biden_text$text <- str_remove(biden_text$text, "\\[[^\\)]+\\]")
n <- dim(biden_text)[1] 
biden_text <- biden_text[3:(n-7),] 

# check final text
head(biden_text)

tail(biden_text, 10)

# SECOND TEXT
trump <- read_html('https://www.presidency.ucsb.edu/documents/address-before-joint-session-the-congress-the-state-the-union-25')

# THIRD TEXT
obama <- read_html('https://www.presidency.ucsb.edu/documents/address-before-joint-session-the-congress-the-state-the-union-17')

# FOURTH TEXT
bush <- read_html('https://www.presidency.ucsb.edu/documents/address-before-joint-session-the-congress-the-state-the-union-22')

# FIFTH TEXT
clinton <- read_html('https://www.presidency.ucsb.edu/documents/address-before-joint-session-the-congress-the-state-the-union-12')
