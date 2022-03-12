# load libraries
library(rvest)
library(tidyverse)
library(tidytext)
library(patchwork)

# pull the text off the webpage
scrape1 <- read_html('https://en.wikipedia.org/wiki/Web_scraping')

scrape1_nodes <- scrape1 %>%
  html_nodes("p")

length(scrape1_nodes)

head(scrape1_nodes)

scrape1_text <- tibble(source='wiki',text = scrape1 %>%
                         html_nodes("p") %>%
                         html_text())

head(scrape1_text)

tail(scrape1_text, 10)

# pull the text off the webpage
scrape2 <- read_html('https://www.marshall.edu/onemarshallu/i-have-a-dream/')

scrape2

# make the text tibble
scrape2_text <- tibble(source = 'mlk', text = scrape2 %>%
                         html_nodes("p") %>%
                         html_text())

scrape2_text

n <- dim(scrape2_text)[1] # find the number of rows
n

scrape2_text <- scrape2_text[1:(n-3),] 

scrape2_text

