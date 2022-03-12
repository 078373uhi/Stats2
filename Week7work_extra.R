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

library(stringr)

scrape2_text$text <- str_remove(scrape2_text$text, "\\[[^\\)]+\\]") 

# pull the text off the webpage
scrape3 <- read_html('https://winstonchurchill.org/resources/speeches/1940-the-finest-hour/we-shall-fight-on-the-beaches/')

scrape3_text <- tibble(source = 'wc', text = scrape3 %>%
                         html_nodes("p") %>%
                         html_text())

head(scrape3_text, 10)

tail(scrape3_text, 10)

n <- dim(scrape3_text)[1] # find the number of rows

scrape3_text <- scrape3_text[5:(n-5),] 

scrape3_text

# Have a go at performing a sentiment analysis on these texts.
# First, bind them, then tidy them
# bind the texts together
texts <- rbind(scrape1_text,scrape2_text,scrape3_text)

# tidy text 
tidy_texts <- texts %>%
  unnest_tokens(word, text)

tidy_texts

tidy_texts %>%
  count(word, sort=TRUE)

tidy_texts %>%
  group_by(source) %>%
  count(word, sort=TRUE)

tidy_texts <-  tidy_texts %>%
  anti_join(stop_words) 

tidy_texts %>%
  count(word, sort=TRUE)

tidy_texts %>%
  group_by(source) %>%
  count(word, sort=TRUE)

library(ggplot2)

tidy_texts %>%
  count(word, sort = TRUE) %>%
  filter(n > 10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill=word)) +
  geom_col() +
  labs(title='Word frequency for The 3 Texts',
       y = NULL, x= "word count") +
  theme(legend.position="none")
