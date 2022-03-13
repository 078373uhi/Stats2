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

# •	Choose at least two different US Presidents for your analysis.
# •	Scrape text from the website and create a corpus.

# The text chosen for this analysis was the first State of the Union Address from 
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

trump_nodes <- trump %>%
  html_nodes("p")

# inspect text
length(trump_nodes)

head(trump_nodes)

# create tibble from text
trump_text <- tibble(source='tru',text = trump %>%
                       html_nodes("p") %>%
                       html_text())

# inspect tibble
head(trump_text)

tail(trump_text, 10)

# remove unecessary introduction/ending lines and brackets of applause etc.
trump_text$text <- str_remove(trump_text$text, "\\[[^\\)]+\\]")
n <- dim(trump_text)[1] 
trump_text <- trump_text[3:(n-6),] 

# check final text
head(trump_text)

tail(trump_text, 10)

# THIRD TEXT
obama <- read_html('https://www.presidency.ucsb.edu/documents/address-before-joint-session-the-congress-the-state-the-union-17')

obama_nodes <- obama %>%
  html_nodes("p")

# inspect text
length(obama_nodes)

head(obama_nodes)

# create tibble from text
obama_text <- tibble(source='oba',text = obama %>%
                       html_nodes("p") %>%
                       html_text())

# inspect tibble
head(obama_text)

tail(obama_text, 10)

# remove unecessary introduction/ending lines and brackets of applause etc.
obama_text$text <- str_remove(obama_text$text, "\\[[^\\)]+\\]")
n <- dim(obama_text)[1] 
obama_text <- obama_text[3:(n-6),] 

# check final text
head(obama_text)

tail(obama_text, 10)

# FOURTH TEXT
bush <- read_html('https://www.presidency.ucsb.edu/documents/address-before-joint-session-the-congress-the-state-the-union-22')

bush_nodes <- bush %>%
  html_nodes("p")

# inspect text
length(bush_nodes)

head(bush_nodes)

# create tibble from text
bush_text <- tibble(source='bus',text = bush %>%
                       html_nodes("p") %>%
                       html_text())

# inspect tibble
head(bush_text)

tail(bush_text, 10)

# remove unecessary introduction/ending lines and brackets of applause etc.
bush_text$text <- str_remove(bush_text$text, "\\[[^\\)]+\\]")
n <- dim(bush_text)[1] 
bush_text <- bush_text[3:(n-6),] 

# check final text
head(bush_text)

tail(bush_text, 10)

# FIFTH TEXT
clinton <- read_html('https://www.presidency.ucsb.edu/documents/address-before-joint-session-the-congress-the-state-the-union-12')

clinton_nodes <- clinton %>%
  html_nodes("p")

# inspect text
length(clinton_nodes)

head(clinton_nodes)

# create tibble from text
clinton_text <- tibble(source='cli',text = clinton %>%
                      html_nodes("p") %>%
                      html_text())

# inspect tibble
head(clinton_text)

tail(clinton_text, 10)

# remove unecessary introduction/ending lines and brackets of applause etc.
clinton_text$text <- str_remove(clinton_text$text, "\\[[^\\)]+\\]")
n <- dim(clinton_text)[1] 
clinton_text <- clinton_text[3:(n-6),] 

# check final text
head(clinton_text)

tail(clinton_text, 10)

# •	Mine the text (word analysis), making comparisons between the presidents

# collate the texts together
pres <- rbind(biden_text,trump_text,obama_text, bush_text, clinton_text)

# tidy up the text 
tidy_pres <- pres %>%
  unnest_tokens(word, text)

tidy_pres

# count unique words then group by source (president)
tidy_pres %>%
  count(word, sort=TRUE)

tidy_pres %>%
  group_by(source) %>%
  count(word, sort=TRUE)

# remove stop words and recount
tidy_pres_ns <-  tidy_pres %>%
  anti_join(stop_words) 

tidy_pres_ns %>%
  count(word, sort=TRUE)

tidy_pres_ns %>%
  group_by(source) %>%
  count(word, sort=TRUE)

# plots
# Biden plot
bid_plot <- tidy_pres_ns %>%
  filter(source == "bid") %>%
  count(word, sort = TRUE) %>%
  filter(n > 15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = word)) +
  geom_col() +
  labs(title='Word frequency for the Biden speech',
       y = NULL, x= "word count") +
  theme(legend.position="none")

# Trump plot
tru_plot <- tidy_pres_ns %>%
  filter(source == "tru") %>%
  count(word, sort = TRUE) %>%
  filter(n > 15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = word)) +
  geom_col() +
  labs(title='Word frequency for the Trump speech',
       y = NULL, x= "word count") +
  theme(legend.position="none")

# Obama plot
oba_plot <- tidy_pres_ns %>%
  filter(source == "oba") %>%
  count(word, sort = TRUE) %>%
  filter(n > 15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = word)) +
  geom_col() +
  labs(title='Word frequency for the Obama speech',
       y = NULL, x= "word count") +
  theme(legend.position="none")

# Bush plot
bus_plot <- tidy_pres_ns %>%
  filter(source == "bus") %>%
  count(word, sort = TRUE) %>%
  filter(n > 15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = word)) +
  geom_col() +
  labs(title='Word frequency for the Bush speech',
       y = NULL, x= "word count") +
  theme(legend.position="none")

# Clinton plot
cli_plot <- tidy_pres_ns %>%
  filter(source == "cli") %>%
  count(word, sort = TRUE) %>%
  filter(n > 15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = word)) +
  geom_col() +
  labs(title='Word frequency for the Clinton speech',
       y = NULL, x= "word count") +
  theme(legend.position="none")

bid_plot + tru_plot + oba_plot + bus_plot + cli_plot

# It is clear there are words that are very common to all the speeches  but do  
# not add much information so I amgoing to remove these along with the other 
# stop words
tidy_pres2 <- tidy_pres_ns %>%
  subset(word!="people") %>%
  subset(word!="america") %>%
  subset(word!="american") %>%
  subset(word!="americans")

tidy_pres2 %>%
group_by(source) %>%
  count(word, sort=TRUE)

# plot the count of the words by words said 25 times or more
tidy_pres2 %>%
  count(word, sort = TRUE) %>%
  filter(n > 25) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill=word)) +
  geom_col() +
  labs(title='Word frequency for the five speeches',
       y = NULL, x= "word count") +
  theme(legend.position="none")

# plots by President
# Biden plot
bid_plot2 <- tidy_pres2 %>%
  filter(source == "bid") %>%
  count(word, sort = TRUE) %>%
  filter(n > 10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = word)) +
  geom_col() +
  labs(title='Word frequency for the Biden speech',
       y = NULL, x= "word count") +
  theme(legend.position="none")

# Trump plot
tru_plot2 <- tidy_pres2 %>%
  filter(source == "tru") %>%
  count(word, sort = TRUE) %>%
  filter(n > 10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = word)) +
  geom_col() +
  labs(title='Word frequency for the Trump speech',
       y = NULL, x= "word count") +
  theme(legend.position="none")

# Obama plot
oba_plot2 <- tidy_pres2 %>%
  filter(source == "oba") %>%
  count(word, sort = TRUE) %>%
  filter(n > 10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = word)) +
  geom_col() +
  labs(title='Word frequency for the Obama speech',
       y = NULL, x= "word count") +
  theme(legend.position="none")

# Bush plot
bus_plot2 <- tidy_pres2 %>%
  filter(source == "bus") %>%
  count(word, sort = TRUE) %>%
  filter(n > 10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = word)) +
  geom_col() +
  labs(title='Word frequency for the Bush speech',
       y = NULL, x= "word count") +
  theme(legend.position="none")

# Clinton plot
cli_plot2 <- tidy_pres2 %>%
  filter(source == "cli") %>%
  count(word, sort = TRUE) %>%
  filter(n > 10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = word)) +
  geom_col() +
  labs(title='Word frequency for the Clinton speech',
       y = NULL, x= "word count") +
  theme(legend.position="none")

bid_plot2 + tru_plot2 + oba_plot2 + bus_plot2 + cli_plot2
