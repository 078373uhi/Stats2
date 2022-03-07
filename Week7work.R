# load libraries
library(tidyverse)
library(tidytext)
library(janeaustenr)

# get our books and tidy them with bigrams
books_bigrams <- austen_books() %>%
  filter(book == "Persuasion" | book == "Mansfield Park") %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

# reformat to have words in sep. columns
bigrams_sep <- books_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# remove stop words
bigrams_no_stop <- bigrams_sep %>%
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word)

# count bigrams
bigram_counts <- bigrams_no_stop %>%
  count(word1, word2, sort=TRUE)

bigram_counts

mk.bigram <- function(dataset) {
  dataset %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    na.omit(word1) %>%
    count(word1, word2, sort = TRUE)
}

books_bigrams <- austen_books() %>%
  filter(book == "Persuasion" | book == "Mansfield Park") 

bigram_counts<-mk.bigram(books_bigrams)

bigram_counts

#Visualising Bigrams
# Load library
library(igraph)
library(ggraph)

head(bigram_counts)

# use igram functionality
bigram_graph <- graph_from_data_frame(bigram_counts)

# look at result
bigram_graph

# use igram functionality
bigram_graph <- bigram_counts %>%
  filter(n>10) %>%
  graph_from_data_frame()

# look at result
bigram_graph

# set seed - this doesn't change the content of the plot, but ensures that the 
# position of the groupings within the plot remains constant. Try running the 
# code with and without a seed to see the difference.
set.seed(2)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point(color = "blue") +
  geom_node_text(aes(label = name), repel = TRUE)

