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

set.seed(2)

a <- grid::arrow(type='closed', length = unit(0.25,"cm"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n),
                 arrow = a, end_cap = circle(0.25, "cm")) +
  geom_node_point(colour="lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void() +   # removes the border
  theme(legend.position="none")

set.seed(2)


V(bigram_graph)$size <- degree(bigram_graph)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_arc(strength = 0.2, width = 0.5, alpha = 0.15) +
  geom_node_point(aes( size = size),color = "lightblue") +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme(legend.position = "none")

library(patchwork)

set.seed(2)

# Test different layouts 
g1 <- ggraph(bigram_graph, layout = "mds") +
  geom_edge_arc(strength=0.2, width=0.5, alpha=.15) + 
  geom_node_point(aes( size = size),color = "lightblue") + 
  theme_void() +
  theme(legend.position = "none") + 
  labs(title = "Multi-Dimensional Scaling")

g2 <- ggraph(bigram_graph, layout = "kk") +
  geom_edge_arc(strength=0.2, width=0.5, alpha=.15) + 
  geom_node_point(aes( size = size),color = "lightblue") + 
  theme_void() +
  theme(legend.position = "none") + 
  labs(title = "Kamada-Kawai")

g3 <- ggraph(bigram_graph, layout = "lgl") +
  geom_edge_arc(strength=0.2, width=0.5, alpha=.15) + 
  geom_node_point(aes( size = size),color = "lightblue") + 
  theme_void() +
  theme(legend.position = "none") + 
  labs(title = "Large Graph Layout") 


g4 <- ggraph(bigram_graph, layout = "graphopt") +
  geom_edge_arc(strength=0.2, width=0.5, alpha=.15) + 
  geom_node_point(aes( size = size),color = "lightblue") + 
  theme_void() +
  theme(legend.position = "none") + 
  labs(title = "GraphOPT")

(g1 + g2) 
(g3 + g4)

#Investigating Other Texts
vis.bigrams <- function(bigrams) {
  set.seed(2)
  a <- grid::arrow(type = "closed", length = unit(0.25, "cm"))
  
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(arrow = a,end_cap = circle(0.25, "cm")) +
    geom_node_point(color = "lightblue", aes(size = 5)) +
    geom_node_text(aes(label = name),repel = TRUE) +
    theme_void() +
    theme(legend.position = "none")
}

# load the library
library(gutenbergr)

# grab the text
book <- tibble(text = gutenberg_download(61262)$text) 

# find the bigrams
bigram_counts <- mk.bigram(book)

head(bigram_counts)

# filter for most common, & visualize the bigrams

bigram_counts %>%
  filter(n > 5) %>%
  vis.bigrams()
