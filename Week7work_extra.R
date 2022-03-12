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

bing_word_counts <- tidy_texts %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(source, word, sentiment, sort = TRUE) %>%
  ungroup()

head(bing_word_counts)

bing_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(sentiment~source, scales = "free_y") +
  labs(x = "Contribution to Sentiment",
       y = NULL)

texts_bigrams <- texts %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)
texts_bigrams

# format to have words in sep. columns
bigrams_sep <- texts_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# remove stop words
bigrams_no_stop <- bigrams_sep %>%
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word)

# count bigrams
bigram_counts <- bigrams_no_stop %>%
  count(source, word1, word2, sort=TRUE)

head(bigram_counts)

# convert bigrams from two columns back to one
bigrams_united <- bigrams_no_stop %>%
  unite(bigram, word1, word2, sep = " ")

# calculate the tf-idf
bigram_tf_idf <- bigrams_united %>%
  count(source, bigram) %>%
  bind_tf_idf(bigram, source, n) %>%
  arrange(desc(tf_idf))

head(bigram_tf_idf)

# visualize results
bigram_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  group_by(source) %>%
  slice_max(tf_idf, n = 7) %>%
  ungroup() %>%
  mutate(bigram = reorder(bigram, tf_idf)) %>%
  ggplot(aes(tf_idf, bigram, fill = source)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ source,  scales = "free") +
  labs(x = "tf-idf of bigram", y = NULL)

negation_words <- c("not", "no", "never", "without")

# count affected words
negated_words <- bigrams_sep %>%
  filter(word1 %in% negation_words) %>%
  inner_join(get_sentiments("afinn"), by = c(word2 = "word")) %>%
  count(source, word1, word2, value, sort = TRUE)

head(negated_words)

# plot affected words
negated_words %>%
  mutate(contribution = n * value,
         word2 = reorder(paste(word2, word1, sep = "__"), contribution)) %>%
  group_by(word1) %>%
  slice_max(abs(contribution), n = 12, with_ties = FALSE) %>%
  ggplot(aes(word2, contribution, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(source ~ word1, scales = "free") +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  xlab("Words preceded by negation term") +
  ylab("Sentiment value * # of occurrences") +
  coord_flip()

wiki_section_words <- texts %>%
  filter(source == "wiki") %>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)

head(wiki_section_words)

#Carry out pairwise count and pairwise correlation. Need package widyr

library(widyr)
# count words co-occuring within sections
wiki_word_pairs <- wiki_section_words %>%
  pairwise_count(word, section, sort = TRUE)
wiki_word_pairs

# we need to filter for at least relatively common words first
wiki_word_cors <- wiki_section_words %>%
  group_by(word) %>%
  filter(n >= 10) %>%
  pairwise_cor(word, section, sort = TRUE)
wiki_word_cors

#What words have a strong correlation with ‘write’
wiki_word_cors %>%
  filter(item1 == "write")

#Twitter
install.packages("rtweet")
library(rtweet)

sample_tweets <- search_tweets(q = "#COP26 OR #cop26",   # query hashtag. Can also search for text
                               n = 500,          # number of results
                               include_rts = FALSE, # Don't include retweets
                               type = "recent",  # could be 'popular',
                               lang = "en")      # Set language to english

# Write data (working with 'live' data)
#write.csv(sample_tweets, "Data/sample_tweets.csv")


# Create a table of users and tweet counts for the topic
sc_name <- table(sample_tweets$screen_name)
# NB user_id is a better option to use, as users can change user names, but not
# their ID.

# Sort the table in descending order of tweet counts
sc_name_sort <- sort(sc_name, decreasing = TRUE)

# View sorted table for top 10 users
head(sc_name_sort, 10)

# Extract tweets posted by the top user 

get_top <- get_timeline("@valuestoimpact", n = 500)

#Analyse Text
#Text clean up - strip urls, @, &, punctuation, emojis

sample_tweets$text <-  gsub("https\\S*", "", sample_tweets$text) 
sample_tweets$text <-  gsub("@\\S*", "", sample_tweets$text) 
sample_tweets$text  <-  gsub("amp", "", sample_tweets$text) 
sample_tweets$text  <-  gsub("[\r\n]", "", sample_tweets$text)
sample_tweets$text  <-  gsub("[[:punct:]]", "", sample_tweets$text)
sample_tweets$text  <-  gsub('\\p{So}|\\p{Cn}', '', sample_tweets$text, perl = TRUE)

words_tweets <- sample_tweets %>%
  select(text) %>%
  unnest_tokens(word, text)

words_tweets %>% 
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n, fill=word)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Most frequent words found in the extracted tweets") +
  theme(legend.position = "None")

# the, to, in, and etc are called 'stop-words'.  Need to remove these.
words_tweets_ns <- words_tweets %>%
  anti_join(stop_words)

# Plot again
words_tweets_ns %>% 
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n, fill=word)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Most frequent words found in the extracted tweets",
       subtitle = "'Stop words' removed from the list") + 
  theme(legend.position = "None")

# Remove cop26 from word list (too many!)
tweet_samp <- words_tweets_ns %>%
  subset(word!="cop26") 

# Plot again
tweet_samp %>% 
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n, fill=word)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Most frequent words found in the extracted tweets",
       subtitle = "'Stop words & COP26' removed from the list") + 
  theme(legend.position = "None")

# Word Cloud
library(wordcloud2) 
library(wordcloud)

top_100 <- tweet_samp %>% 
  count(word, sort = TRUE) %>%
  top_n(100)

wordcloud2(data=top_100)

wordcloud(words=top_100$word, freq = top_100$n, colors = brewer.pal(8,"Dark2"))

wordcloud(words=top_100$word, freq = top_100$n,colors = brewer.pal(8,"Dark2"), scale = c(2.5, 0.6))

#Sentiment Analysis
tweets_nrc <- tweet_samp %>%
  inner_join(get_sentiments("nrc"), by = "word") 

tweets_bing <- tweet_samp %>%
  inner_join(get_sentiments("bing"), by = "word")

p1 <- tweets_bing %>%
  count(sentiment, sort = TRUE)  %>%
  mutate(sentiment = reorder(sentiment, n)) %>%
  ggplot(aes(x = sentiment, y = n, fill=sentiment)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words")+
  theme(legend.position = "None")

p2 <- tweets_nrc %>%
  count(sentiment, sort = TRUE)  %>%
  mutate(sentiment = reorder(sentiment, n)) %>%
  ggplot(aes(x = sentiment, y = n, fill=sentiment)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words")+
  theme(legend.position = "None")

p1 / p2

#Relationships
#sample_tweets was our original data.

tweet_bigrams <- sample_tweets %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

tweet_bigrams %>%
  count(bigram, sort = TRUE)

# reformat to have words in sep. columns
bigrams_sep <- tweet_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# remove stop words
bigrams_no_stop <- bigrams_sep %>%
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word)

# count bigrams
bigram_counts <- bigrams_no_stop %>%
  count(word1, word2, sort=TRUE)

bigram_counts

library(igraph)
library(ggraph)

# plot climate change word network
# (plotting graph edges is currently broken)
set.seed(12)

a <- grid::arrow(type='closed', length = unit(0.25,"cm"))

bigram_counts %>%
  filter(n>10) %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n),
                 arrow = a, end_cap = circle(0.25, "cm")) +
  geom_node_point(colour="lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void() +   # removes the border
  theme(legend.position="none")+
  labs(title = "Word Network: Tweets using the hashtag - COP26",
       subtitle = "Text mining twitter data ",
       x = "", y = "")
