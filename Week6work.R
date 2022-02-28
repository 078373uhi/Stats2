# Load library
library(tidyverse)
library(tidytext)

get_sentiments("bing")

# install package
#install.packages("textdata")
# load library
install.packages("textdata")
library(textdata)

get_sentiments("nrc")

# the first time you run this code, this lexicon will ask to confirm that you wish to download it.
get_sentiments("afinn")

# the first time you run this code, this lexicon will ask to confirm that you wish to download it.
get_sentiments("loughran")

library(janeaustenr)
library(dplyr)
library(stringr)

tidy_pers <- austen_books() %>%
  filter(book=="Persuasion") %>%
  mutate(
    linenumber = row_number(),
    chapter = cumsum(str_detect(text, 
                                regex("^chapter [\\divxlc]", 
                                      ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

head(tidy_pers)

bing_pos <- get_sentiments("bing") %>%
  filter(sentiment=="positive")

head(bing_pos)

tidy_pers %>%
  inner_join(bing_pos, by="word") %>%
  count(word, sort=TRUE)

nrc_joy <- get_sentiments("nrc") %>%
  filter(sentiment=="joy")
nrc_fear <- get_sentiments("nrc") %>%
  filter(sentiment=="fear")

# count of joyful words
tidy_pers %>%
  inner_join(nrc_joy, by="word") %>%
  count(word, sort=TRUE)

# count of fearfull words
tidy_pers %>%
  inner_join(nrc_fear, by="word") %>%
  count(word, sort=TRUE)

# Pull the text for the books and make it tidy.
tidy_books <- austen_books() %>%
  filter(book == "Persuasion" | book == "Mansfield Park") %>%
  mutate(
    linenumber = row_number(),
    chapter = cumsum(str_detect(text, 
                                regex("^chapter [\\divxlc]", 
                                      ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

head(tidy_books)

books_sentiment <- tidy_books %>%
  inner_join(get_sentiments("bing"), by="word") %>%
  count(book, index = linenumber %/% 80, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)
# index=linenumber %/% 80, is splitting our text up into chunks of 80 lines each.

head(books_sentiment)

library(ggplot2)

ggplot(books_sentiment, aes(index, sentiment, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, scales = "free_x")

# nrc dataframe
books_sent_nrc <- tidy_books %>%
  inner_join(get_sentiments("nrc"), by="word") %>%
  count(book, index = linenumber %/% 80, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

# plot nrc
ggplot(books_sent_nrc, aes(index, sentiment, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, scales = "free_x")

# Remove stop words
tidy_books <-  tidy_books %>%
  anti_join(stop_words, by = "word")

# count the positives/negatives
bing_word_counts <- tidy_books %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

head(bing_word_counts)

custom_stop_words <- bind_rows(tibble(word = c("miss"),  
                                      lexicon = c("custom")), 
                               stop_words)

head(custom_stop_words)

tidy_books2 <-  tidy_books %>%
  anti_join(custom_stop_words, by = "word")

bing_word_counts2 <- tidy_books2 %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

head(bing_word_counts2)

bing_word_counts2 %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to Sentiment",
       y = NULL)

install.packages("wordcloud")
library(wordcloud) # different package from last week

tidy_books2 %>%
  count(word) %>%
  with(wordcloud(word, n, min.freq = 1, max.words = 100))

install.packages("reshape2")
library(reshape2)

tidy_books2 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "blue"),
                   max.words = 100)

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

head(bigram_counts)

bigrams_no_stop %>%
  filter(word2 == "street") %>%
  count(book, word1, sort = TRUE)

bigrams_no_stop %>%
  filter(word1 == "sir") %>%
  count(book, word2, sort = TRUE)

# convert bigrams from two columns back to one
bigrams_united <- bigrams_no_stop %>%
  unite(bigram, word1, word2, sep = " ")

# calculate the tf-idf
bigram_tf_idf <- bigrams_united %>%
  count(book, bigram) %>%
  bind_tf_idf(bigram, book, n) %>%
  arrange(desc(tf_idf))

head(bigram_tf_idf)

# visualize results
bigram_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  group_by(book) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup() %>%
  mutate(bigram = reorder(bigram, tf_idf)) %>%
  ggplot(aes(tf_idf, bigram, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ book,  scales = "free") +
  labs(x = "tf-idf of bigram", y = NULL)

not_words <- bigrams_sep %>%
  filter(word1 == "not") %>%
  inner_join(get_sentiments("afinn") , by = c(word2 = "word")) %>%
  count(word2, value, sort = TRUE)

not_words

plot_nots <- not_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) 

ggplot(plot_nots,aes(n * value, word2, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Sentiment value * number of occurrences",
       y = "Words preceded by \"not\"")

negation_words <- c("not", "no", "never", "without")

# count affected words
negated_words <- bigrams_sep %>%
  filter(word1 %in% negation_words) %>%
  inner_join(get_sentiments("afinn"), by = c(word2 = "word")) %>%
  count(word1, word2, value, sort = TRUE)

head(negated_words)

# plot affected words
negated_words %>%
  mutate(contribution = n * value,
         word2 = reorder(paste(word2, word1, sep = "__"), contribution)) %>%
  group_by(word1) %>%
  slice_max(abs(contribution), n = 12, with_ties = FALSE) %>%
  ggplot(aes(word2, contribution, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ word1, scales = "free") +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  xlab("Words preceded by negation term") +
  ylab("Sentiment value * # of occurrences") +
  coord_flip()

pers_section_words <- austen_books() %>%
  filter(book == "Persuasion") %>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)

head(pers_section_words)

# install.packages("widyr")
install.packages("widyr")
library(widyr)
# count words co-occuring within sections
word_pairs <- pers_section_words %>%
  pairwise_count(word, section, sort = TRUE)
word_pairs

word_pairs %>%
  filter(item1 == "captain")

# we need to filter for at least relatively common words first
word_cors <- pers_section_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort = TRUE)
word_cors

word_cors %>%
  filter(item1 == "daughter")

word_cors %>%
  filter(item1 %in% c("anne", "daughter", "married", "friend")) %>%
  group_by(item1) %>%
  slice_max(correlation, n = 6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()
