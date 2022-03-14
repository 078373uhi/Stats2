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
library(reshape2)

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

# count number of words spoken by each president
count(tidy_pres, source)
# Biden spoke the most with 7,769 words though Clinton (7,421 words) and Obama
# (7,238 words) were not far behind him.  Trump was next with 5,819 words while
# Bush said the least (3,820 words).

# Count unique words spoken by each President.
tidy_pres %>%
  filter(source == "bid") %>%
  count(word, sort=TRUE)
  
tidy_pres %>%
  filter(source == "tru") %>%
  count(word, sort=TRUE)

tidy_pres %>%
  filter(source == "oba") %>%
  count(word, sort=TRUE)

tidy_pres %>%
  filter(source == "bus") %>%
  count(word, sort=TRUE)

tidy_pres %>%
  filter(source == "cli") %>%
  count(word, sort=TRUE)
# Biden spoke 1,853 unique words.  This is the most of all the Presidents and may  
# be expected as he spoke the most of all the speeches. Bush said 1,238 different  
# words.  This was the least and also correlates with him being the President 
# that spoke the least. Obama said 1,712 unique words, Trump said 1,662 different 
# words and Clinton said 1,631 unique words.

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
# not add much information so I am going to remove these along with the other 
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
  labs(title='Word frequency for the five speeches with common/stop words removed',
       y = NULL, x= "word count") +
  theme(legend.position="none")

# plots by President of words said 10 or more times with common/stop words removed
# Biden plot
bid_plot2 <- tidy_pres2 %>%
  filter(source == "bid") %>%
  count(word, sort = TRUE) %>%
  filter(n > 10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = word)) +
  geom_col() +
  labs(title='Word frequency for the Biden speech with common/stop words removed',
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
  labs(title='Word frequency for the Trump speech with common/stop words removed',
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
  labs(title='Word frequency for the Obama speech with common/stop words removed',
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
  labs(title='Word frequency for the Bush speech with common/stop words removed',
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
  labs(title='Word frequency for the Clinton speech with common/stop words removed',
       y = NULL, x= "word count") +
  theme(legend.position="none")

bid_plot2 + tru_plot2 + oba_plot2 + bus_plot2 + cli_plot2

# plot a wordcloud showing the top 100 words from all the speeches together
top_100 <- tidy_pres2 %>% 
  count(word, sort = TRUE) %>%
  top_n(100)

wordcloud2(data=top_100)

# This shows quite an interesting result with some clear themes to the speeches.
# The Clinton speech shows themes of health, welfare and family while Bush is 
# focused on security, terror, weapons and war.  Obama discusses jobs, business
# and the economy while Trump does not seem to have a clear theme.  Biden covers
# many topics though jobs, costs and families feature highly.

# •	Perform sentiment analysis, making comparisons between the presidents.

# add sentiments to previous text corpus
pos_neg_sent <- tidy_pres2 %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(source, word, sentiment, sort = TRUE) %>%
  ungroup()

head(pos_neg_sent)

pos_neg_sent %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(sentiment ~ source, nrow = 2, scales = "free_y") +
  labs(x = "Contribution to Sentiment",
       y = NULL) +
  theme(strip.text.x = element_text(margin = margin(2, 0, 2, 0)))

# This positive/negative sentiment analysis shows results that tie in with the  
# above word analysis. Obama highlights the recession, support and recovery while  
# Bush brings up terror and freedom.  Clinton has crime, reform and support while  
# Trump and Biden have less obvious themes.

# Variation in overall positivity by President
# prepare text for analysis
tidy_pres3 <- tidy_pres2 %>%
    mutate(
    linenumber = row_number())

head(tidy_pres3)

# break text into chunks and measure positivity of each chunk
pres3_sentiment <- tidy_pres3 %>%
  inner_join(get_sentiments("bing"), by="word") %>%
  count(source, index = linenumber %/% 80, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)
# index=linenumber %/% 80, is splitting our text up into chunks of 80 lines each.

head(pres3_sentiment)

# plot positivity by President
ggplot(pres3_sentiment, aes(index, sentiment, fill = source)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~source, scales = "free_x") +
  labs(x = "Positivity by President",
       y = "Scale of positive/negative") 

# The pattern of positivity for Bush is quite clear.  The first half of his speech 
# was negative and then it turned positive.  Trump's speech can be seen to start 
# positively, then turn negative before finishing positively again.  The others
# are less obvious though Biden can be seen to be generally positive with Clinton
# and Obama being more mixed.  This can be seen in the wordclouds below.

# plot a wordcloud showing the top 100 words from all the speeches together with 
# positive/negative analysis
top_100_sent <- tidy_pres2 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "green"),
                   max.words = 100)

# Plot a wordcloud showing the top 100 words from Biden with positive/negative 
# analysis
bid_100_sent <- tidy_pres2 %>%
  filter(source == "bid") %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "green"),
                   max.words = 100)


# Plot a wordcloud showing the top 100 words from Trump with positive/negative 
# analysis
tru_100_sent <- tidy_pres2 %>%
  filter(source == "tru") %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "green"),
                   max.words = 100)

# Plot a wordcloud showing the top 100 words from Obama with positive/negative 
# analysis
oba_100_sent <- tidy_pres2 %>%
  filter(source == "oba") %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "green"),
                   max.words = 100)

# Plot a wordcloud showing the top 100 words from Bush with positive/negative 
# analysis
bus_100_sent <- tidy_pres2 %>%
  filter(source == "bus") %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "green"),
                   max.words = 100)

# Plot a wordcloud showing the top 100 words from Clinton with positive/negative 
# analysis
cli_100_sent <- tidy_pres2 %>%
  filter(source == "cli") %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "green"),
                   max.words = 100)

# Bigrams
pres_bigrams <- pres %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)
pres_bigrams

# format to have words in separate columns
bigrams_sep <- pres_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# remove stop words
bigrams_no_stop <- bigrams_sep %>%
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word)

# count bigrams
bigram_counts <- bigrams_no_stop %>%
  count(source, word1, word2, sort=TRUE)

head(bigram_counts)

