library(tidyverse)
library(tidytext)

# create the document text
txt <- c("So this is my FIRST document!", "Another, but not a very long one... Or is it?", "Here's the third", "This is also a document, but one I'm proud of.")
# create the document list
doc <- c(1,2,3,4)
# put it all together, and have a look at the result
txt_data <- tibble(txt, doc)
txt_data

unnest_tokens(tbl = txt_data, output = "tok", input = txt)

# install.packages("gutenbergr")
install.packages("gutenbergr")
library(gutenbergr)

gutenberg_metadata %>%
  filter(author == "Poe, Edgar Allan")

raven <- gutenberg_download(1065)$text 

raven

child <- gutenberg_download(40448)$text
head(child, 50)

child <- read_lines("http://www.gutenberg.org/cache/epub/40448/pg40448.txt", skip = 68)
head(child, 10)

# make Raven into a dataframe
library(dplyr)
raven_txt <- tibble(text=raven)
head(raven_txt)

# use unnest_tokens()
tidy_raven <- raven_txt %>%
  unnest_tokens(word, text)

tidy_raven

# count each word
tidy_raven %>%
  count(word, sort=TRUE)

#remove stop words
tidy_raven <-  tidy_raven %>%
  anti_join(stop_words) 

tidy_raven %>%
  count(word, sort=TRUE)

# visualise data
#plot
library(ggplot2)

tidy_raven %>%
  count(word, sort = TRUE) %>%
  filter(n > 4) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill=word)) +
  geom_col() +
  labs(title='Word frequency for The Raven',
       y = NULL, x= "word count") +
  theme(legend.position="none")

#wordcloud
install.packages("wordcloud2")
library(wordcloud2)
library(viridisLite) # for colours
colour_pal <- turbo(n=100)


wc_data <- tidy_raven %>%
  count(word)  %>%
  filter(n > 1)   # only include words that appear more than once.
wordcloud2(wc_data, shape='circle', size=0.5, color=colour_pal)

# use multiple sources
gutenberg_metadata %>% filter(gutenberg_id %in% c(209, 42, 345, 175))
gothic <- gutenberg_download(c(209, 42, 345, 175), meta_fields = "author")

gothic_words <- gothic %>%
  unnest_tokens(word, text) %>%
  count(author, word, sort = TRUE) %>% print()

# remove stopwords
tidy_gothic <- gothic_words %>%
  anti_join(stop_words, by="word") 

tidy_gothic

# Tf-idf weighting
gothic_words %>%
  bind_tf_idf(word, author, n) %>%
  arrange(desc(tf_idf))

#visualise results
plot_gothic <- gothic_words %>%
  bind_tf_idf(word, author, n) %>%
  group_by(author) %>%
  top_n(10, tf_idf) %>%
  ungroup()
ggplot(plot_gothic, aes(reorder(word, tf_idf), tf_idf, fill = author)) +
  geom_col(show.legend = FALSE) + labs(x = NULL, y = "tf-idf") +
  facet_wrap( ~ author, ncol = 4, scales = "free") + coord_flip()

