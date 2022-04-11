# Load required packages
library(lme4)
library(sjPlot)
library(tidyverse)
library(glmmTMB)

# read in and check data
books <- read.csv("~/Stats2/CS2.csv")

view(books)
summary(books)

# plots to investigate data
occult <- ggplot(books, aes(x = occult, fill = removed)) +
  geom_bar() +
  scale_x_discrete(breaks = c(0, 1), labels = c(0, 1)) + # LABELS REFUSE TO SHOW 
  # HERE NO MATTER WHAT I TRY! NOT REMOVED (0) IS ON THE LEFT AND REMOVED (1) IS
  # ON THE RIGHT
  labs(y= "Number of books", x = "Removed") +
  ggtitle("Number of books challenged on occult material")

occult

