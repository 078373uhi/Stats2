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
  scale_x_discrete("Removed", labels = c("No", "Yes")) +
  labs(y= "Number of books", x = "Removed") +
  ggtitle("Number of books challenged on occult material")

occult
