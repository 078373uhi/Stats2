# Load required packages
library(lme4)
library(sjPlot)
library(tidyverse)
library(glmmTMB)
library(ggplot2)
library(patchwork)

# read in and check data
books <- read.csv("~/Stats2/CS2.csv")

view(books)
summary(books)

# plots to investigate data
author <- ggplot(books, aes(x = freqchal, fill = removed)) +
  geom_bar() +
  scale_x_discrete(breaks = c(0, 1), labels = c(0, 1)) + # LABELS REFUSE TO SHOW 
  # HERE NO MATTER WHAT I TRY! NOT REMOVED (0) IS ON THE LEFT AND REMOVED (1) IS
  # ON THE RIGHT
  labs(y= "Number of books", x = "Removed") +
  ggtitle("Frequently challenged authors (more than 10 challenges)")

author

obama <- ggplot(books, aes(x = obama, fill = removed)) +
  geom_bar() +
  scale_x_discrete(breaks = c(0, 1), labels = c(0, 1)) + # LABELS REFUSE TO SHOW 
  # HERE NO MATTER WHAT I TRY! NOT REMOVED (0) IS ON THE LEFT AND REMOVED (1) IS
  # ON THE RIGHT
  labs(y= "Number of books", x = "Removed") +
  ggtitle("Obama was President")

obama

sex <- ggplot(books, aes(x = sexexp, fill = removed)) +
  geom_bar() +
  scale_x_discrete(breaks = c(0, 1), labels = c(0, 1)) + # LABELS REFUSE TO SHOW 
  # HERE NO MATTER WHAT I TRY! NOT REMOVED (0) IS ON THE LEFT AND REMOVED (1) IS
  # ON THE RIGHT
  labs(y= "Number of books", x = "Removed") +
  ggtitle("Sexually explicit material")

sex

family <- ggplot(books, aes(x = antifamily, fill = removed)) +
  geom_bar() +
  scale_x_discrete(breaks = c(0, 1), labels = c(0, 1)) + # LABELS REFUSE TO SHOW 
  # HERE NO MATTER WHAT I TRY! NOT REMOVED (0) IS ON THE LEFT AND REMOVED (1) IS
  # ON THE RIGHT
  labs(y= "Number of books", x = "Removed") +
  ggtitle("Anti family material")

family

occult <- ggplot(books, aes(x = occult, fill = removed)) +
  geom_bar() +
  scale_x_discrete(breaks = c(0, 1), labels = c(0, 1)) + # LABELS REFUSE TO SHOW 
  # HERE NO MATTER WHAT I TRY! NOT REMOVED (0) IS ON THE LEFT AND REMOVED (1) IS
  # ON THE RIGHT
  labs(y= "Number of books", x = "Removed") +
  ggtitle("Occult material")

occult

language <- ggplot(books, aes(x = language, fill = removed)) +
  geom_bar() +
  scale_x_discrete(breaks = c(0, 1), labels = c(0, 1)) + # LABELS REFUSE TO SHOW 
  # HERE NO MATTER WHAT I TRY! NOT REMOVED (0) IS ON THE LEFT AND REMOVED (1) IS
  # ON THE RIGHT
  labs(y= "Number of books", x = "Removed") +
  ggtitle("Inappropriate language")

language

homosexuality <- ggplot(books, aes(x = homosexuality, fill = removed)) +
  geom_bar() +
  scale_x_discrete(breaks = c(0, 1), labels = c(0, 1)) + # LABELS REFUSE TO SHOW 
  # HERE NO MATTER WHAT I TRY! NOT REMOVED (0) IS ON THE LEFT AND REMOVED (1) IS
  # ON THE RIGHT
  labs(y= "Number of books", x = "Removed") +
  ggtitle("Material about homosexuality")

homosexuality

violence <- ggplot(books, aes(x = violence, fill = removed)) +
  geom_bar() +
  scale_x_discrete(breaks = c(0, 1), labels = c(0, 1)) + # LABELS REFUSE TO SHOW 
  # HERE NO MATTER WHAT I TRY! NOT REMOVED (0) IS ON THE LEFT AND REMOVED (1) IS
  # ON THE RIGHT
  labs(y= "Number of books", x = "Removed") +
  ggtitle("Violent material")

violence

author + obama + sex + family + occult + language + homosexuality + violence
