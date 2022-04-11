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
allbooks <- books %>%
  mutate(removed = as.factor(removed)) %>%
  ggplot(aes(x = removed)) +
  geom_bar() +
  labs(y= "Number of books", x = "Removed") +
  ggtitle("Result of book challenges - all books") +
  scale_x_discrete(labels=c("0" = "No", "1" = "Yes"))

allbooks

state <- books %>%
  mutate(removed = as.factor(removed)) %>%
  ggplot(aes(x = state, fill = removed)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle=270)) +
  labs(y= "Number of books", x = "State") +
  ggtitle("Books challenged by state") +
  scale_fill_discrete(name = "Removed", labels = c("No", "Yes"))

state

violence <- books %>%
  mutate(removed = as.factor(removed)) %>%
  mutate(violence = as.factor(violence)) %>%
  filter(violence == 1) %>%
  ggplot(aes(x = removed)) +
  geom_bar() +
  labs(y= "Number of books", x = "Removed") +
  ggtitle("Violent content") +
  scale_x_discrete(labels=c("0" = "No", "1" = "Yes")) +
  ylim(0, 250)

violence

homosexuality <- books %>%
  mutate(removed = as.factor(removed)) %>%
  mutate(homosexuality = as.factor(homosexuality)) %>%
  filter(homosexuality == 1) %>%
  ggplot(aes(x = removed)) +
  geom_bar() +
  labs(y= "Number of books", x = "Removed") +
  ggtitle("Homosexual material") +
  scale_x_discrete(labels=c("0" = "No", "1" = "Yes")) +
  ylim(0, 250)

homosexuality

language <- books %>%
  mutate(removed = as.factor(removed)) %>%
  mutate(language = as.factor(language)) %>%
  filter(language == 1) %>%
  ggplot(aes(x = removed)) +
  geom_bar() +
  labs(y= "Number of books", x = "Removed") +
  ggtitle("Inappropriate language") +
  scale_x_discrete(labels=c("0" = "No", "1" = "Yes")) +
  ylim(0, 250)

language

occult <- books %>%
  mutate(removed = as.factor(removed)) %>%
  mutate(occult = as.factor(occult)) %>%
  filter(occult == 1) %>%
  ggplot(aes(x = removed)) +
  geom_bar() +
  labs(y= "Number of books", x = "Removed") +
  ggtitle("Occult material") +
  scale_x_discrete(labels=c("0" = "No", "1" = "Yes")) +
  ylim(0, 250)

occult

family <- books %>%
  mutate(removed = as.factor(removed)) %>%
  mutate(antifamily = as.factor(antifamily)) %>%
  filter(antifamily == 1) %>%
  ggplot(aes(x = removed)) +
  geom_bar() +
  labs(y= "Number of books", x = "Removed") +
  ggtitle("Anti-family material") +
  scale_x_discrete(labels=c("0" = "No", "1" = "Yes")) +
  ylim(0, 250)

family

sex <- books %>%
  mutate(removed = as.factor(removed)) %>%
  mutate(sexexp = as.factor(sexexp)) %>%
  filter(sexexp == 1) %>%
  ggplot(aes(x = removed)) +
  geom_bar() +
  labs(y= "Number of books", x = "Removed") +
  ggtitle("Sexually explicit material") +
  scale_x_discrete(labels=c("0" = "No", "1" = "Yes")) +
  ylim(0, 250)

sex

obama <- books %>%
  mutate(removed = as.factor(removed)) %>%
  mutate(obama = as.factor(obama)) %>%
  filter(obama == 1) %>%
  ggplot(aes(x = removed)) +
  geom_bar() +
  labs(y= "Number of books", x = "Removed") +
  ggtitle("Obama was President") +
  scale_x_discrete(labels=c("0" = "No", "1" = "Yes")) +
  ylim(0, 250)

obama

author <- books %>%
  mutate(removed = as.factor(removed)) %>%
  mutate(freqchal = as.factor(freqchal)) %>%
  filter(freqchal == 1) %>%
  ggplot(aes(x = removed)) +
  geom_bar() +
  labs(y= "Number of books", x = "Removed") +
  ggtitle("Frequently challenged authors (more than 10 challenges)") +
  scale_x_discrete(labels=c("0" = "No", "1" = "Yes")) +
  ylim(0, 250)

author

author + obama + sex + family + occult + language + homosexuality + violence
