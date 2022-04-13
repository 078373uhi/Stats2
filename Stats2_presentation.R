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

# Aim to determine what combination of books features are most likely to result 
# in a successful challenge and the removal of the book.

# tables and plots to investigate data
table(books$removed) # look at how many challenged books were removed or not: 190 
# removed and 610 not removed then view as a plot.

allbooks <- books %>%
  mutate(removed = as.factor(removed)) %>%
  ggplot(aes(x = removed)) +
  geom_bar() + #create bar plot
  labs(y= "Number of books", x = "Removed") + # give titles
  ggtitle("Result of book challenges - all books") +
  scale_x_discrete(labels=c("0" = "No", "1" = "Yes")) # change removed labels to 
# yes/no

allbooks

# compare challenged books by state and whether removed or not
state <- books %>%
  mutate(removed = as.factor(removed)) %>%
  ggplot(aes(x = state, fill = removed)) +
  geom_bar() + #create bar plot
  theme(axis.text.x = element_text(angle=270)) + # better present state labels
  labs(y= "Number of books", x = "State") + # give titles
  ggtitle("Books challenged by state") +
  scale_fill_discrete(name = "Removed", labels = c("No", "Yes")) # change 
# removed labels to yes/no

state

stateR <- books %>%
  mutate(removed = as.factor(removed)) %>%
  filter(removed == 1) %>%
  ggplot(aes(x = state, fill = removed)) +
  geom_bar() + #create bar plot
  theme(axis.text.x = element_text(angle=270)) + # better present state labels
  labs(y= "Number of books", x = "State") + # give titles
  ggtitle("Books removed by state") +
  theme(legend.position = "none") +
  scale_fill_discrete(name = "Removed", labels = c("Yes")) # change 
# removed labels to yes/no

stateR

# Pennsylvania has the highest number of books challenged followed by Oregon and 
# Colorado.  The lowest number of book challenges were in Mississippi, Wyoming 
# and West Virginia.  However, the number of books actually removed was highest
# in Virginia (16 books) and California (12 books) and lowest in Alaska, Conneticut, 
# Delaware, Mississippi, New Mexico, Rhode Island and South Dakota (1 book each).

# plot challenged books by political leaning
politics <- books %>%
  mutate(removed = as.factor(removed)) %>%
  ggplot(aes(x = removed, y = pvi2)) +
  geom_boxplot() + #create box plot
  scale_x_discrete(labels=c("0" = "No", "1" = "Yes")) + # change removed labels 
  # to yes/no
  ggtitle("Books challenged by state score on the Political Value Index") + 
  # add title and caption
  labs(caption = "Positive indicates a Democratic leaning, negative indicates a Republican leaning, and 0 is neutral")

politics
# This shows that more books were removed in Republican leaning states than Democrats
# and similarly more books were retained in Democrat states than Republican.

# plot challenged books by % of High School graduates
HSgraduates <- books %>%
  mutate(removed = as.factor(removed)) %>%
  ggplot(aes(x = removed, y = cperhs)) +
  geom_boxplot() + #create box plot
  scale_x_discrete(labels=c("0" = "No", "1" = "Yes")) + # change removed labels 
  # to yes/no
  ggtitle("Books challenged by High School graduation %") +  # add title 
  ylab("Percentage of high school graduates in a state (grand mean centered)")

HSgraduates

Cgraduates <- books %>%
  mutate(removed = as.factor(removed)) %>%
  ggplot(aes(x = removed, y = cperba)) +
  geom_boxplot() + #create box plot
  scale_x_discrete(labels=c("0" = "No", "1" = "Yes")) +
  ggtitle("Books challenged by College graduation %") +  # change removed labels 
  # to yes/no
  ylab("Percentage of college graduates in a state (grand mean centered)") # add title

Cgraduates
# I have no real idea what this is telling us.  How can there be negative of  
# graduates?

# plot challenged books by median state income
income <- books %>%
  mutate(removed = as.factor(removed)) %>%
  ggplot(aes(x = removed, y = cmedin)) +
  geom_boxplot() + #create box plot
  scale_x_discrete(labels=c("0" = "No", "1" = "Yes")) + # change removed labels 
  # to yes/no
  ggtitle("Books challenged by median state income") +  # add title and caption
  ylab("median state income (grand median centered)")

income
# This does not appear to show any clear pattern except that very slightly more
# challenges were rejected in higher income areas.

# plot challenged books by date (measured from January 1 2000)
date <- books %>%
  mutate(removed = as.factor(removed)) %>%
  ggplot(aes(x = removed, y = days2000)) +
  geom_boxplot() + #create box plot
  scale_x_discrete(labels=c("0" = "No", "1" = "Yes")) + # change removed labels 
  # to yes/no
  ggtitle("Books challenged by date") + 
  labs(caption = "Date challenge was made, measured by number of days after January 1, 2000") +
  ylab("days after January 1, 2000") # add title and caption

date
# This shows that more rejections happened in more recent times (further away from
# 1.1.2000) while successful challenges occurred more in the past.

# plot by theme or material in book
violence <- books %>%
  mutate(removed = as.factor(removed)) %>%
  mutate(violence = as.factor(violence)) %>%
  filter(violence == 1) %>%
  ggplot(aes(x = removed)) +
  geom_bar() + #create bar plot
  labs(y= "Number of books", x = "Removed") +
  ggtitle("Violent content") + # add title and labels
  scale_x_discrete(labels=c("0" = "No", "1" = "Yes")) + # change removed labels 
  # to yes/no
  ylim(0, 250) # set common limits to compare plots

homosexuality <- books %>%
  mutate(removed = as.factor(removed)) %>%
  mutate(homosexuality = as.factor(homosexuality)) %>%
  filter(homosexuality == 1) %>%
  ggplot(aes(x = removed)) +
  geom_bar() + #create bar plot
  labs(y= "Number of books", x = "Removed") +
  ggtitle("Homosexual material") + # add title and labels
  scale_x_discrete(labels=c("0" = "No", "1" = "Yes")) + # change removed labels 
  # to yes/no
  ylim(0, 250) # set common limits to compare plots

language <- books %>%
  mutate(removed = as.factor(removed)) %>%
  mutate(language = as.factor(language)) %>%
  filter(language == 1) %>%
  ggplot(aes(x = removed)) +
  geom_bar() + #create bar plot
  labs(y= "Number of books", x = "Removed") +
  ggtitle("Inappropriate language") + # add title and labels
  scale_x_discrete(labels=c("0" = "No", "1" = "Yes")) + # change removed labels 
  # to yes/no
  ylim(0, 250) # set common limits to compare plots

occult <- books %>%
  mutate(removed = as.factor(removed)) %>%
  mutate(occult = as.factor(occult)) %>%
  filter(occult == 1) %>%
  ggplot(aes(x = removed)) +
  geom_bar() + #create bar plot
  labs(y= "Number of books", x = "Removed") +
  ggtitle("Occult material") + # add title and labels
  scale_x_discrete(labels=c("0" = "No", "1" = "Yes")) + # change removed labels 
  # to yes/no
  ylim(0, 250) # set common limits to compare plots

family <- books %>%
  mutate(removed = as.factor(removed)) %>%
  mutate(antifamily = as.factor(antifamily)) %>%
  filter(antifamily == 1) %>%
  ggplot(aes(x = removed)) +
  geom_bar() + #create bar plot
  labs(y= "Number of books", x = "Removed") +
  ggtitle("Anti-family material") + # add title and labels
  scale_x_discrete(labels=c("0" = "No", "1" = "Yes")) + # change removed labels 
  # to yes/no
  ylim(0, 250) # set common limits to compare plots

sex <- books %>%
  mutate(removed = as.factor(removed)) %>%
  mutate(sexexp = as.factor(sexexp)) %>%
  filter(sexexp == 1) %>%
  ggplot(aes(x = removed)) +
  geom_bar() + #create bar plot
  labs(y= "Number of books", x = "Removed") +
  ggtitle("Sexually explicit material") + # add title and labels 
  scale_x_discrete(labels=c("0" = "No", "1" = "Yes")) + # change removed labels 
  # to yes/no
  ylim(0, 250) # set common limits to compare plots

obama <- books %>%
  mutate(removed = as.factor(removed)) %>%
  mutate(obama = as.factor(obama)) %>%
  filter(obama == 1) %>%
  ggplot(aes(x = removed)) +
  geom_bar() + #create bar plot
  labs(y= "Number of books", x = "Removed") +
  ggtitle("Obama was President") + # add title and labels
  scale_x_discrete(labels=c("0" = "No", "1" = "Yes")) + # change removed labels 
  # to yes/no
  ylim(0, 250) # set common limits to compare plots

author <- books %>%
  mutate(removed = as.factor(removed)) %>%
  mutate(freqchal = as.factor(freqchal)) %>%
  filter(freqchal == 1) %>%
  ggplot(aes(x = removed)) +
  geom_bar() + #create bar plot
  labs(y= "Number of books", x = "Removed") + # add title and labels
  ggtitle("Frequently challenged authors (more than 10 challenges)") +
  scale_x_discrete(labels=c("0" = "No", "1" = "Yes")) + # change removed labels 
  # to yes/no
  ylim(0, 250) # set common limits to compare plots

# compare plots by theme
author + obama + sex + family + occult + language + homosexuality + violence
# This shows that the theme with the most removals was sexually explicit material
# followed by inappropriate language.  The least removals were from Anti-family and
# occult themes.

# Models
## My general approach with these tasks is to copy an example from the coursework 
## and apply it to my data however this data is way more complicated than anything
## in the coursework so I have no real clue what I am doing here.

## I know I should have training and test data but that is beyond me.  I feel like
## it doesn't really matter because this is such a mess anyway.

## Models using week 4 notes
# standardise predictors ## no idea if I need to do this? Or if I should apply 
## this to all the predictors.  I decided to not apply it to binary predictors.
books$pvi2adj <- scale(books$pvi2)
books$cperhsadj <- scale(books$cperhs)
books$cmedinadj <- scale(books$cmedin)
books$cperbaadj <- scale(books$cperba)

# Model 1 takes into account random variations by state ## I think
model1 <- glmer(removed ~ freqchal + obama + sexexp +antifamily + occult + 
                  language + homosexuality + violence + (1|state), data = books, 
                family = binomial)

summary(model1) # This appears to show that frequency challenged, Obama and 
# inappropriate language may be significant.

## I also tried doing the same but without the variation by state.  I think
## it gives a similar result
model0a <- glm(removed ~ freqchal + obama + sexexp +antifamily + occult + 
                 language + homosexuality + violence, data = books, 
               family = binomial)

summary(model0a)

# Model 2 takes into account random variations by politics ## I think
model2 <- glmer(removed ~ freqchal + obama + sexexp +antifamily + occult + 
                  language + homosexuality + violence + (1|pvi2adj), data = books, 
                family = binomial)

summary(model2) # Same as model 1 - frequency challenged, Obama and 
# inappropriate language may be significant.

# Model 3 takes into account random variations by median income ## I think
model3 <- glmer(removed ~ freqchal + obama + sexexp +antifamily + occult + 
                  language + homosexuality + violence + (1|cmedinadj), data = books, 
                family = binomial)

summary(model3) # Same as model 1 and 2 - frequency challenged, Obama and 
# inappropriate language may be significant. ## Are these really all telling 
## me the same thing? 

# Model 4 takes into account random variations by college graduation ## I think
model4 <- glmer(removed ~ freqchal + obama + sexexp +antifamily + occult + 
                  language + homosexuality + violence + (1|cperbaadj), data = books, 
                family = binomial)

summary(model4) #As previous

# Model 5 takes into account random variations by HS graduation ## I think
model5 <- glmer(removed ~ freqchal + obama + sexexp +antifamily + occult + 
                  language + homosexuality + violence + (1|cperhsadj), data = books, 
                family = binomial)

summary(model5) #As previous

## Models using week 2 notes
m_glm = glm(removed ~ state + freqchal + pvi2 + obama + cperhs + sexexp + 
              antifamily + cmedin + cperba + days2000 + occult + language + 
              homosexuality + violence, 
             data=books, family=binomial)

tab_model(m_glm)

m_glmstate = glm(removed ~ state, data = books, family = binomial)
tab_model(m_glmstate)
plot_model(m_glmstate, type='pred', grid = T)

# model 1
m_glm1 = glm(removed ~ freqchal + obama + sexexp + antifamily + occult + 
               language + homosexuality + violence, 
            data=books, family=binomial)
tab_model(m_glm1)
plot_model(m_glm1, type='pred', grid = T)
# This suggests that books that include sexually explicit, occult, violent or 
# homosexual material or inappropriate language have a higher chance of 
# being removed, though the highest odds ratio comes for books with authors who
# are frequently challenged.

m_glm2 = glm(removed ~ freqchal + sexexp + occult + language + homosexuality + 
               violence, 
             data=books, family=binomial) # remove obama and anti-family
tab_model(m_glm2)
plot_model(m_glm2, type='pred', grid = T)
# All variables in the model now increase the chances of a book being removed 
# if challenged.

m_glm3 = glm(removed ~ freqchal + sexexp + occult + language + homosexuality,
             data=books, family=binomial) #removed violence

m_glm4 = glm(removed ~ freqchal + sexexp + occult + language + violence, 
             data=books, family=binomial) # remove homosexuality

m_glm5 = glm(removed ~ freqchal + sexexp + occult + homosexuality + violence, 
             data=books, family=binomial) #remove language

m_glm6 = glm(removed ~ freqchal + sexexp + language + homosexuality + violence, 
             data=books, family=binomial) # remove occult

m_glm7 = glm(removed ~ freqchal + occult + language + homosexuality + violence, 
             data=books, family=binomial) # remove sex

m_glm8 = glm(removed ~ sexexp + occult + language + homosexuality + violence, 
             data=books, family=binomial) # remove frequently challenged

# compare models
tab_model(m_glm2, m_glm3, m_glm4, m_glm5, m_glm6, m_glm7, m_glm8)

anova(m_glm2, m_glm3, m_glm4, m_glm5, m_glm6, m_glm7, m_glm8, test = "Chisq")
# Looking at the residual deviance shows that mdel 2 and model 6 have the lowest 
# residual deviance which shows the better fit.
