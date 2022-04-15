library(readr)
library(tidyverse)
library(usmap)
library(lme4)
library(GGally)
library(sjPlot)


books <- read.csv("~/Stats2/CS2.csv")

glimpse(books)

# what combination of book features are most likely to result in a successful
# challenge?

books %>%
  count(removed == 1) 
# Out of 800 books 190 were removed (successfully challenged)

ggcorr(books)
# We can see there is a strong correlation between Obama being president and the
# days after 2000 and also between income and college graduates which makes sense.


# STATE
# Plot by state
books %>%
  filter(removed == 1) %>%
  group_by(state) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = reorder(state,(-count)), y = count)) +
  geom_col()

books %>%
 ggplot(aes(x = state, fill = as.factor(removed))) +
 geom_bar()


# POLITICAL LEANING
# add in a 'leaning' column
books <- books %>% 
  mutate(leaning = ifelse(pvi2 == 0, "neutral", ifelse(pvi2 < 0, "republican", 
                                                      "democratic")))
# plot by leaning
books %>%
  filter(removed == 1) %>%
  ggplot(aes(x = leaning)) +
  geom_bar()


#days since 2000
books %>%
  group_by(days2000, removed) %>%
  count(removed) %>%
  ggplot(aes(x = days2000, y = n, colour = as.factor(removed), group = removed)) +
  geom_line() +
  facet_wrap(vars(removed), ncol = 1)

# Plot challenges on a map to see any obvious trends North/South etc
states <- books %>%
  count(state)

plot_usmap(data = states,values = "n", labels = TRUE) + 
  labs(title = "Book challenges by State", fill = "No of Challenges") +
  theme(legend.position = "right")


plot_usmap(data = books,values = "leaning", labels = TRUE) + 
  labs(title = "Book challenges by State", fill = "No of Challenges") +
  theme(legend.position = "right")

#********** need to finish visualisations**************

# Several of the variables are a proxy for state i.e. state can be directly 
# transformed into some of the variables; income, high school grades, political 
# leaning etc and vice-versa

# The data has a hierarchical structure as the challenges are grouped within 
# different states.  Books being challenged in the same state are not 
# independent of each other because they will be subject to the same level/type of 
# scrutiny.  Therefore it seems like a model with state as a random effect 
# might be best suited.


# HYPOTHESES

# H0 - There is no significant difference in the probability of a book being banned 
# following a challenge in different states after accounting for other book-level
# variables and a generalised linear model will be the best fit for the data.

# H1 - There is a significant difference in the probability of a book being banned 
# following a challenge in different states after accounting for other book-level 
# variables and a generalised linear mixed model will be the best fit for the data.

# SPLIT DATA INTO TEST AND TRAIN

# rescale days & drop na
books <- books %>%
  mutate(days2000 = days2000/1000) %>%
  drop_na()


set.seed(10) # set seed so reproducible
# choose the sample 80%
books_ind <- sample(seq_len(nrow(books)), size = floor(0.8 * nrow(books)))

train <- books[books_ind, ]
test <- books[-books_ind, ]

# ASSESSING THE NEED FOR A MULTILEVEL MODEL

# RANDOM INTERCEPTS
# Fitting a baseline model with only the intercept
InterceptOnly <- glm(removed ~ 1, data = train, family = "binomial")
# Fitting a model where intercepts vary over the states
RandomInterceptOnly <- glmer(removed ~ 1 + (1|state), data = train, family = "binomial")

summary(InterceptOnly)
summary(RandomInterceptOnly)
# Comparing the AIC and BIC of the models we can see that allowing the intercepts to 
# vary improves the fit of the model (AIC reduced from 697.13 to 651.51)

anova(RandomInterceptOnly, InterceptOnly)
# Here we can see that the BIC and LogLikelihoods are also reduced in the 
# multilevel model

ranef(RandomInterceptOnly)
# by extracting the random effects we can see that they do vary by state


# The evidence above suggests that there is a variance between the different 
# states, therefore we can reject the null hypothesis that there is no difference
# between the states and a GLMM will be the best fit for the data

# RANDOM SLOPE

# This model allows for the effect of time to vary across the states
RandomInterceptSlope <- glmer(removed ~ 1 + (days2000|state), data = train, 
                              family = "binomial")

# Here we can see that there is a significant difference between the models
# (p<0.05) and adding random slopes has improved the model; AIC reduced from
# 652 to 637
anova(RandomInterceptOnly, RandomInterceptSlope)


# ADDING IN FIXED EFFECTS

# We will continue to use the AIC score as our decision rule


# Since several of the variables were directly derived from the 'state' variable
# we will begin with a model with all variables other than those related to state
m1 <-  glmer(removed ~  (days2000|state) + obama + freqchal +  sexexp + 
                antifamily + occult + language + homosexuality + violence,
              data = train, family = "binomial")

# This model suggests that freqchal might be significant.  
summary(m1)
tab_model(m1)

# Here I am trying some combinations of the other variables with lowest P values
# (antifamily and language)
m2 <-  glmer(removed ~  (days2000|state) + freqchal, data = train, 
             family = "binomial")

m3 <-  glmer(removed ~  (days2000|state)  + freqchal + antifamily, data = train, 
             family = "binomial")

m4 <-  glmer(removed ~  (days2000|state) + freqchal + antifamily + language, data = train, 
             family = "binomial")

m5 <-  glmer(removed ~  (days2000|state) + freqchal + language, data = train, 
             family = "binomial")

# There are no significant differences between these models but AIC is lowest for
# m4 (as is BIC) suggesting it is the best fit for the training data
anova(m1, m2, m3, m4, m5, mM1)
tab_model(m1, m2, m3, m4, m5, mM1)

summary(m4)

# The chosen model (m4) has random intercepts for states and random slopes
# for the variable days2000 (date) along with fixed effects freqchal, antifamily
# and language


# TESTING THE MODEL



# RMSE of test data (model hasn't seen this data before)
p1_test <- predict(m4, newdata = test, type = "response")

error1_test <- test$removed - p1_test

RMSE_test <- sqrt(mean(error1_test^2))


# RMSE of training data
p1_train <- predict(m4, type = "response")

error1_train <- train$removed - p1_train

RMSE_train <- sqrt(mean(error1_train^2))

RMSE_test
RMSE_train
# The RMSE is higher on the training data than the test data suggesting it may be
# underfitted 


m4_test <-  glmer(removed ~  (days2000|state) + freqchal + antifamily + language, data = test, 
                  family = "binomial")

SSModelMetrics::auc(m4) 
ModelMetrics::auc(m4_test) # The AUC tells us that 82.3% of the variation in the 
# training data and 88.4% of the variation in the test (unseen) data is explained 
# by the model. ??? That seems like it would be a good thing???


# REPORTING ON THE MODEL


tab_model(m4)
# The odds ratios indicate that, when all other factors are constant, 
# a frequently challenged book is 1.99 times more likely to result in the book 
# being removed following a challenge. Books containing bad language are 1.44 
# times to be removed.  Books containing anti-family themes are actually 0.45 
# times LESS likely to be removed.  


#This is confirmed in the plot below; we can see that removed probability 
# increases with frequently challenged books, is higher in
# books with bad language but is lower in books with anti-family themes.

plot_model(m4, type = "pred", terms = c( "freqchal", "language" , "antifamily"))


